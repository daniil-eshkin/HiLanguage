{-# LANGUAGE LambdaCase #-}
module HW3.Evaluator (
    eval
) where

import HW3.Base
    ( HiFun(..), HiError(..), HiValue(..), HiAction (..), HiExpr (..), HiMonad (runAction))
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Text (Text)
import Data.Ratio ( (%), denominator, numerator )
import Control.Monad.List (join)
import Data.Text.Encoding ( encodeUtf8, decodeUtf8' )
import Codec.Compression.Zlib
    ( defaultCompressParams,
      bestCompression,
      CompressParams(compressLevel),
      compressWith,
      decompress )
import qualified Data.ByteString.Lazy as BL
import Codec.Serialise ( deserialiseOrFail, serialise )
import Data.Either (fromRight)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Data.Foldable ( Foldable(toList) )
import Data.String (fromString)
import Text.Read (readMaybe)
import Data.Time (addUTCTime, diffUTCTime)
import Data.Sequence (Seq (..))
import Data.Map (Map)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval (HiExprValue value) = return (Right value)
eval (HiExprApply f a) = let
        applyFunStrict :: HiFun -> Either HiError [HiValue] -> Either HiError HiValue
        applyFunStrict name args = let
            arity = hiArity name
            fun = hiImplStrict name 
            in
                args >>= (hiArityCheck arity) >>= fun

        applyFunLazy :: HiMonad m => HiFun -> [HiExpr] -> m (Either HiError HiValue)
        applyFunLazy name args = if hiArity name == length args
            then hiImplLazy name args
            else return $ Left HiErrorArityMismatch 
    in
        do
            efun <- eval f
            case efun of
                e@(Left _) -> return e
                Right (HiValueFunction fun) -> 
                    case hiMethod fun of
                        Strict -> do
                            args <- fmap sequence $ mapM eval a
                            return $ applyFunStrict fun args
                        Lazy -> applyFunLazy fun a
                Right (HiValueString text) -> do
                    args <- fmap sequence $ mapM eval a
                    return $ args >>= hiString text
                Right (HiValueList list) -> do
                    args <- fmap sequence $ mapM eval a
                    return $ args >>= hiListF list
                Right (HiValueBytes bytes) -> do
                    args <- fmap sequence $ mapM eval a
                    return $ args >>= hiBytes bytes
                Right (HiValueDict dict) -> do
                    args <- fmap sequence $ mapM eval a
                    return $ args >>= hiArityCheck 1 >>= hiDict dict
                Right _ -> return $ Left HiErrorInvalidFunction 
eval (HiExprRun e) = do
    res <- eval e
    let action = (res >>= (\case
            HiValueAction a -> Right a
            _ -> Left HiErrorInvalidArgument))
    case action of
        Left er -> return $ Left er
        Right a -> fmap Right $ runAction a
eval (HiExprDict a) = do
    args <- fmap sequence $ mapM (\(ek, ev) -> do
        k <- eval ek
        v <- eval ev
        case (k, v) of
            (Left e, _) -> return $ Left e
            (_, Left e) -> return $ Left e
            (Right x, Right y) -> return $ Right (x, y)) a
    return $ fmap (HiValueDict . M.fromList) args

hiArityCheck :: Int -> [HiValue] -> Either HiError [HiValue]
hiArityCheck ar args = if ar == -1 || ar == length args then Right args else Left HiErrorArityMismatch

data HiEvalMethod = Lazy | Strict

hiMethod :: HiFun -> HiEvalMethod
hiMethod HiFunAnd = Lazy
hiMethod HiFunOr = Lazy
hiMethod HiFunIf = Lazy
hiMethod _ = Strict

hiArity :: HiFun -> Int
hiArity HiFunNot = 1
hiArity HiFunLength = 1
hiArity HiFunToUpper = 1
hiArity HiFunToLower = 1
hiArity HiFunReverse = 1
hiArity HiFunTrim = 1
hiArity HiFunPackBytes = 1
hiArity HiFunUnpackBytes = 1
hiArity HiFunEncodeUtf8 = 1
hiArity HiFunDecodeUtf8 = 1
hiArity HiFunZip = 1
hiArity HiFunUnzip = 1
hiArity HiFunSerialise = 1
hiArity HiFunDeserialise = 1
hiArity HiFunRead = 1
hiArity HiFunMkDir = 1
hiArity HiFunChDir = 1
hiArity HiFunParseTime = 1
hiArity HiFunEcho = 1
hiArity HiFunIf = 3
hiArity HiFunList = -1
hiArity HiFunCount = 1
hiArity HiFunKeys = 1
hiArity HiFunValues = 1
hiArity HiFunInvert = 1
hiArity _ = 2

hiImplLazy :: HiMonad m => HiFun -> [HiExpr] -> m (Either HiError HiValue)
hiImplLazy HiFunAnd = hiAndLazy
hiImplLazy HiFunOr = hiOrLazy
hiImplLazy HiFunIf = hiIfLazy
hiImplLazy _ = undefined 

hiAndLazy :: HiMonad m => [HiExpr] -> m (Either HiError HiValue)
hiAndLazy [ea, eb] = do
    a <- eval ea
    case a of 
        e@(Left _) -> return e
        Right (HiValueBool False) -> return a
        Right HiValueNull -> return a
        _ -> eval eb
hiAndLazy _ = return $ Left HiErrorInvalidArgument

hiOrLazy :: HiMonad m => [HiExpr] -> m (Either HiError HiValue)
hiOrLazy [ea, eb] = do
    a <- eval ea
    case a of
        e@(Left _) -> return e
        Right (HiValueBool False) -> eval eb
        Right HiValueNull -> eval eb
        _ -> return a
hiOrLazy _ = return $ Left HiErrorInvalidArgument

hiIfLazy :: HiMonad m => [HiExpr] -> m (Either HiError HiValue)
hiIfLazy [ef, a, b] = do
    f <- eval ef 
    case f of
        e@(Left _) -> return e
        Right (HiValueBool True) -> eval a
        Right (HiValueBool False) -> eval b
        _ -> return $ Left HiErrorInvalidArgument 
hiIfLazy _ = return $ Left HiErrorInvalidArgument

hiImplStrict :: HiFun -> [HiValue] -> Either HiError HiValue
hiImplStrict HiFunAdd = hiAdd
hiImplStrict HiFunSub = hiSub
hiImplStrict HiFunMul = hiMul
hiImplStrict HiFunDiv = hiDiv
hiImplStrict HiFunNot = hiNot
hiImplStrict HiFunAnd = hiAnd
hiImplStrict HiFunOr = hiOr
hiImplStrict HiFunLessThan = hiLessThan
hiImplStrict HiFunGreaterThan = hiGreaterThan
hiImplStrict HiFunEquals = hiEquals
hiImplStrict HiFunNotLessThan = hiNotLessThan
hiImplStrict HiFunNotGreaterThan = hiNotGreaterThan
hiImplStrict HiFunNotEquals = hiNotEquals
hiImplStrict HiFunIf = hiIf
hiImplStrict HiFunLength = hiLength
hiImplStrict HiFunToUpper = hiToUpper
hiImplStrict HiFunToLower = hiToLower
hiImplStrict HiFunReverse = hiReverse
hiImplStrict HiFunTrim = hiTrim
hiImplStrict HiFunList = hiList
hiImplStrict HiFunRange = hiRange
hiImplStrict HiFunFold = hiFold
hiImplStrict HiFunPackBytes = hiPackBytes
hiImplStrict HiFunUnpackBytes = hiUnpackBytes
hiImplStrict HiFunEncodeUtf8 = hiEncodeUtf8
hiImplStrict HiFunDecodeUtf8 = hiDecodeUtf8
hiImplStrict HiFunZip = hiZip
hiImplStrict HiFunUnzip = hiUnzip
hiImplStrict HiFunSerialise = hiSerialise
hiImplStrict HiFunDeserialise = hiDeserialise
hiImplStrict HiFunRead = hiRead
hiImplStrict HiFunWrite = hiWrite
hiImplStrict HiFunMkDir = hiMkDir
hiImplStrict HiFunChDir = hiChDir
hiImplStrict HiFunParseTime = hiParseTime
hiImplStrict HiFunRand = hiRand
hiImplStrict HiFunEcho = hiEcho
hiImplStrict HiFunCount = hiCount
hiImplStrict HiFunKeys = hiKeys
hiImplStrict HiFunValues = hiValues
hiImplStrict HiFunInvert = hiInvert

hiAdd :: [HiValue] -> Either HiError HiValue
hiAdd [HiValueNumber a, HiValueNumber b] = Right $ HiValueNumber (a + b)
hiAdd [HiValueString a, HiValueString b] = Right $ HiValueString (a <> b)
hiAdd [HiValueList a, HiValueList b] = Right $ HiValueList (a <> b)
hiAdd [HiValueBytes a, HiValueBytes b] = Right $ HiValueBytes (a <> b)
hiAdd [HiValueTime t, HiValueNumber d] = Right $ HiValueTime $ addUTCTime (realToFrac d) t
hiAdd _ = Left HiErrorInvalidArgument

hiSub :: [HiValue] -> Either HiError HiValue
hiSub [HiValueNumber a, HiValueNumber b] = Right $ HiValueNumber (a - b)
hiSub [HiValueTime a, HiValueTime b] = Right $ HiValueNumber $ toRational $ diffUTCTime a b
hiSub _ = Left HiErrorInvalidArgument

hiMul :: [HiValue] -> Either HiError HiValue
hiMul [HiValueNumber a, HiValueNumber b] = Right $ HiValueNumber (a * b)
hiMul [HiValueString a, HiValueNumber n] = if denominator n == 1 && numerator n > 0
    then Right $ HiValueString (T.replicate (fromIntegral $ numerator n) a)
    else Left HiErrorInvalidArgument
hiMul [HiValueList a, HiValueNumber n] = if denominator n == 1 && numerator n > 0
    then Right $ HiValueList $ join (S.replicate (fromIntegral $ numerator n) a)
    else Left HiErrorInvalidArgument
hiMul [HiValueBytes a, HiValueNumber n] = if denominator n == 1 && numerator n > 0
    then Right $ HiValueBytes $ B.concat (replicate (fromIntegral $ numerator n) a)
    else Left HiErrorInvalidArgument
hiMul _ = Left HiErrorInvalidArgument

hiDiv :: [HiValue] -> Either HiError HiValue
hiDiv [HiValueNumber a, HiValueNumber b] = if (b == 0) then Left HiErrorDivideByZero else Right $ HiValueNumber (a / b)
hiDiv [HiValueString a, HiValueString b] = Right $ HiValueString $ T.concat [a, T.pack "/", b]
hiDiv _ = Left HiErrorInvalidArgument

hiNot :: [HiValue] -> Either HiError HiValue
hiNot [HiValueBool a] = Right $ HiValueBool (not a)
hiNot _ = Left HiErrorInvalidArgument

hiAnd :: [HiValue] -> Either HiError HiValue
hiAnd [a@(HiValueBool False), _] = Right a
hiAnd [a@(HiValueNull), _] = Right a
hiAnd [_, b] = Right b
hiAnd _ = Left HiErrorInvalidArgument

hiOr :: [HiValue] -> Either HiError HiValue
hiOr [HiValueBool False, b] = Right b
hiOr [HiValueNull, b] = Right b
hiOr [a, _] = Right a
hiOr _ = Left HiErrorInvalidArgument

hiLessThan :: [HiValue] -> Either HiError HiValue
hiLessThan [a, b] = Right $ HiValueBool (a < b)
hiLessThan _ = Left HiErrorInvalidArgument

hiGreaterThan :: [HiValue] -> Either HiError HiValue
hiGreaterThan [a, b] = hiLessThan [b, a]
hiGreaterThan _ = Left HiErrorInvalidArgument

hiEquals :: [HiValue] -> Either HiError HiValue
hiEquals [a, b] = Right $ HiValueBool (a == b)
hiEquals _ = Left HiErrorInvalidArgument

hiNotLessThan :: [HiValue] -> Either HiError HiValue
hiNotLessThan [a, b] = hiNotGreaterThan [b, a]
hiNotLessThan _ = Left HiErrorInvalidArgument

hiNotGreaterThan :: [HiValue] -> Either HiError HiValue
hiNotGreaterThan [a, b] = Right $ HiValueBool (a <= b)
hiNotGreaterThan _ = Left HiErrorInvalidArgument

hiNotEquals :: [HiValue] -> Either HiError HiValue
hiNotEquals [a, b] = Right $ HiValueBool (a /= b)
hiNotEquals _ = Left HiErrorInvalidArgument

hiIf :: [HiValue] -> Either HiError HiValue
hiIf [HiValueBool f, a, b] = Right $ if f then a else b
hiIf _ = Left HiErrorInvalidArgument

hiLength :: [HiValue] -> Either HiError HiValue
hiLength [HiValueString t] = Right $ HiValueNumber $ toRational $ T.length t
hiLength [HiValueList l] = Right $ HiValueNumber $ toRational $ length l
hiLength [HiValueBytes b] = Right $ HiValueNumber $ toRational $ B.length b
hiLength _ = Left HiErrorInvalidArgument

hiToUpper :: [HiValue] -> Either HiError HiValue
hiToUpper [HiValueString t] = Right $ HiValueString $ T.toUpper t
hiToUpper _ = Left HiErrorInvalidArgument

hiToLower :: [HiValue] -> Either HiError HiValue
hiToLower [HiValueString t] = Right $ HiValueString $ T.toLower t
hiToLower _ = Left HiErrorInvalidArgument

hiReverse :: [HiValue] -> Either HiError HiValue
hiReverse [HiValueString t] = Right $ HiValueString $ T.reverse t
hiReverse [HiValueList l] = Right $ HiValueList $ S.reverse l
hiReverse [HiValueBytes b] = Right $ HiValueBytes $ B.reverse b
hiReverse _ = Left HiErrorInvalidArgument

hiTrim :: [HiValue] -> Either HiError HiValue
hiTrim [HiValueString t] = Right $ HiValueString $ T.strip t
hiTrim _ = Left HiErrorInvalidArgument

hiString :: Text -> [HiValue] -> Either HiError HiValue
hiString _ [] = Left HiErrorArityMismatch
hiString _ (_:_:_:_) = Left HiErrorArityMismatch
hiString t [HiValueNumber i] = if denominator i == 1 then
    if numerator i >= 0 && numerator i < toInteger (T.length t)
        then Right $ HiValueString (T.singleton $ T.index t (fromIntegral $ numerator i))
        else Right $ HiValueNull
    else Left HiErrorInvalidArgument
hiString t [HiValueNumber l, HiValueNumber r] = if denominator l == 1 && denominator r == 1 then let
        l1 = if numerator l < 0 then max 0 (T.length t + fromIntegral (numerator l)) else fromIntegral $ numerator l
        r1 = if numerator r < 0 then max 0 (T.length t + fromIntegral (numerator r)) else fromIntegral $ numerator r
    in
        if l1 >= 0 && r1 >= 0 then
            if l1 < r1 then Right (HiValueString (T.drop l1 (T.take r1 t)))
            else Right $ HiValueString T.empty
        else Left HiErrorInvalidArgument
    else Left HiErrorInvalidArgument
hiString t [HiValueNull, r@(HiValueNumber _)] = hiString t [HiValueNumber 0, r]
hiString t [l@(HiValueNumber _), HiValueNull] = hiString t [l, HiValueNumber ((toInteger $ T.length t) % 1)]
hiString t [HiValueNull, HiValueNull] = Right $ HiValueString t
hiString _ _ = Left HiErrorInvalidArgument

hiList :: [HiValue] -> Either HiError HiValue
hiList l = Right $ HiValueList $ S.fromList l

hiRange :: [HiValue] -> Either HiError HiValue
hiRange [HiValueNumber l, HiValueNumber r] = Right $ HiValueList $ S.fromList $ map HiValueNumber [l..r]
hiRange _ = Left HiErrorInvalidArgument

hiFold :: [HiValue] -> Either HiError HiValue
hiFold [HiValueFunction f, HiValueList l] = let
        arity = hiArity f
        fun = hiImplStrict f
    in if arity == 2 || arity == -1
        then case l of
            Empty -> Right HiValueNull 
            (h:<|t) -> foldl (\a b -> case a of
                e@(Left _) -> e
                Right x -> fun [x, b]) (Right h) (toList t)
        else Left HiErrorInvalidArgument
hiFold _ = Left HiErrorInvalidArgument

hiListF :: Seq HiValue -> [HiValue] -> Either HiError HiValue
hiListF _ [] = Left HiErrorArityMismatch
hiListF _ (_:_:_:_) = Left HiErrorArityMismatch
hiListF t [HiValueNumber i] = if denominator i == 1 then
    if numerator i >= 0 && numerator i < toInteger (S.length t)
        then Right (S.index t (fromIntegral $ numerator i))
        else Right $ HiValueNull
    else Left HiErrorInvalidArgument
hiListF t [HiValueNumber l, HiValueNumber r] = if denominator l == 1 && denominator r == 1 then let
        l1 = if numerator l < 0 then max 0 (S.length t + fromIntegral (numerator l)) else fromIntegral $ numerator l
        r1 = if numerator r < 0 then max 0 (S.length t + fromIntegral (numerator r)) else fromIntegral $ numerator r
    in
        if l1 >= 0 && r1 >= 0 then
            if l1 < r1 then Right (HiValueList (S.drop l1 (S.take r1 t)))
            else Right $ HiValueList S.empty
        else Left HiErrorInvalidArgument
    else Left HiErrorInvalidArgument
hiListF t [HiValueNull, r@(HiValueNumber _)] = hiListF t [HiValueNumber 0, r]
hiListF t [l@(HiValueNumber _), HiValueNull] = hiListF t [l, HiValueNumber ((toInteger $ S.length t) % 1)]
hiListF t [HiValueNull, HiValueNull] = Right $ HiValueList t
hiListF _ _ = Left HiErrorInvalidArgument

hiPackBytes :: [HiValue] -> Either HiError HiValue
hiPackBytes [HiValueList l] = let
        toByte :: HiValue -> Either HiError Word8
        toByte (HiValueNumber n) = if denominator n == 1 && numerator n >= 0 && numerator n < 256 then
            Right ((fromIntegral $ numerator n) :: Word8) else Left HiErrorInvalidArgument
        toByte _ = Left HiErrorInvalidArgument
    in
        (mapM toByte l) >>= (\b -> Right $ HiValueBytes $ B.pack $ toList b)
hiPackBytes _ = Left HiErrorInvalidArgument

hiUnpackBytes :: [HiValue] -> Either HiError HiValue
hiUnpackBytes [HiValueBytes s] = Right $ HiValueList $ S.fromList $ map (HiValueNumber . (% 1) . (\w -> (fromIntegral w) :: Integer)) (B.unpack s)
hiUnpackBytes _ = Left HiErrorInvalidArgument

hiEncodeUtf8 :: [HiValue] -> Either HiError HiValue
hiEncodeUtf8 [HiValueString t] = Right $ HiValueBytes $ encodeUtf8 t
hiEncodeUtf8 _ = Left HiErrorInvalidArgument

hiDecodeUtf8 :: [HiValue] -> Either HiError HiValue
hiDecodeUtf8 [HiValueBytes s] = let
        res = decodeUtf8' s
        get (Left _) = HiValueNull
        get (Right t) = HiValueString t
    in
        Right $ get res
hiDecodeUtf8 _ = Left HiErrorInvalidArgument

hiZip :: [HiValue] -> Either HiError HiValue
hiZip [HiValueBytes s] = Right $ HiValueBytes $ BL.toStrict $
    compressWith defaultCompressParams { compressLevel = bestCompression } $ BL.fromStrict s
hiZip _ = Left HiErrorInvalidArgument

hiUnzip :: [HiValue] -> Either HiError HiValue --unsafe
hiUnzip [HiValueBytes s] = Right $ HiValueBytes $ BL.toStrict $
    decompress $ BL.fromStrict s
hiUnzip _ = Left HiErrorInvalidArgument

hiSerialise :: [HiValue] -> Either HiError HiValue
hiSerialise [a] = Right $ HiValueBytes $ BL.toStrict $ serialise a
hiSerialise _ = Left HiErrorInvalidArgument

hiDeserialise :: [HiValue] -> Either HiError HiValue
hiDeserialise [HiValueBytes s] = Right $ fromRight HiValueNull $ deserialiseOrFail $ BL.fromStrict s
hiDeserialise _ = Left HiErrorInvalidArgument

hiBytes :: ByteString -> [HiValue] -> Either HiError HiValue
hiBytes _ [] = Left HiErrorArityMismatch
hiBytes _ (_:_:_:_) = Left HiErrorArityMismatch
hiBytes t [HiValueNumber i] = if denominator i == 1 then
    if numerator i >= 0 && numerator i < toInteger (B.length t)
        then Right $ HiValueNumber $ toRational $ B.index t (fromIntegral $ numerator i)
        else Right $ HiValueNull
    else Left HiErrorInvalidArgument
hiBytes t [HiValueNumber l, HiValueNumber r] = if denominator l == 1 && denominator r == 1 then let
        l1 = if numerator l < 0 then max 0 (B.length t + fromIntegral (numerator l)) else fromIntegral $ numerator l
        r1 = if numerator r < 0 then max 0 (B.length t + fromIntegral (numerator r)) else fromIntegral $ numerator r
    in
        if l1 >= 0 && r1 >= 0 then
            if l1 < r1 then Right (HiValueBytes (B.drop l1 (B.take r1 t)))
            else Right $ HiValueList S.empty
        else Left HiErrorInvalidArgument
    else Left HiErrorInvalidArgument
hiBytes t [HiValueNull, r@(HiValueNumber _)] = hiBytes t [HiValueNumber 0, r]
hiBytes t [l@(HiValueNumber _), HiValueNull] = hiBytes t [l, HiValueNumber ((toInteger $ B.length t) % 1)]
hiBytes t [HiValueNull, HiValueNull] = Right $ HiValueBytes t
hiBytes _ _ = Left HiErrorInvalidArgument

hiRead :: [HiValue] -> Either HiError HiValue
hiRead [HiValueString t] = Right $ HiValueAction $ HiActionRead $ T.unpack t
hiRead _ = Left HiErrorInvalidArgument 

hiWrite :: [HiValue] -> Either HiError HiValue
hiWrite [HiValueString t, HiValueString s] = Right $ HiValueAction $ HiActionWrite (T.unpack t) (fromString $ T.unpack s)
hiWrite _ = Left HiErrorInvalidArgument 

hiMkDir :: [HiValue] -> Either HiError HiValue
hiMkDir [HiValueString t] = Right $ HiValueAction $ HiActionMkDir $ T.unpack t
hiMkDir _ = Left HiErrorInvalidArgument 

hiChDir :: [HiValue] -> Either HiError HiValue
hiChDir [HiValueString t] = Right $ HiValueAction $ HiActionChDir $ T.unpack t
hiChDir _ = Left HiErrorInvalidArgument 

hiParseTime :: [HiValue] -> Either HiError HiValue
hiParseTime [HiValueString t] =
    let
        time = readMaybe $ T.unpack t
    in
        case time of
            Nothing -> Right HiValueNull
            Just x -> Right $ HiValueTime x
hiParseTime _ = Left HiErrorInvalidArgument 

hiRand :: [HiValue] -> Either HiError HiValue
hiRand [HiValueNumber a, HiValueNumber b] = if denominator a == 1 && denominator b == 1
    then Right $ HiValueAction $ HiActionRand (fromIntegral $ numerator a) (fromIntegral $ numerator b)
    else Left HiErrorInvalidArgument 
hiRand _ = Left HiErrorInvalidArgument 

hiEcho :: [HiValue] -> Either HiError HiValue
hiEcho [HiValueString t] = Right $ HiValueAction $ HiActionEcho t
hiEcho _ = Left HiErrorInvalidArgument 

counterMap :: [HiValue] -> HiValue
counterMap l = HiValueDict $ M.map HiValueNumber $ M.fromListWith (+) $ zip l (repeat 1)

hiCount :: [HiValue] -> Either HiError HiValue
hiCount [HiValueList l] = Right $ counterMap $ toList l
hiCount [HiValueString t] = Right $ counterMap $ map (HiValueString . T.singleton) $ T.unpack t
hiCount [HiValueBytes s] = Right $ counterMap $ map (HiValueNumber . (% 1) . (\w -> (fromIntegral w) :: Integer)) (B.unpack s)
hiCount _ = Left HiErrorInvalidArgument 

hiKeys :: [HiValue] -> Either HiError HiValue
hiKeys [HiValueDict d] = Right $ HiValueList $ S.fromList $ M.keys d
hiKeys _ = Left HiErrorInvalidArgument 

hiValues :: [HiValue] -> Either HiError HiValue
hiValues [HiValueDict d] = Right $ HiValueList $ S.fromList $ M.elems d
hiValues _ = Left HiErrorInvalidArgument 

hiInvert :: [HiValue] -> Either HiError HiValue
hiInvert [HiValueDict d] = Right $ HiValueDict $ M.map (HiValueList . S.fromList) $ M.fromListWith (++) $ map (\(k, v) -> (v, [k])) $ M.toList d
hiInvert _ = Left HiErrorInvalidArgument 

hiDict :: Map HiValue HiValue -> [HiValue] -> Either HiError HiValue
hiDict d [t] = case M.lookup t d of
    Nothing -> Right HiValueNull
    Just v -> Right v
hiDict _ _ = Left HiErrorInvalidArgument 
