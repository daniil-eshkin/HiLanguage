module HW3.Pretty (
    prettyValue
) where

import HW3.Base ( HiValue (..), HiAction (..) )
import Prettyprinter ( Doc, viaShow, Pretty (pretty) )
import Prettyprinter.Render.Terminal ( AnsiStyle )
import Data.Ratio ( denominator, numerator )
import qualified Data.Sequence as S
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Char (intToDigit)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Scientific (fromRationalRepetendUnlimited, formatScientific, FPFormat (Fixed))

prettyByteString :: ByteString -> Doc AnsiStyle 
prettyByteString s = let
        wordToInt :: Word8 -> Int
        wordToInt w = (fromIntegral w) :: Int

        int8ToString :: Int -> String
        int8ToString x = [(intToDigit $ (div x 16)), (intToDigit (mod x 16))]
    in B.foldl (\x b -> x <> pretty " " <> pretty (int8ToString $ wordToInt b)) (pretty "[#") s <> pretty " #]"

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber n) = let
    num = numerator n
    denom = denominator n
    in
        if (denom == 1) then pretty num else let
            finite :: Integer -> Bool
            finite 0 = False
            finite 1 = True
            finite d = if (even d) then finite $ div d 2 else (mod d 5 == 0) && (finite $ div d 5)

            (quo, re) = quotRem num denom
        in
            if finite denom then let
                    (a, b) = fromRationalRepetendUnlimited n
                in pretty (formatScientific Fixed b a) else
                if quo == 0 then pretty (show re ++ "/" ++ show denom) else
                    pretty (show quo ++ (if quo < 0 then " - " else " + ") ++ show (abs re) ++ "/" ++ show denom)
prettyValue (HiValueFunction f) = pretty $ show f
prettyValue (HiValueBool b) = pretty $ if b then "true" else "false"
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueString t) = viaShow t
prettyValue (HiValueList l) = pretty "[ "
    <> S.foldMapWithIndex (\i x -> prettyValue x <> (if i + 1 < length l then pretty ", " else pretty " ")) l
    <> pretty "]"
prettyValue (HiValueBytes s) = prettyByteString s
prettyValue (HiValueTime time) = pretty "parse-time(\"" <> viaShow time <> pretty "\")"
prettyValue (HiValueAction HiActionCwd) = pretty "cwd"
prettyValue (HiValueAction (HiActionRead p)) = pretty "read(" <> viaShow p <> pretty ")"
prettyValue (HiValueAction (HiActionWrite p s)) = pretty "write(" <> viaShow p <> pretty ", " <> prettyByteString s <> pretty ")"
prettyValue (HiValueAction (HiActionMkDir p)) = pretty "mkdir(" <> viaShow p <> pretty ")"
prettyValue (HiValueAction (HiActionChDir p)) = pretty "cd(" <> viaShow p <> pretty ")"
prettyValue (HiValueAction HiActionNow) = pretty "now"
prettyValue (HiValueAction (HiActionRand a b)) = pretty "rand(" <> pretty a <> pretty ", " <> pretty b <> pretty ")"
prettyValue (HiValueAction (HiActionEcho t)) = pretty "echo(" <> viaShow t <> pretty ")"
prettyValue (HiValueDict m) = let l = S.fromList (M.assocs m) in pretty "{"
    <> S.foldMapWithIndex (\i (k, v) 
        -> pretty " " 
        <> prettyValue k 
        <> pretty ": "
        <> prettyValue v
        <> (if i + 1 < S.length l then pretty "," else mempty)) l
    <> pretty " }"
