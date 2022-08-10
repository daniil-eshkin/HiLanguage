{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module HW3.Parser (
    parse
) where

import HW3.Base ( HiExpr (..), HiValue (..), HiFun (..), HiAction (..) )
import Text.Megaparsec ( ParseErrorBundle, Parsec, choice, many, runParser, (<|>), MonadParsec (eof, try, notFollowedBy), manyTill, satisfy, sepEndBy, sepBy1 )
import Data.Void ( Void )
import Text.Megaparsec.Char ( string, char, space, hexDigitChar, space1 )
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Monad.RWS.Strict (void)
import Data.Scientific ( Scientific(..) )
import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(..) )
import Data.Word (Word8)
import Data.Char (digitToInt, isAlpha, isAlphaNum)
import qualified Data.List as List

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = let
    hiFunctions :: [HiFun]
    hiFunctions =
        [ HiFunDiv
        , HiFunMul
        , HiFunAdd
        , HiFunSub
        , HiFunAnd
        , HiFunOr
        , HiFunLessThan
        , HiFunGreaterThan
        , HiFunEquals
        , HiFunNotLessThan
        , HiFunNotGreaterThan
        , HiFunNotEquals
        , HiFunNot
        , HiFunIf
        , HiFunLength
        , HiFunToUpper
        , HiFunToLower
        , HiFunReverse
        , HiFunTrim
        , HiFunList
        , HiFunRange
        , HiFunFold
        , HiFunPackBytes
        , HiFunUnpackBytes
        , HiFunEncodeUtf8
        , HiFunDecodeUtf8
        , HiFunZip
        , HiFunUnzip
        , HiFunSerialise
        , HiFunDeserialise
        , HiFunRead
        , HiFunWrite
        , HiFunMkDir
        , HiFunChDir
        , HiFunParseTime
        , HiFunRand
        , HiFunEcho
        , HiFunCount
        , HiFunKeys
        , HiFunValues
        , HiFunInvert ]

    infixL :: String -> HiFun-> Operator Parser HiExpr
    infixL name f = InfixL ((\a b -> HiExprApply (HiExprValue $ HiValueFunction f) [a, b]) <$ lexString name)

    infixR :: String -> HiFun-> Operator Parser HiExpr
    infixR name f = InfixR ((\a b -> HiExprApply (HiExprValue $ HiValueFunction f) [a, b]) <$ lexString name)

    infixN :: String -> HiFun-> Operator Parser HiExpr
    infixN name f = InfixN ((\a b -> HiExprApply (HiExprValue $ HiValueFunction f) [a, b]) <$ lexString name)

    operatorTable =
        [ [ infixL "*" HiFunMul
          , InfixL ((\a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunDiv) [a, b]) <$ (lexeme . try) (do
              void (char '/')
              notFollowedBy (char '='))) ]
        , [ infixL "+" HiFunAdd
          , infixL "-" HiFunSub ]
        , [ infixN "<=" HiFunNotGreaterThan
          , infixN ">=" HiFunNotLessThan
          , infixN "<" HiFunLessThan
          , infixN ">" HiFunGreaterThan
          , infixN "==" HiFunEquals
          , infixN "/=" HiFunNotEquals ]
        , [ infixR "&&" HiFunAnd ]
        , [ infixR "||" HiFunOr ] ]

    parseInfix' :: Parser HiExpr
    parseInfix' = do
        void (lexChar '(')
        expr <- parseInfix
        void (lexChar ')')
        return expr

    parseInfix :: Parser HiExpr
    parseInfix = makeExprParser parseExpr operatorTable

    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme space

    lexChar :: Char -> Parser Char
    lexChar = lexeme . char

    lexString :: [Char] -> Parser [Char]
    lexString = lexeme . string

    lexNum :: Parser Scientific
    lexNum = L.signed space (lexeme L.scientific)

    parseExpr :: Parser HiExpr
    parseExpr = do
        name <- parseValue <|> parseInfix'
        args <- lexeme $ many (parseArgs <|> parseDotAccess <|> (Nothing <$ lexChar '!'))
        return $ foldl (\a b -> case b of
            Nothing -> HiExprRun a
            Just x -> HiExprApply a x) name args

    parseValue :: Parser HiExpr
    parseValue = choice
        [ parseNum
        , parseFun
        , parseBool
        , parseString
        , parseNull
        , parseBytes
        , parseList
        , parseAction
        , parseDict ]

    parseKeyValue :: Parser (HiExpr, HiExpr)
    parseKeyValue = do
        k <- parseInfix
        void (lexChar ':')
        v <- parseInfix
        return (k, v)

    parseDict :: Parser HiExpr
    parseDict = do
        void (lexChar '{')
        res <- (do
            eHead <- parseKeyValue
            eTail <- many (do
                void (lexChar ',')
                parseKeyValue )
            return (eHead:eTail)) <|> return []
        void (lexChar '}')
        return $ HiExprDict res

    parseAction :: Parser HiExpr
    parseAction = do
        action <- choice
            [ HiActionCwd <$ lexString "cwd"
            , HiActionNow <$ lexString "now" ]
        return (HiExprValue $ HiValueAction action)

    parseByte :: Parser Word8
    parseByte = do
        a <- hexDigitChar
        b <- hexDigitChar
        return (let
            x = 16 * digitToInt a + digitToInt b
            in (fromIntegral x) :: Word8)

    parseBytes:: Parser HiExpr
    parseBytes = do
        void (lexString "[#")
        res <- sepEndBy parseByte space1
        void (lexString "#]")
        return $ HiExprValue $ HiValueBytes $ B.pack res

    parseList :: Parser HiExpr
    parseList = do
        void (lexChar '[')
        res <- (do
            eHead <- parseInfix
            eTail <- many (do
                void (lexChar ',')
                parseInfix )
            return (eHead:eTail)) <|> return []
        void (lexChar ']')
        return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) res

    parseString :: Parser HiExpr
    parseString = do
        str <- lexeme (char '"' >> manyTill L.charLiteral (char '"'))
        return $ HiExprValue $ HiValueString $ T.pack str

    parseNum :: Parser HiExpr
    parseNum = do
        num <- lexNum
        return (HiExprValue $ HiValueNumber $ toRational num)

    funChoice :: Parser HiFun
    funChoice = choice $ map (\f -> f <$ (lexString $ show f)) hiFunctions

    parseFun :: Parser HiExpr
    parseFun = do
        name <- funChoice
        return (HiExprValue $ HiValueFunction name)

    parseBool :: Parser HiExpr
    parseBool = do
        val <- choice
            [ True <$ lexString "true"
            , False <$ lexString "false" ]
        return (HiExprValue $ HiValueBool val)

    parseNull :: Parser HiExpr
    parseNull = do
        void (lexString "null")
        return (HiExprValue HiValueNull)

    parseDotAccessKey :: Parser String
    parseDotAccessKey = do 
        k <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
        return $ List.intercalate "-" k

    parseDotAccess :: Parser (Maybe [HiExpr])
    parseDotAccess = do
        void (char '.')
        key <- parseDotAccessKey
        return $ Just [HiExprValue $ HiValueString $ T.pack $ key]

    parseArgs :: Parser (Maybe [HiExpr])
    parseArgs = do
        void (lexChar '(')
        res <- (do
            eHead <- parseInfix
            eTail <- many (do
                void (lexChar ',')
                parseInfix )
            return (eHead:eTail)) <|> return []
        void (lexChar ')')
        return $ Just res
    in
        runParser (space *> parseInfix <* eof) ""
