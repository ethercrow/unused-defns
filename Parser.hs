{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Monad (void)
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Trifecta
import Text.Parser.Token.Highlight (Highlight(..))
import Text.Trifecta.Delta

import Types

parseText :: T.Text -> Either T.Text AST
parseText t =
    case parseByteString parseTopLevel (Lines 0 0 0 0) (T.encodeUtf8 t) of
        Success ast -> Right ast
        Failure diag -> Left (T.pack (show diag))

parseFile :: FilePath -> IO (Maybe AST)
parseFile = parseFromFile parseTopLevel

parseTopLevel :: Parser AST
parseTopLevel = (() :<) . List <$> many' parseList

many' :: Parser a -> Parser [a]
many' p = skipStuff >> sepEndBy p skipStuff

skipStuff :: Parser ()
skipStuff = do
    _ <- many $ choice [ void comma
                       , void $ oneOf " \t\n"
                       , void $ char ';' >> many (noneOf "\n") >> newline
                       ]
    return ()

parseExpr :: Parser AST
parseExpr = 
    choice [ parseList
           , parseAnonFunction
           , parseVec
           , parseMap
           , parseSet
           , parseQuote
           , parseLitString
           , parseLitRegex
           , parseAtom
           ]

parseList :: Parser AST
parseList = (() :<) . List <$> parens (many' parseExpr)

parseAnonFunction :: Parser AST
parseAnonFunction = (() :<) . List <$> between (text "#(")
                                               (text ")")
                                               (many' parseExpr)

parseVec :: Parser AST
parseVec = (() :<) . Vec <$> brackets (many' parseExpr)

parseMap :: Parser AST
parseMap = (() :<) . Map <$> braces (many' exprPair)
    where exprPair = do
            key <- parseExpr
            skipStuff
            value <- parseExpr
            return (key, value)

parseSet :: Parser AST
parseSet =
    (() :<) . Set <$> between (text "#{")
                              (text "}")
                              (many' parseExpr)

parseQuote :: Parser AST
parseQuote =
    (() :<) . Quote <$> (void (text "'") >> parseExpr)

parseLitRegex :: Parser AST
parseLitRegex = fmap ((() :<) . CljRegexLiteral . CljRegex . T.pack) . try $ do
    void $ text "#"
    between (text "\"")
            (text "\"")
            (many (noneOf "\""))

parseLitString :: Parser AST
parseLitString = (() :<) . CljStringLiteral . CljString  <$> javaStringLiteral

parseAtom :: Parser AST
parseAtom = (() :<) . Atom . T.pack <$> some (noneOf " ()[]{},\n")

javaStringLiteral :: (TokenParsing m, IsString s) => m s
javaStringLiteral = fromString <$> token (highlight StringLiteral lit) where
  lit = Prelude.foldr (maybe id (:)) ""
    <$> between (char '"') (char '"' <?> "end of string") (many stringChar)
    <?> "string"
  stringChar = Just <$> stringLetter
           <|> stringEscape
       <?> "string character"
  stringLetter = satisfy (\c -> (c == '\n') || ((c /= '"') && (c /= '\\') && (c > '\026')))
  stringEscape = highlight EscapeCode $ char '\\' *> esc where
    esc = Nothing <$ escapeGap
      <|> Nothing <$ escapeEmpty
      <|> Just <$> escapeCode
  escapeEmpty = char '&'
  escapeGap = skipSome space *> (char '\\' <?> "end of string gap")
{-# INLINE javaStringLiteral #-}

escapeCode :: TokenParsing m => m Char
escapeCode = (charEsc <|> charUnicode <|> charNum <|> charAscii <|> charControl) <?> "escape code"
  where
  charControl = (\c -> toEnum (fromEnum c - fromEnum '@')) <$> (char '^' *> (upper <|> char '@'))
  charNum     = toEnum . fromInteger <$> num where
    num = decimal
      <|> (char 'o' *> number 8 octDigit)
      <|> (char 'x' *> number 16 hexDigit)
  charUnicode = toEnum . fromInteger <$> (char 'u' *> number 16 hexDigit)
  charEsc = choice $ parseEsc <$> escMap
  parseEsc (c,code) = code <$ char c
  escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"
  charAscii = choice $ parseAscii <$> asciiMap
  parseAscii (asc,code) = try $ code <$ string asc
  asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
  ascii2codes, ascii3codes :: [String]
  ascii2codes = [ "BS","HT","LF","VT","FF","CR","SO"
                , "SI","EM","FS","GS","RS","US","SP"]
  ascii3codes = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK"
                ,"BEL","DLE","DC1","DC2","DC3","DC4","NAK"
                ,"SYN","ETB","CAN","SUB","ESC","DEL"]
  ascii2, ascii3 :: String
  ascii2 = "\BS\HT\LF\VT\FF\CR\SO\SI\EM\FS\GS\RS\US\SP"
  ascii3 = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"

number :: TokenParsing m => Integer -> m Char -> m Integer
number base baseDigit =
  foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 <$> some baseDigit
