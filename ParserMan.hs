module ParserMan where 

import Control.Applicative
import Control.Monad
import Data.Char

-- main :: IO ()
-- main = undefined

newtype Parser a = Parser { runParser :: String -> Maybe (a , String) }

instance Functor Parser where
    fmap func (Parser a) = Parser b where 
                                b str = case a str of
                                    Nothing -> Nothing
                                    Just (val, rest) -> Just (func val, rest)

instance Applicative Parser where
    pure val = Parser (\str -> Just (val, str))
    (<*>) (Parser a) (Parser b) = Parser $ \str -> 
                                        case a str of 
                                        Nothing -> Nothing 
                                        Just (fun, rest) -> 
                                            case b rest of
                                            Nothing -> Nothing
                                            Just (val, otherRest) -> Just (fun val, otherRest)

instance Alternative Parser where
    empty = Parser (\_ -> Nothing)
    (<|>) (Parser a) (Parser b) = Parser (\ str -> (a str) <|> (b str))

charP :: Char -> Parser Char
charP char = Parser prs 
        where prs (x:xs) 
                | x == char = Just (char, xs)
                | otherwise = Nothing
              prs [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spaceP :: Parser String
spaceP = spanP isSpace

boolPrs :: Parser Bool
boolPrs = const False <$> stringP "false" 
        <|> const True <$> stringP "true"

spanP :: (Char -> Bool) -> Parser String
spanP pred = Parser (\ str -> let (val, rest) = span pred str in 
                                    Just (val, rest))
strictSpanP :: (Char -> Bool) -> Parser String
strictSpanP pred = Parser (\ str -> let (val, rest) = span pred str in 
                                    if val == "" then Nothing else Just (val, rest))

strLitInternalP :: Parser String
strLitInternalP = spanP (/= '\"')

strLitP = charP '\"' *> strLitInternalP <* charP '\"'

antiCharP :: Char -> Parser Char
antiCharP char = Parser (\ (x:xs) -> if x == char then Nothing else Just (x, xs))

charLitP :: Parser Char
charLitP = charP '\'' *> (antiCharP '\'') <* charP '\''