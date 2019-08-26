module Lang4Parser where

import Data.Char
import Data.Map(Map, lookup, insert, empty, fromList)  -- for State in tests

import Lang4 hiding (test1, test2)
import MyParserLib



-- It is recommended you use the following helper functions

idParser :: Parser Ast
idParser = eatS varParser `mapParser` \ str -> Id str

parseAstInt :: Parser Ast
parseAstInt = eatS intParser
                `mapParser` (\ i -> AstInt i)

letParser :: Parser Ast
letParser = eatS (literal "let ") +++ eatS varParser +++ eatS (literal "=") +++ parser +++ eatS (literal "in") +++ parser
              `mapParser` \ (((((_, str), _), ast1), _), ast2) -> Let str ast1 ast2

pluses :: Parser [Ast]
pluses  = (rep ( eatS ((literal "+") +++ parseIntOrParensOrEq)
            `mapParser` \ (_, ast) -> ast) )

seperators :: Parser [Ast]
seperators  = (rep ( eatS (literal ";" +++ plusExpParser)
            `mapParser` \ (_, ast) -> ast) )

parseParens :: Parser Ast
parseParens  = eatS (literal "(") +++ parser +++ eatS (literal ")")
                `mapParser` \ ((_, ast), _) -> ast

parseIntOrParensOrEq :: Parser Ast
parseIntOrParensOrEq  = parseAstInt <|> parseParens <|> letParser <|> idParser
                        `mapParser` \ ast -> case ast of
                                                Left (Left (Left a))  -> a
                                                Left (Left (Right a))  -> a
                                                Left (Right a) -> a
                                                Right a        -> a

plusExpParser :: Parser Ast
plusExpParser  = parseIntOrParensOrEq +++ pluses `mapParser` combinePlus

combinePlus :: (Ast, [Ast]) -> Ast
combinePlus (a, []) = a
combinePlus (a, (h:t)) =  a `Plus` (combinePlus (h, t))

combineSeparator :: (Ast, [Ast]) -> Ast
combineSeparator (a, []) = a
combineSeparator (a, (h:t)) =  a `Separator` (combineSeparator (h, t))

parser :: Parser Ast
parser  = plusExpParser +++ seperators `mapParser` combineSeparator

unsafeParser :: String -> Ast
unsafeParser input = case ((parser +++ spaces) `mapParser`  \ (ast, _) -> ast) input of
                            Just (ast,"")  -> ast
                            _              -> error "you made a typo"
