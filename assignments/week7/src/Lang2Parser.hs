module Lang2Parser where

import Lang2 hiding (test1, test2)
import MyParserLib


-- It is recommended you use the following helper functions


parseAstInt :: Parser Ast
parseAstInt = eatS intParser
                `mapParser` (\ i -> AstInt i)

pluses :: Parser [Ast]
pluses = (rep ( eatS ((literal "+") +++ parseIntOrParensOrPrint)
            `mapParser` \ (_, ast) -> ast) )


seperators :: Parser [Ast]
seperators = (rep ( eatS (literal ";" +++ plusExpParser)
            `mapParser` \ (_, ast) -> ast) )

parseParens :: Parser Ast
parseParens = eatS (literal "(") +++ parser +++ eatS (literal ")")
                `mapParser` \ ((_, ast), _) -> ast

parsePrint :: Parser Ast
parsePrint = eatS (literal "print") +++ eatS (literal "(") +++ parser +++ eatS (literal ")")
                `mapParser` \ (((_,_),ast),_) -> Print ast

parseIntOrParensOrPrint  :: Parser Ast
parseIntOrParensOrPrint = (parseAstInt <|> parseParens <|> parsePrint)
                             `mapParser` \ ast -> case ast of
                                                    Left (Left a)  -> a
                                                    Left (Right a)  -> a
                                                    Right a -> a

plusExpParser :: Parser Ast
plusExpParser =  parseIntOrParensOrPrint +++ pluses `mapParser` combinePlus

-- could be a fold
combinePlus :: (Ast, [Ast]) -> Ast
combinePlus (a, []) = a
combinePlus (a, (h:t)) =  a `Plus` (combinePlus (h, t))

combineSeparator :: (Ast, [Ast]) -> Ast
combineSeparator (a, []) = a
combineSeparator (a, (h:t)) =  a `Separator` (combineSeparator (h, t))



parser :: Parser Ast
parser = plusExpParser +++ seperators `mapParser` combineSeparator



unsafeParser :: String -> Ast
unsafeParser input = case ((parser +++ spaces) `mapParser`  \ (ast, _) -> ast) input of
                            Just (ast,"")  -> ast
                            _              -> error "you made a typo"
