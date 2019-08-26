module LangParser where

import Lang
import ParserMonad



parser :: Parser Ast
parser = apps <||>  atoms


-- note that the parser must respect all the precedence and associativity rules expressed in the prettyShow function.
-- that means
-- ! binds more tightly than
-- * / which binds more tightly than
-- + - which binds more tightly than
-- && which binds more tightly than
-- || which binds more tightly than
-- : which binds more tightly than
-- {the application} which binds weakest of all

-- + - * / && || {the application} are left associative
-- : is right associative

-- we are mostly following the questionable c precedence rules

-- ungraded bonus: add additional pretty syntax for lists: [1,2,3,4]




-- some general functions that make parsing this easier

oneOf :: [Parser a] -> Parser a
oneOf [] = failParse
oneOf (pa:rest) = pa <||> oneOf rest

-- *LangParser> parse (oneOf [ints,bools]) "-78"
-- Just (-78,"")
-- *LangParser> parse (oneOf [ints,bools]) " true"
-- Just (true,"")
-- *LangParser> parse (oneOf [ints,bools]) " tr ue"
-- Nothing


-- we saw before the midterm that there were issues when there are multiple operators with the same precedence, this is a helper function to handle those
-- this generalizes the helper function posted on piaza from last time
-- it is left associative
withInfix :: Parser a -> [(String, a -> a -> a)] -> Parser a
withInfix pa ls = let operators = fmap fst ls
                      opParsers = fmap (\ s -> token $ literal s) operators

                      --innerParser :: a -> Parser a, where a is the same as above
                      innerParser left = do s <- oneOf opParsers
                                            next <- pa
                                            case lookup s ls of
                                             Nothing -> failParse
                                             Just f ->  let out = f left next
                                                        in (innerParser out) <||> return out
                  in do l <- pa
                        (innerParser l) <||> (return l)

-- *LangParser> parse (withInfix intParser [("+", (+)), ("-", (-))]) "1+2+3+4-5"
-- Just (5,"")
-- *LangParser> parse (withInfix intParser [("+", (+)), ("-", (-))]) "100-1-10"
-- Just (89,"")


-- you may want to structure you grammar like this:

keywords = ["if","then","else", "let", "in", "true","false"]


vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

ints :: Parser Ast
ints = do i <- token $ intParser
          return $ ValInt i

bools :: Parser Ast
bools = do b <- token $ varParser
           case b of "true"  -> return $ ValBool True
                     "false" -> return $ ValBool False
                     _       -> failParse

nil :: Parser Ast
nil = do token $ literal "["
         token $ literal "]"
         return $ Nil


apps :: Parser Ast
apps = withInfix cons [("",App)] -- the tokens eat up all the spaces so we split on the empty string


cons :: Parser Ast
cons = (do l <- orExpr
           token $ literal ":"
           r <- cons
           return $ Cons l r) <||> orExpr


-- *LangParser> parse cons "1 : 4: true"
-- Just (1 : 4 : true,"")
-- *LangParser> parse cons "1 : 4: 3+5"
-- Just (1 : 4 : (3 + 5),"")


orExpr :: Parser Ast
orExpr = (do l <- andExpr
             token $ literal "||"
             r <- orExpr
             return $ Or l r) <||> andExpr


-- *LangParser> parse orExpr "true || false && 7"
-- Just (true || false && 7,"")
-- *LangParser> parse orExpr "true || false || 7"
-- Just (true || false || 7,"")
-- *LangParser> parse orExpr "true"
-- Just (true,"")

andExpr :: Parser Ast
andExpr = (do l   <- addSubExpr
              token $ literal "&&"
              r   <- andExpr
              return $ And l r) <||> addSubExpr

-- *LangParser> parse andExpr "false"
-- Just (false,"")
-- *LangParser> parse andExpr "false && false"
-- Just (false && false,"")

addSubExpr :: Parser Ast
addSubExpr = withInfix multDivExpr [("+", Plus), ("-", Minus)]

-- *LangParser> parse addSubExpr "1+2+3+4"
-- Just (1 + 2 + 3 + 4,"")
-- *LangParser> parse addSubExpr "1-2-3-4"
-- Just (1 - 2 - 3 - 4,"")

multDivExpr :: Parser Ast
multDivExpr = withInfix notExp [("*", Mult), ("/", Div)]

notExp :: Parser Ast
notExp = (do token $ literal "!"
             a' <- notExp
             return $ Not a') <||> atoms


atoms:: Parser Ast
atoms = ints <||> bools  <||>  nil <||> parens <||> ifParser <||> letParser <||>  lambdaParser <||> vars

-- *LangParser> parse atoms "111"
-- Just (111,"")
-- *LangParser> parse atoms "  true"
-- Just (true,"")

ifParser :: Parser Ast
ifParser = do token $ literal "if "
              a <- parser
              token $ literal "then "
              b <- parser
              token $ literal "else "
              c <- parser
              return $ If a b c

letParser :: Parser Ast
letParser = do token $ literal "let "
               str <- (token $ varParser)
               token $ literal "="
               a <- parser
               token $ literal "in "
               b <- parser
               return $ Let str a b

-- *LangParser> parse letParser "let x=3 in x+x"
-- Just (let x = 3 in x + x,"")



lambdaParser :: Parser Ast
lambdaParser = do token $ literal "\\"
                  s   <- token $ varParser
                  token $ literal "->"
                  body <- parser
                  return $ Lam s body


parens :: Parser Ast
parens = do (token $ literal "(")
            a <- parser
            (token $ literal ")")
            return a


-- *LangParser> parse parser "(true)"
-- Just (true,"")
-- *LangParser> parse parser "let x = (if true && false then 3 else elsee) in x + x"
-- Just (let x = if true && false then 3 else elsee in x + x,"")




-- Some examples of weird stuff

ex = prettyShow  (Mult (Var "a") (Or (Var "b") (Var "c"))) 0

ex1 = prettyShow (Minus (Var "y") (Minus (App (ValBool True) (ValInt (-3))) (Mult (ValBool False) (ValBool False)))) 0

ex2 = prettyShow (Cons (Var "z") (Not (Not (Plus (Mult (ValInt (-18)) Nil) (Not (ValInt 2)))))) 0

ex3 = "! ! (-18)"

ex4 = prettyShow (Mult (ValInt (-18)) Nil) 0
