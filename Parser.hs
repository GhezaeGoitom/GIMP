module Parser where
import Grammer
import Interpreter




-- We used a function 'parse'
-- which unwraps our parsers. It is equivalent to the following:
newtype Parser a = P (String -> [(a,String)])  
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp



instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> [(g v,out)])


instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v,inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                            []        -> []
                            [(g,out)] -> parse (fmap g px) out)                             
  
instance Monad Parser where
  -- return :: a -> Parser a
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> parse (f v) out)

                   
class Monad f => Alternative f where  
  empty :: f a
  (<|>) :: f a -> f a -> f a
  many :: f a -> f [a]
  some :: f a -> f [a]
  many x = some x <|> pure []
  some x = pure (:) <*> x <*> many x
  chain :: f a -> f (a -> a -> a) -> f a          -- chain operator
  chain p op = do a <- p; rest a
        where
            rest a = (do f <- op; b <- p; rest (f a b)) <|> return a


-- Parsers are alternative so p <|> q applies q only if p fails

instance Alternative Parser where     
  empty = P (const [])

  (P p) <|> (P q) =
    P( \input -> case p input of
          [] -> q input
          [(v, out)] -> [(v, out)]
      )


-- unit parser

item :: Parser Char  
item =
    P (\input -> case input of 
        [] -> []
        (x : xs) -> [(x, xs)]) 

satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
    do 
      x <- item 
      if p x then return x else empty -- return item defined above

-- elem is from prelude returns True if the list contains 
-- an item equal to the first argument From

digits :: [Char]             
digits = ['0' .. '9']

isDigit :: Char -> Bool  
isDigit x = elem x digits    

digitCase :: Parser Char
digitCase = satisfy isDigit

lowers :: [Char]
lowers = ['a' .. 'z']

isLower :: Char -> Bool
isLower x = elem x lowers

lowerCase :: Parser Char 
lowerCase = satisfy isLower

uppers :: [Char]
uppers = ['A' .. 'Z']

isUpper :: Char -> Bool
isUpper x = elem x uppers

upperCase :: Parser Char 
upperCase = satisfy isUpper 

isLetter :: Char -> Bool 
isLetter x = isUpper x || isLower x

letterCase :: Parser Char 
letterCase = satisfy isLetter

isAlphaNum :: Char -> Bool 
isAlphaNum x = isLetter x || isDigit x

alphaNumCase :: Parser Char
alphaNumCase = satisfy isAlphaNum

char :: Char -> Parser Char 
char x = satisfy (== x)

string :: String -> Parser String 
string [] = return []
string (x : xs) = 
    do 
        char x
        string xs 
        return (x : xs)

anIdentifier :: Parser String  
anIdentifier = 
    do 
        x <- letterCase
        xs <- many alphaNumCase    -- Many takes from 0 to n
        return (x : xs)


-- Integer positive number
natInt :: Parser Int        
natInt =                   
    do 
        xs <- some digitCase
        return (read xs)

--Integer positive and negative numbers

int :: Parser Int                  
int = 
    do
    char '-' 
    n <- natInt
    return (-n)
    <|> natInt             


spaces :: [Char]
spaces = ['\n', '\t', '\r', ' ']

isSpace :: Char -> Bool
isSpace x = elem x spaces

aSpace :: Parser ()               
aSpace = do 
            many (satisfy isSpace)        
            return ()

-- detect numbers,identifiers and commands
token :: Parser a -> Parser a         -- deletes spaces
token p =
    do 
        aSpace
        v <- p
        aSpace
        return v


identifier :: Parser String           
identifier = token anIdentifier

naturalNumber :: Parser Int           
naturalNumber = token natInt

integer :: Parser Int                 
integer = token int

symbol :: String -> Parser String     
symbol xs = token (string xs)

-- ARITHMETIC EVALUATION --

aExp  :: Parser AExp         
aExp = do chain aTerm op
    where                        
      op = 
          (do symbol "+"; return Add)
          <|> do symbol "-"; return Sub

aTerm :: Parser AExp    
aTerm = do chain aFactor op     
    where 
      op = 
        (do symbol "*"; return Mul)
        <|> (do symbol "/"; return Div)
        <|> (do symbol "^"; return Power)


aFactor :: Parser AExp
aFactor = do
    (Constant <$> integer)      
        <|> do
            i <- identifier     
            do
                symbol "["      
                n <- aExp  
                symbol "]"
                return (ArrayVar i n)
                <|> return (Avar i) 
        <|> do
            symbol "("                   
            a <- aExp 
            symbol ")"
            return a

-- BOOLEAN EVALUATION---
-- (), <, >, <=, >=, ==,!=,True, False, Not

bExp :: Parser BExp       
bExp = chain bTerm op            
  where op = do                  
            symbol "Or"
            return Or

bTerm :: Parser BExp        
bTerm = chain bFact op        
  where op = do
            symbol "And"
            return And

bFact :: Parser BExp
bFact =
  do
    symbol "True"
    return (Boolean True)
    <|> do
      symbol "False"
      return (Boolean False)
    <|> do
      symbol "Not"
      Not <$> bExp
    <|> do
      symbol "("
      b <- bExp
      symbol ")"
      return b
    <|> do 
        a1 <- aExp 
        do
            symbol "<"
            a2 <- aExp 
            return (Lt a1 a2)
            <|> do
              symbol ">"
              a2 <- aExp 
              return (Gt a1 a2)
            <|> do
              symbol "<="
              a2 <- aExp 
              return (Lte a1 a2)
            <|> do
              symbol ">="
              a2 <- aExp 
              return (Gte a1 a2)
            <|> do
              symbol "=="
              a2 <- aExp 
              return (Eq a1 a2)
            <|> do
              symbol "!="
              a2 <- aExp 
              return (Neq a1 a2)
    <|> (Bvar <$> identifier) 

-- COMMAND EXPRESION----

command :: Parser Command
command =
  arithDeclare
    <|> boolDeclare 
    <|> arrDeclare 
    <|> arithAssign
    <|> boolAssign 
    <|> arrAssign 
    <|> ifElse
    <|> whiledo
    <|> skip 

program :: Parser [Command]         
program =
  do many command                   
arithDeclare :: Parser Command
arithDeclare =
  do
    symbol "int"              
    i <- identifier
    symbol "="
    r <- ArithDeclare i <$> aExp      
    symbol ";"
    return r

boolDeclare :: Parser Command
boolDeclare =
  do
    symbol "bool"            
    i <- identifier
    symbol "="
    r <- BoolDeclare i <$> bExp
    symbol ";"
    return r

arrDeclare  :: Parser Command
arrDeclare  =
  do
    symbol "arr"                
    i <- identifier
    symbol "["
    j <- aExp 
    symbol "]"
    symbol ";"
    return (ArrDeclare i j)  
      


arithAssign :: Parser Command
arithAssign =
  do
    i <- identifier
    symbol "="
    r <- ArithAssign i <$> aExp 
    symbol ";"
    return r
    
boolAssign  :: Parser Command
boolAssign  =
  do
    i <- identifier
    symbol "="
    r <- BoolAssign  i <$> bExp
    symbol ";"
    return r

arrAssign  :: Parser Command
arrAssign  =
  do
    i <- identifier  
    do           
      symbol "["
      j <- aExp 
      symbol "]"
      symbol "="
      r <- ArrAssign  i j <$> aExp 
      symbol ";"
      return r
      <|>
        do 
          symbol "="
          symbol "["
          i' <- aExp
          i'' <- many (do symbol ","; aExp)
          symbol "]"
          symbol ";"
          return (ArrFullAssign i (Array (i':i'')))
      <|>
        do 
          symbol "["
          symbol "]"
          symbol "="
          x <- identifier
          symbol "["
          symbol "]"
          symbol ";"
          return (ArrFullAssign i (ArrayVariable x)) 

skip  :: Parser Command
skip  =
  do
    symbol "skip"
    symbol ";"
    return Skip 

ifElse :: Parser Command
ifElse =
  do
    symbol "if"
    symbol "("
    b <- bExp
    symbol ")"
    symbol "{"
    thenP <- program
    symbol "}"
    do
      symbol "else"
      symbol "{"
      elseP <- program
      symbol "}"
      return (IfElse b thenP elseP)
      <|> do
        return (IfElse b thenP [Skip])

whiledo :: Parser Command
whiledo =
  do
    symbol "whiledo"
    symbol "("
    b <- bExp
    symbol ")"
    symbol "{"
    p <- program
    symbol "}"
    return (Whiledo b p)


-- MAIN PARSE FUNCTIONS

cparse :: String -> ([Command], String)
cparse s = case p s of
  [] -> ([], "")
  [(c, s)] -> (c, s)
  where
    (P p) = program


parseFailed :: ([Command], String) -> Bool
parseFailed (_, "") = False
parseFailed (_, _) = True

getParsedCommands :: ([Command], String) -> [Command]
getParsedCommands (c, _) = c

getRemainingInput :: ([Command], String) -> String
getRemainingInput (_, s) = s