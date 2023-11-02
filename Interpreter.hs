module Interpreter where
import DataStructure (declareArray, readElemArray, insertElemArray)
import Grammer 


-- Variable representation
-- name:  name of variable
-- value: actual value of variable

data Variable = Variable {name  :: String,
                          value :: Type } deriving Show

-- Env: array of tuples of type Variable

type Env = [Variable]


-- modify our env for incoming state change
-- UPDATE AND EDIT OPEATION
modifyEnv :: Env -> Variable -> Env 
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar = 
        if (name x) == (name newVar) 
                then [newVar] ++ xs 
                         else[x] ++ modifyEnv xs newVar


-- This returns the value of the variable 
-- READ OPEATION
searchVariable:: Env -> String-> Maybe Type
searchVariable [] varname = Nothing
searchVariable (x:xs) varname = if (name x) == varname
        then Just (value x)
                                else searchVariable xs
                                varname


--ARITHMETIC EXPRESSION EVALUATION--

arithExprEval:: Env -> AExp -> Maybe Int

arithExprEval env (Constant i) = Just i

arithExprEval env (Avar i) = 
        case searchVariable env i of
                Just (IntType v)-> Just v
                Just _ -> error "type mismatch found"
                Nothing -> error "undeclared variable found"

arithExprEval env (ArrayVar s i) =
        case searchVariable env s of 
                Just (ArrayType a)-> Just (readElemArray a j)
                        where Just j = arithExprEval env i
                Just _ -> error "type mismatch found"
                Nothing -> error "undeclared variable found"


-- ARITHEMETIC EXPRESSION EVALUATION WITH APPLICATIVE
arithExprEval env (Add a b) =  pure (+) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Sub a b) = pure (-) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Mul a b) = pure (*) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Div a b) = pure (div) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Power a b) = pure (^) <*> (arithExprEval env a) <*> (arithExprEval env b)



-- BOOLEAN EXPRESSION EVALUATION

boolExprEval :: Env -> BExp -> Maybe Bool

boolExprEval env (Boolean b) = Just b

boolExprEval env (Bvar s)=
        case searchVariable env s of 
                Just (BoolType v) -> Just v
                Just _ -> error "type mismatch found"
                Nothing -> error "undeclared variable found"

boolExprEval env (Lt a b) = pure (<) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Gt a b) = pure (>) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Eq a b) = pure (==) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Neq a b) = pure (/=) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Lte a b) = pure (<=) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Gte a b) = pure (>=) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (And a b) = pure (&&) <*> (boolExprEval env a) <*> (boolExprEval env b)

boolExprEval env (Or a b) = pure (||) <*> (boolExprEval env a) <*> (boolExprEval env b)

boolExprEval env (Not a) = not <$> boolExprEval env a      

-- ARRAY EXPRESSION EVALUATION

arrExprEval :: Env -> ArrayExp -> Maybe [Int]
arrExprEval e (Array a) = if hasFailed 
                                    then Nothing
                                    else Just $ map (\v -> case v of Just x -> x) r
                                      where hasFailed = or $ map (\v -> case v of
                                              Nothing -> True
                                              Just x -> False) r
                                            r = map (\exp -> arithExprEval e exp) a
arrExprEval e (ArrayVariable v) = 
  case searchVariable e v of
    Just (ArrayType a) -> Just a
    Nothing -> error "variable not found"


-- EXECUTION OF THE PROGRAM--
-- define the implementation of commands
execProgr :: Env -> [Command] -> Env

execProgr e [] = e 

execProgr e  (Skip : cs) = execProgr e cs

execProgr e ((IfElse b nc nc') : cs) =
        case boolExprEval e b of
                Just True -> execProgr e (nc ++ cs)
                Just False-> execProgr e (nc'++ cs)
                Nothing -> error "error in if statement found"


execProgr e ((Whiledo b nc) : cs) =
        case boolExprEval e b of
                Just True -> execProgr e (nc ++ [(Whiledo b nc)] ++ cs)
                Just False -> execProgr e cs
                Nothing -> error "error in while loop found"

execProgr e ((ArithAssign s a) : cs ) =
        case searchVariable e s of
                Just (IntType _ ) -> execProgr (modifyEnv e var) cs
                               where var = Variable s (IntType z)
                                        where Just z = arithExprEval e a
                Just _ -> error "type mismatch found"
                Nothing -> error "error assign found" 


execProgr e ((BoolAssign s b) : cs ) =
        case searchVariable e s of
                Just (BoolType _ ) -> execProgr (modifyEnv e var) cs
                               where var = Variable s (BoolType z)
                                        where Just z = boolExprEval e b
                Just _ -> error "type mismatch found"
                Nothing -> error "error assign found" 

execProgr e (( ArithDeclare s a ) : cs ) =
        case arithExprEval e a of
                Just exp -> case searchVariable e s of
                        Just _ -> error "double declaration error found"
                        Nothing -> execProgr (modifyEnv e var) cs
                               where var = Variable s (IntType z)
                                        where Just z = arithExprEval e a
                Nothing -> error "error in declaring found"

execProgr e (( BoolDeclare s a ) : cs ) =
        case boolExprEval e a of
                Just exp -> case searchVariable e s of
                        Just _ -> error "double declaration found"
                        Nothing -> execProgr (modifyEnv e var) cs
                               where var = Variable s (BoolType z)
                                        where Just z = boolExprEval e a
                Nothing -> error "error declare found"


-- a[3]= 3; b=[2,3,4]; a[] = b[];

execProgr e ((ArrAssign s i a) : cs ) =
        case searchVariable e s of
                Just (ArrayType x ) -> execProgr (modifyEnv e var) cs
                               where var = Variable s (ArrayType z)
                                        where z = insertElemArray x j a' 
                                                where   
                                                        Just a'= arithExprEval e a 
                                                        Just j = arithExprEval e i
                Just _ -> error "type mismatch found"
                Nothing -> error "error assign found" 


execProgr e ((ArrDeclare s a) : cs ) =
        case searchVariable e s of
                Just _ -> error "double declaration found"
                Nothing -> execProgr (modifyEnv e var) cs
                         where var = Variable s (ArrayType z)
                                 where z = declareArray j 
                                        where Just j = arithExprEval e a
                Nothing -> error "error declare found"

execProgr env ((ArrFullAssign v exp) : cs) =
  case searchVariable env v of
    Just (ArrayType a) -> case arrExprEval env exp of
                            Just b -> if length a == length  b 
                            then
                                execProgr (modifyEnv env (Variable v (ArrayType b))) cs
                            else error "invalid length found"
                            Nothing -> error "aExp evaluation of array failed"
    Nothing -> error "undeclared variable found"