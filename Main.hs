module Main where
import DataStructure
import Interpreter
import Grammer
import Parser

-- our options
-- 1) run program all in one line.
-- 2) exit

-- type of actions that return a string
-- IO String
main :: IO ()
main = do
      putStrLn . unlines $ map concatNums choices
      choice <- getLine
      case validate choice of
         Just n  -> execute . read $ choice           
         Nothing -> putStrLn "Please try again"       

      main
   where concatNums (i, (s, _)) = show i ++ ".) " ++ s   

-- validate the input data
validate :: String -> Maybe Int                           
validate s = isValid (reads s)
   where isValid []            = Nothing
         isValid ((n, _):_) 
               | outOfBounds n = Nothing
               | otherwise     = Just n
         outOfBounds n = (n < 1) || (n > length choices)

choices :: [(Int, (String, IO ()))]                     
choices = zip [1, 2] [
   ("Run Program", run)
 , ("Exit", esc )
 ]

execute :: Int -> IO ()                                 
execute n = doExec $ filter (\(i, _) -> i == n) choices
   where doExec ((_, (_,f)):_) = f

-- run program in line
run = 
  do
  putStrLn "Write a program all in one line:"
  string <- getLine;
  let c = cparse string
  if parseFailed c
     then do
      putStr "\nParsing failed\n"
      putStr "\nRemaining input:"
      print (getRemainingInput c)
  else do
      putStrLn "\nParsing success!\n"
      let s = execProgr [] (getParsedCommands c)
      putStr "\nInput Program\n"
      putStr string
      putStr "\nRepresentation of the program:\n"
      print (getParsedCommands c)
      putStr "\nState of the memory:\n"
      print s 

-- quit interpeter
esc = 
  do 
    error "Exit"
      