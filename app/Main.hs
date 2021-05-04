import Parse (parse)
import Program (run)
import System.Exit (die)

main :: IO ()
main = do
  source <- readFile "./example.crs"
  
  program <- case parse source of
    Left message -> die message
    Right program -> return program
  
  globals <- case run program of
    Left message -> die message
    Right globals -> return globals
  
  putStrLn "congration, you done it"
  putStrLn "-----------------------"
  putStrLn (show globals)