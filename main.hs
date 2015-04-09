import System.Environment
import Control.Monad
import Dumblisp.Repl
main :: IO()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ args !! 0
    otherwise -> putStrLn "Program takes 0 or 1 args"
