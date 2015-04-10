import System.Environment
import Control.Monad
import Dumblisp.Repl
main :: IO()
main = do
  args <- getArgs
  if null args then runRepl else runOne $ args
