import Dumblisp.Parser
import Dumblisp.Eval
import System.Environment
import Control.Monad
import Dumblisp.Types

main :: IO()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
