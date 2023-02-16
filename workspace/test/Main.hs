module Main (main) where

import qualified Spec
import System.Exit
import Test.Hspec.Core.Formatters.V2 (formatException, formatterToFormat)
import Test.Hspec.Core.Util (safeTry)
import Test.Hspec.Formatters.Codewars (escapeLF, newFormatter)
import Test.Hspec.Runner

main :: IO ()
main = do
  codewars <- newFormatter
  summary <- safeTry $ runSpec Spec.spec defaultConfig {configFormat = Just $ formatterToFormat codewars}
  case summary of
    Left ex -> do
      putStrLn $ "\n<ERROR::>Test suite crashed<:LF:>" ++ (escapeLF $ formatException ex)
      exitFailure
    Right s -> evaluateSummary s
