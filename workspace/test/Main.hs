module Main where

import Test.Hspec.Runner
import Test.Hspec.Formatters.Codewars (codewars)

import qualified Spec

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Just codewars} Spec.spec
