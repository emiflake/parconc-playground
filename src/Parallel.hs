module Parallel where

import Control.Applicative
import Control.Parallel.Strategies (rseq, rpar, Eval, runEval)
import Sudoku
import System.Environment
import Data.Maybe
import Control.Exception

parMap :: (a -> b) -> [a] -> Eval [b]
parMap _ [] = pure []
parMap f (x:xs) = liftA2 (:) (rpar (f x)) (parMap f xs)