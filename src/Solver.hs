module Solver where

import ProblemDesc
import Solution

solve :: ProblemDesc -> [Action]
solve prob = [MoveUp, MoveDown, Rotate90Clockwise]
