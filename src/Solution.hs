module Solution where

data Action
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | NoOp
  | Rotate90Clockwise
  | Rotate90CounterClockwise
  | AttachManipulator Int Int
  | AttachFastWheels
  | AttachDrill

solutionString :: [Action] -> String
solutionString = concatMap actionToStr

actionToStr :: Action -> String
actionToStr MoveUp = "W"
actionToStr MoveDown = "S"
actionToStr MoveLeft = "A"
actionToStr MoveRight = "D"
actionToStr NoOp = "Z"
actionToStr Rotate90Clockwise = "E"
actionToStr Rotate90CounterClockwise = "Q"
actionToStr (AttachManipulator x y) = "B(" ++ show x ++ "," ++ show y ++ ")"
actionToStr AttachFastWheels = "F"
actionToStr AttachDrill = "L"
