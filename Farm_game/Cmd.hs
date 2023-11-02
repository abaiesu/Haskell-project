module Cmd where

data Cmd = Go_Left | Go_Right | Go_Back | Do_Shoot | Place_Flag | Go_Flag | Do_Collect | Do_Feed | Quit
  deriving (Show,Read, Eq)
