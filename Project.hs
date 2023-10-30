data item = Nothing | Rock | Spider | Baby deriving (Show)
data Bin item = Leaf item | Node item (Bin item) (Bin item) deriving (Show)
