data Item = Nothing | Rock | Spider | Baby 
    deriving (Show, Eq)
data Bin = Leaf Item | Node Item Bin Bin 
    deriving (Show, Eq)
