data Item = Nothing | Rock | Spider | Baby 
    deriving (Show, Eq)
data Bin Item = Leaf Item | Node Item (Bin Item) (Bin Item)
    deriving (Show, Eq)
