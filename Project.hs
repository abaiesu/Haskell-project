data Item = Rock | Spider | Baby
    deriving (Show, Eq)

data Bin = Leaf (Maybe Item) | Node (Maybe Item) Bin Bin
    deriving (Show, Eq)
    
data BinCxt a = Hole | B0 (BinCxt a) | B1 (BinCxt a)
    deriving (Show, Eq)

