data Item = Rock | Spider | Baby
    deriving (Show, Eq)

data Bin a = Leaf (Maybe a) | Node (Maybe a) (Bin a) (Bin a)
    deriving (Show, Eq)
    
data BinCxt a = Hole | B0 (BinCxt a) | B1 (BinCxt a)
    deriving (Show, Eq)

