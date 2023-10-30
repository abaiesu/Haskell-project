data Item = Rock | Spider | Baby
    deriving (Show, Eq)
data Bin = Leaf (Maybe Item) | Node (Maybe Item) Bin Bin
    deriving (Show, Eq)
    
data BinCxt = Hole | B0 BinCxt (Bin (Maybe Item)) | B1 (Bin (Maybe Item)) BinCxt
    deriving (Show, Eq)
