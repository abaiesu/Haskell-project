data Item = Rock | Spider | Baby 
    deriving (Show, Eq)
data Bin (Maybe Item) = Leaf (Maybe Item) | Node (Maybe Item) (Bin (Maybe Item)) (Bin (Maybe Item))
    deriving (Show, Eq)
