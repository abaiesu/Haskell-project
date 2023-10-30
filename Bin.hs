data Item = Rock | Spider | Baby
    deriving (Show, Eq)

data Bin a = Leaf (Maybe a) | Node (Maybe a) (Bin a) (Bin a)
    deriving (Show, Eq)
    
data BinCxt a = Hole | B0 (BinCxt a) | B1 (BinCxt a)
    deriving (Show, Eq)

data UpdatableFlag a = NoFlag | Flag (BinCxt a)
    deriving (Show, Eq)


-- This will print all the items found on the tree 
printLabels :: Bin Item -> IO ()
printLabels (Leaf Nothing) = putStrLn "Empty Leaf"
printLabels (Leaf (Just item)) = putStrLn $ "Leaf: " ++ show item
printLabels (Node Nothing left right) = do
    putStrLn "Node: (No Label)"
    printLabels left
    printLabels right
printLabels (Node (Just item) left right) = do
    putStrLn $ "Node: " ++ show item
    printLabels left
    printLabels right

--test_tree1 = Node (Just Rock) (Leaf (Just Spider)) (Leaf (Just Baby)) 
--test_tree2 = Node (Nothing) (Leaf (Just Rock)) (Node (Just Spider) (Leaf (Nothing)) (Leaf (Just Spider)))

plug :: BinCxt -> Bin -> Bin
plug Hole      t = t
plug (B0 c t2) t = plug c (B t t2)
plug (B1 t1 c) t = plug c (B t1 t)


type BinZip = (BinCxt,Bin)

go_left :: BinZip -> Maybe BinZip
go_left (c,B t1 t2) = Just (B0 c t2,t1)  -- focus on the left child
go_left (c,L)       = Nothing            -- (leaf => no left child)

go_right :: BinZip -> Maybe BinZip
go_right (c,B t1 t2) = Just (B1 t1 c,t2) -- focus on the right child
go_right (c,L)       = Nothing           -- (leaf => no right child)

go_down :: BinZip -> Maybe BinZip
go_down (B0 c t2,t) = Just (c,B t t2)    -- focus on parent *from* left child
go_down (B1 t1 c,t) = Just (c,B t1 t)    -- focus on parent *from* right child
go_down (Hole,t)    = Nothing            -- (root => no parent)


runGame :: SelectMonad m => (Board -> m (Maybe Cell)) -> (Board -> m (Maybe Cell)) -> Board -> m Board
let flag = NoFlag
runGame = undefined