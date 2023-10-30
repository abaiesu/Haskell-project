data Item = Rock | Spider | Baby
    deriving (Show, Eq)

data Bin a = Leaf (Maybe a) | Node (Maybe a) (Bin a) (Bin a)
    deriving (Show, Eq)
    
data BinCxt a = Hole | B0 (BinCxt a) | B1 (BinCxt a)
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