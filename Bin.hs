{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Bin where

data Item = Rock | Spider | Baby
    deriving (Show, Eq)

data Bin a = Leaf (Maybe a) | Node (Maybe a) (Bin a) (Bin a)
    deriving (Show, Eq)
    
data BinCxt a = Hole | B0 (BinCxt a) (Bin a) | B1 (Bin a) (BinCxt a)
    deriving (Show, Eq)

data UpdatableFlag a = NoFlag | Flag (BinCxt a)
    deriving (Show, Eq)

type BinZip a = (BinCxt a, Bin a)


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

{-plug :: BinCxt a -> Bin a -> Bin a
plug Hole t = t
plug (B0 c x) t = Node x t (plug c t)
plug (B1 x c) t = Node x (plug c t) t-}

{-go_left :: BinZip a -> Maybe (BinZip a)
go_left (c, Node x t1 t2) = Just (B0 c x, t1)  -- Focus on the left child
go_left (c, Leaf _) = Nothing  -- Leaf has no left child

go_right :: BinZip a -> Maybe (BinZip a)
go_right (c, Node x t1 t2) = Just (B1 x c, t2)  -- Focus on the right child
go_right (c, Leaf _) = Nothing  -- Leaf has no right child

go_down :: BinZip a -> Maybe (BinZip a)
go_down (B0 c x, t) = Just (c, Node x t (Leaf Nothing))  -- Focus on parent from the left child
go_down (B1 x c, t) = Just (c, Node x (Leaf Nothing) t)  -- Focus on parent from the right child
go_down (Hole, _) = Nothing  -- Root has no parent-}


check_action :: Item -> Bin Item -> Bool
check_action item (Leaf (Just i)) = item == i
check_action item (Node (Just i) _ _) = item == i
check_action _ _ = False

emptyNode :: Bin Item -> Bin Item
emptyNode (Leaf _) = Leaf Nothing
emptyNode (Node _ left right) = Node Nothing left right

do_collect :: Bin Item -> IO (Maybe (Bin Item))
do_collect node = do
    if check_action Rock node
    then do
        putStrLn "Collected !"
        return (Just (emptyNode node))
    else do
        putStrLn "Nothing to collect"
        return Nothing


do_shoot :: Bin Item -> IO (Maybe (Bin Item))
do_shoot node = do
    if check_action Spider node
    then do
        putStrLn "Killed !"
        return (Just (emptyNode node))
    else do
        putStrLn "Nothing to kill"
        return Nothing


do_feed :: Bin Item -> IO (Maybe (Bin Item))
do_feed node = do
    if check_action Spider node
    then do
        putStrLn "Fed !"
        return (Just (emptyNode node))
    else do
        putStrLn "Nothing to feed"
        return Nothing












