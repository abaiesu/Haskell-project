{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import System.IO
import System.Random

module Bin where

data Item = Rock | Spider | Baby | NonExistant
    deriving (Show, Eq)

data Bin a = Leaf (Maybe a) | Node (Maybe a) (Bin a) (Bin a)
    deriving (Show, Eq)
    
data BinCxt a = Hole | B0 (BinCxt a) (Bin a) | B1 (Bin a) (BinCxt a)
    deriving (Show, Eq)

data UpdatableFlag a = NoFlag | Flag (BinCxt a)
    deriving (Show, Eq)

type BinZip a = (BinCxt a, Bin a)


-- debugger function : prints all the items found in a tree
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

go_back :: BinZip Item -> BinZip Item
go_back = undefined



-- generate a random Item (Rock, Baby, Spider, or Nothing) 
-- 1/2 proba of Nothing, 1/6 proba for Rock, 1/6 proba for Baby, 1/6 for Spider
randomItem :: IO (Maybe Item)
randomItem = do
    randomNumber <- randomIO :: IO Int
    if randomNumber `mod` 2 == 0 -- first choose between Nothing or Just an item
        then return Nothing
        else do -- then choose among the 3 possible items
            let n = randomNumber `mod` 3
            case n of
                0 -> return (Just Rock)
                1 -> return (Just Baby)
                _ -> return (Just Spider)


-- data structure to help us build a random binary tree 
data SubtreeOption = LeftSubtree | RightSubtree | BothSubtrees

-- generated a random subtree option with equal proba each
randomSubtreeOption :: IO SubtreeOption
randomSubtreeOption = do
  randomNumber <- randomIO :: IO Int
  case randomNumber `mod` 3 of
    0 -> return LeftSubtree
    1 -> return RightSubtree
    _ -> return BothSubtrees

-- generate a random binary tree with depth n
generateTree :: Int -> IO (Bin Item)
generateTree 0 = do --depth 0 = leaf
  item <- randomItem
  return (Leaf item)
generateTree depth = do
  item <- randomItem -- pick an item
  subtreeOption <- randomSubtreeOption
  leftSubtree <- case subtreeOption of --generate a left subtree if
    LeftSubtree -> generateTree (depth - 1) -- left subtree
    BothSubtrees -> generateTree (depth - 1) -- both
    _ -> return (Leaf item) --otherwise, stop at a leaf
  rightSubtree <- case subtreeOption of --same for the right substree
    RightSubtree -> generateTree (depth - 1)
    BothSubtrees -> generateTree (depth - 1)
    _ -> return (Leaf item)
  return (Node item leftSubtree rightSubtree)







-- get the number of nodes with Nothing
countNothingNodes :: Bin Item -> Int
countNothingNodes (Leaf Nothing) = 1
countNothingNodes (Leaf _) = 0
countNothingNodes (Node Nothing left right) = 1 + countNothingNodes left + countNothingNodes right
countNothingNodes (Node _ left right) = countNothingNodes left + countNothingNodes right


{-
Populates the empty nodes/leafs with either a spider or a rock
ARG1 : tree
ARG2 : number of spots to be populated (excpeted value)
-}
populateEmptyNodes :: Bin Item -> Int -> IO (Bin Item)
populateEmptyNodes tree p = do
    let n = countNothingNodes tree
    go tree n
  where
    go (Leaf Nothing) n = do --empty Leaf
      randomNumber1 <- randomRIO (0.0, 1.0 :: Float)
      if randomNumber1 <= fromIntegral p / fromIntegral n --decide if we populate 
        then do
            randomNumber2 <- randomRIO (0, 1 :: Int) --decide with what do we populate (Rock or Spider)
            let newItem = if (randomNumber2 == 0)
                            then Just Rock
                            else Just Spider
            return (Leaf newItem)
        else return (Leaf Nothing)

    go (Node Nothing left right) n = do --empty node
      newLeft <- go left n --try to populate the right child
      newRight <- go right n --try to populate the left child
      randomNumber1 <- randomRIO (0.0, 1.0 :: Float)
      if randomNumber1 <= fromIntegral p / fromIntegral n
        then do
            randomNumber2 <- randomRIO (0, 1 :: Int)
            let newItem = if (randomNumber2 == 0)
                            then Just Rock
                            else Just Spider
            return (Node newItem newLeft newRight)
      else return (Node Nothing newLeft newRight)

    --when the node/leaf is already populated
    go (Leaf item) _ = return (Leaf item)
    go (Node item left right) n = do
      newLeft <- go left n
      newRight <- go right n
      return (Node item newLeft newRight)



--example to test that it populated well
--let t = Node Nothing (Leaf Nothing) (Node Nothing (Leaf Nothing) (Leaf Nothing))
