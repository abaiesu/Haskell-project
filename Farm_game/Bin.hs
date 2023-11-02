{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Use camelCase" #-}
module Bin where
import System.IO ()
import System.Random ( randomIO, randomRIO )


data Thing = Rock | Crow | NonExistant
    deriving (Show, Eq)

type Item = (Bool, Maybe Thing)

data Bin a = Leaf a | Node a (Bin a) (Bin a)
    deriving (Show, Eq)

data BinCxt a = Hole | B0 (BinCxt a) (Bin a) | B1 (Bin a) (BinCxt a)
    deriving (Show, Eq)

type BinZip a = (BinCxt a, Bin a)

-- data structure to help us build a random binary tree 
data SubtreeOption = LeftSubtree | RightSubtree | BothSubtrees

-- debugger function : prints all the items found in a tree
printLabels :: Bin Item -> IO ()
printLabels (Leaf (_, Nothing)) = putStrLn "Empty Leaf"
printLabels (Leaf (_, Just thing)) = putStrLn $ "Leaf: " ++ show thing
printLabels (Node (_ ,Nothing) left right) = do
    putStrLn "Node: (No Label)"
    printLabels left
    printLabels right
printLabels (Node (_, Just thing) left right) = do
    putStrLn $ "Node: " ++ show thing
    printLabels left
    printLabels right



check_action :: Thing -> Bin Item -> Bool
check_action item (Leaf (_, Just i)) = item == i
check_action item (Node (_, Just i) _ _) = item == i
check_action _ _ = False

emptyNode :: Bin Item -> Bin Item
emptyNode (Leaf (b, _)) = Leaf (b, Nothing)
emptyNode (Node (b,_) left right) = Node (b, Nothing) left right

do_collect :: Bin Item -> IO (Maybe (Bin Item))
do_collect node = if check_action Rock node
then do
    putStrLn "Collected !"
    return (Just (emptyNode node))
else do
    putStrLn "Nothing to collect"
    return Nothing


do_shoot :: Bin Item -> IO (Maybe (Bin Item))
do_shoot node = if check_action Crow node
then do
    putStrLn "Killed !"
    return (Just (emptyNode node))
else do
    putStrLn "Nothing to kill"
    return Nothing

go_back :: BinZip Item -> BinZip Item
go_back = undefined



-- generate a random Item (Rock, Baby, Spider, or Nothing) 
-- 1/2 proba of Nothing, 1/6 proba for Rock, 1/6 proba for Baby, 1/6 for Spider
randomItem :: IO Item
randomItem = do
    randomNumber <- randomIO :: IO Int
    if even randomNumber -- first choose between Nothing or Just an item
        then return (False, Nothing)
        else do -- then choose among the 3 possible items
            let n = randomNumber `mod` 3
            case n of
                0 -> return (False, Just Rock)
                1 -> return (False, Just Crow)


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
  Leaf <$> randomItem
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
countNothingNodes (Leaf (_, Nothing)) = 1
countNothingNodes (Leaf _) = 0
countNothingNodes (Node (_, Nothing) left right) = 1 + countNothingNodes left + countNothingNodes right
countNothingNodes (Node _ left right) = countNothingNodes left + countNothingNodes right

{-
Populates the empty nodes/leafs with either a rock or crow
ARG1 : tree
ARG2 : number of spots to be populated (excpeted value)
-}
populateEmptyNodes :: Bin Item -> Int -> IO (Bin Item)
populateEmptyNodes tree p = do
    let n = countNothingNodes tree
    go tree n
  where
    go (Leaf (b, Nothing)) n = do --empty Leaf
      randomNumber1 <- randomRIO (0.0, 1.0 :: Float)
      if randomNumber1 <= fromIntegral p / fromIntegral n --decide if we populate 
        then do
            randomNumber2 <- randomRIO (0, 1 :: Int) --decide with what do we populate (Rock or Spider)
            let newItem = if randomNumber2 == 0
                            then Just Rock
                            else Just Crow
            return (Leaf (b, newItem))
        else return (Leaf (b, Nothing))

    go (Node (b, Nothing) left right) n = do --empty node
      newLeft <- go left n --try to populate the right child
      newRight <- go right n --try to populate the left child
      randomNumber1 <- randomRIO (0.0, 1.0 :: Float)
      if randomNumber1 <= fromIntegral p / fromIntegral n
        then do
            randomNumber2 <- randomRIO (0, 1 :: Int)
            let newItem = if randomNumber2 == 0
                            then Just Rock
                            else Just Crow
            return (Node (b, newItem) newLeft newRight)
      else return (Node (b, Nothing) newLeft newRight)

    --when the node/leaf is already populated
    go (Leaf item) _ = return (Leaf item)
    go (Node item left right) n = do
      newLeft <- go left n
      newRight <- go right n
      return (Node item newLeft newRight)

switchBool :: Bin Item -> IO (Bin Item)
switchBool (Leaf (b, item)) = return (Leaf (not b, item))
switchBool (Node (b, item) left right) = return (Node (not b, item) left right)

