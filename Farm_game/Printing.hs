--module Printing where
import Bin
import System.Console.ANSI

{-
Return the trimmed version of the input tree
ARG1 : tree
ARG2 : depth at which to trim
-}
trimTree :: Bin a -> Int -> Bin a
trimTree (Leaf x) _ = Leaf x
trimTree (Node x left right) depth
    | depth == 0 = Leaf x
    | otherwise   = Node x (trimTree left (depth - 1)) (trimTree right (depth - 1))

--Gets the max depth of a tree
maxDepth :: Bin Item -> Int
maxDepth (Leaf _) = 0
maxDepth (Node _ left right) = 1 + max (maxDepth left) (maxDepth right)

-- Fills in the given tree to get a depth n 
fill :: Bin Item -> Int -> Bin Item
fill tree 0 = tree
fill (Leaf item) n = Node item (fill (Leaf (False, Just NonExistant)) (n-1)) (fill (Leaf (False, Just NonExistant)) (n-1))
fill (Node item left right) n = Node item (fill left (n-1)) (fill right (n-1))

-- Takes in a non-balanced tree and returns a balanced version with non-existant nodes
balanceTree :: Bin Item -> Bin Item
balanceTree (Leaf item) = Leaf item
balanceTree (Node item left right) = Node item (fill left n) (fill right n)
  where
    n = max (maxDepth left) (maxDepth right) 



-- Checks if a binary tree is balanced
isBalanced :: Bin Item -> Bool
isBalanced (Leaf _) = True
isBalanced (Node _ left right) =
  let leftHeight = maxDepth left
      rightHeight = maxDepth right
  in
    abs (leftHeight - rightHeight) <= 1 && isBalanced left && isBalanced right



-- From a binary tree returns a list if lists where each inner list has the nodes present at a given level
levelLabels :: Bin Item -> [[Item]]
levelLabels tree = levelLabelsHelper [tree]
  where
    levelLabelsHelper :: [Bin Item] -> [[Item]]
    levelLabelsHelper [] = []
    levelLabelsHelper nodes =
      let labels = concatMap getNodeLabel nodes
          nextLevelNodes = concatMap getChildren nodes
      in labels : levelLabelsHelper nextLevelNodes

    getNodeLabel (Leaf label) = [label]
    getNodeLabel (Node label _ _) = [label]

    getChildren (Node _ left right) = [left, right]
    getChildren _ = []





--Prints items
printItem :: Maybe Thing -> String
printItem (Just Rock) = "Rock "
printItem (Just Crow) = "Crow "
printItem (Just NonExistant) = "     "
printItem Nothing = "  O  "

-- Print text in the specified color
printInColor :: Color -> String -> IO ()
printInColor color text = do
    setSGR [SetColor Foreground Dull color]  -- Set the specified text color
    putStr text
    setSGR [Reset]  -- Reset text color to default

-- Set background to specifed color and prints
setBackPrint :: Color -> String -> IO ()
setBackPrint color text = do
    setSGR [SetColor Background Dull color]  -- Set the specified text color
    putStr text
    setSGR [Reset]  -- Reset text color to default

checkIfExistant :: [Item] -> Int -> [Item] -> Int -> IO ()
checkIfExistant l3 m l4 n = do
    let result
          | snd (l3 !! m) == Just NonExistant = "  "
          | snd (l4 !! n) == Just NonExistant = "+"
          | otherwise = "\\/"
    putStr result

{-
usage : ARG1 : list of the nodes at each level [[root], [children], [grandchildren], [great grandchildren]]
        len root = 1, len children = 2, len grandchildren = 4, len great grandchildren = 8

        ARG2 : list of the nodes of the sibling of the node we are at [[root], [children]]
        len root = 1, len children = 2

        ARG3 : item found at the parent of the current node

        ARG4 : 0 = we took the left path (and have a right sibling)
               1 = we took the right path (and have a left sibling)
               2 = we are at the root and have no sibling
-}
prettyPrintHelper :: [[Item]] -> [[Item]] -> Maybe Thing-> Int -> IO ()
prettyPrintHelper [l1, l2, l3, l4] [c1, c2] parent_item dir = do

    let expectedLengths_l = [1, 2, 4, 8]
    let actualLengths_l = map length [l1, l2, l3, l4]

    let expectedLengths_c = [1, 2]
    let actualLengths_c = map length [c1, c2]

    if expectedLengths_l /= actualLengths_l && expectedLengths_c /= actualLengths_c
    then putStrLn "Error: The provided lists don't specify the requirements"
    else do

        if snd (l2 !! 1) == Just NonExistant --if one of them is non existant, then the second one too and it's just a root
        then putStr ""
        else do

          -------FURTHER NODES
          --
          --        \/
          --  -------         --------


          putStr $ replicate 7 ' '
          putStr " "
          checkIfExistant l3 0 l4 1 --check if grandchild 1 has children (0 and 1)
          if snd (head l3) /= Just NonExistant
          then printInColor Blue  "-------------" --put the field separtion
          else putStr "            " --if there is no grand child, nothing

          checkIfExistant l3 1 l4 2 --check if grandchild 2 has children (2 and 3)
          putStr $ replicate 12 ' '

          checkIfExistant l3 2 l4 4 --check if grandchild 3 has children (4 and 5)
          if snd (l3 !! 2) /= Just NonExistant
          then printInColor Blue  "-------------" --put the field separtion
          else putStr "            "

          checkIfExistant l3 3 l4 6 --check if grandchild 4 has children (6 and 7)
          putStrLn ""

          ------- GRANDCHILDREN 
          --        \/
          --  -------         --------
          -- |      |        |       |
          -- 4      5        6       7
          -- |      |        |       |
          -- --------        ---------
          --     |               |
          putStr $ replicate 8 ' '
          if snd (head l3) == Just NonExistant then putStr " " else putStr "|"
          if fst (head l3) then setBackPrint Green (replicate 13 ' ' ) else putStr $ replicate 13 ' '
          if snd (l3 !! 1) == Just NonExistant then putStr " " else putStr "|"
          putStr $ replicate 13 ' '
          if snd (l3 !! 2) == Just NonExistant then putStr " " else putStr "|"
          if fst (l3 !! 2) then setBackPrint Green (replicate 13 ' ' ) else putStr $ replicate 13 ' '
          if snd (l3 !! 3) == Just NonExistant then putStr " " else putStr "|"
          putStrLn ""


          putStr $ replicate 6 ' '
          putStr $ printItem (snd (head l3))
          if fst (head l3) then setBackPrint Green (replicate 9 ' ' ) else putStr $ replicate 9 ' '
          putStr $ printItem (snd (l3 !! 1))
          putStr $ replicate 9 ' '
          putStr $ printItem (snd (l3 !!2))
          if fst (l3 !! 2) then setBackPrint Green (replicate 9 ' ' ) else putStr $ replicate 9 ' '
          putStr $ printItem (snd (l3 !! 3))
          putStrLn ""

          putStr $ replicate 8 ' '
          if snd (head l3) == Just NonExistant then putStr " " else putStr "|"
          if fst (head l3) then setBackPrint Green (replicate 13 ' ' ) else putStr $ replicate 13 ' '
          if snd (l3 !! 1) == Just NonExistant then putStr " " else putStr "|"
          putStr $ replicate 13 ' '
          if snd (l3 !! 2) == Just NonExistant then putStr " " else putStr "|"
          if fst (l3 !! 2) then setBackPrint Green (replicate 13 ' ' ) else putStr $ replicate 13 ' '
          if snd (l3 !! 3) == Just NonExistant then putStr " " else putStr "|"
          putStrLn ""

          putStr $ replicate 6 ' '
          if snd (l3 !!  1) == Just NonExistant
          then do
             putStr $ replicate 9 ' '
             printInColor Blue "+-------"
          else do
            putStr $ if snd (head l3) == Just NonExistant then "          " else "  +------" --first half of first line
            if snd (l3 !!  1) == Just NonExistant then printInColor Blue "+------" else putStr "-------+" --second half of first line

          if snd (l3 !!  3) == Just NonExistant then printInColor Blue "-----------------+"  else printInColor Blue "-------------" -- non walkable fence
          putStr $ if snd (l3 !!  2) == Just NonExistant then "     " else "+------" --first half of second line
          putStr $ if snd (l3 !!  3) == Just NonExistant then "     " else "-------+" --second half of second line
          putStrLn ""


          ---------- CHILDREN -------------------

          --        \/
          --  -------         --------
          -- |      |        |       |
          -- 4      5        6       7
          -- |      |        |       |
          -- --------        ---------
          --     |               |
          --     2               3
          --     |               |

          putStr $ replicate 15 ' '
          if snd (head l2) == Just NonExistant
          then putStr "  "
          else if fst (head l2)
              then do
                putStr "|"
                setBackPrint Green (replicate 25 ' ')
              else putStr $ "|" ++ replicate 25 ' '

          if snd (l2 !! 1) == Just NonExistant then putStr " " else putStr "|"
          putStrLn ""


          putStr $ replicate 13 ' '
          putStr $ printItem (snd (head l2))
          if fst (head l2) then setBackPrint Green (replicate 21 ' ') else putStr $ replicate 21 ' '
          putStr $ printItem (snd (l2 !! 1))  --second item
          putStrLn ""

          putStr $ replicate 15 ' '
          if snd (head l2) == Just NonExistant
          then putStr " " -- Check that there is a node 
          else if fst (head l2)
              then do
                putStr "|"
                setBackPrint Green (replicate 25 ' ')
              else putStr $ "|" ++ replicate 25 ' '
          putStrLn "|"

        if dir == 0 then do --the sibling is on the right
            -- 4      5        6       7
            -- |      |        |       |
            -- --------        ---------
            --     |               |
            --     2               3
            --     |               |
            --     -----------------          \/
            putStr $ replicate 15 ' '
            putStr $ if snd (head l2) == Just NonExistant then "               " else "+-------------" --first half
            putStr $ if snd (l2 !!  1) == Just NonExistant then "               " else "------------+"  --second half
            -- check if the current position is a leaf
            if snd (l2 !!  1) == Just NonExistant then putStr " " else printInColor Blue "--------------" --non walkable fence
            if snd (l2 !!  1) /= Just NonExistant
            then putStr $ if snd (c2 !!  1) == Just NonExistant then  "+" else "\\/"
            else putStr ""
            putStrLn ""

            -- 4      5        6       7
            -- |      |        |       |
            -- --------        ---------
            --     |               |
            --     2               3
            --     -----------------             \/
            --             |                      |
            --             1                      9
            putStr $ replicate 6 ' '
            putStr $ replicate 23 ' ' ++ ['|']
            if fst (head l1)
            then setBackPrint Green (replicate 26 ' ')
            else putStr $ replicate 26 ' '
            putStr "|"
            putStrLn ""

            putStr $ replicate 6 ' '
            putStr $ replicate 22 ' ' ++printItem (snd (head l1))
            if fst (head l1)
            then setBackPrint Green (replicate 21 ' ')
            else putStr $ replicate 21 ' '
            putStr $ printItem (snd (head c1))
            putStrLn ""


            -- 4      5        6       7
            -- |      |        |       |
            -- --------        ---------
            --     |               |
            --     2               3
            --     -----------------
            --             |                  \/
            --             1                   9
            --             |                   |
            --             ---------------------
            putStr $ replicate 6 ' '
            putStr $ replicate 20 ' '
            printInColor Red "YOU"
            putStr "|"
            if fst (head l1)
            then setBackPrint Green (replicate 26 ' ')
            else putStr $ replicate 26 ' '
            putStr "|"
            putStrLn ""
            putStr $ replicate 6 ' '
            putStr $ replicate 23 ' ' ++ "+--------------------------+"
            putStrLn ""


            -- 4      5        6       7
            -- |      |        |       |
            -- --------        ---------
            --     |               |
            --     2               3
            --     -----------------
            --             |                  \/
            --             1                   9
            --             ---------------------
            --                        |
            --                        Item
            putStr $ replicate 6 ' '
            putStr $ replicate 36 ' ' ++ ['|']
            putStrLn ""
            putStr $ replicate 40 ' '
            putStr $ printItem parent_item
            putStrLn ""

        else if dir == 1 then do --sibling to the left

            -- 4      5        6       7
            -- |      |        |       |
            -- --------        ---------
            --     |               |
            --     2               3
            --     |               |
            -- \/  -----------------  
            if snd (head l2) == Just NonExistant
            then putStrLn ""
            else do
              putStr $ replicate 4 ' '
              putStr $ if snd (c2 !!  1) == Just NonExistant then "+" else "\\/"
              printInColor Blue "----------"
              putStr $ if snd (head l2) == Just NonExistant then "               " else "+-------------" --first half
              putStr $ if snd (l2 !!  1) == Just NonExistant then "               " else "------------+"  --second half
              putStrLn ""

            --      4      5        6       7
            --      |      |        |       |
            --      --------        ---------
            --          |               |
            --          2               3
            -- \/       -----------------
            -- |                |      
            -- 9                1                  
            putStr $ replicate 4 ' '
            putStr ['|']
            if fst (head l1)
            then setBackPrint Green (replicate 24 ' ')
            else putStr $ replicate 25 ' '
            putStr ['|']
            putStrLn ""
            putStr $ replicate 2 ' '
            putStr $ printItem ( snd (head c1))
            if fst (head l1)
            then setBackPrint Green (replicate 20 ' ') 
            else putStr $ replicate 21 ' ' 
            putStr $ printItem (snd (head l1))
            putStrLn ""



            --      4      5        6       7
            --      |      |        |       |
            --      --------        ---------
            --          |               |
            --          2               3
            --          -----------------
            -- \/               |    
            --  9          YOU  1     
            --  |               |
            --  -----------------     
            putStr $ replicate 4 ' '
            putStr "|"
            if fst (head l1)
            then setBackPrint Green (replicate 24 ' ') 
            else putStr $ replicate 25 ' ' 
            putStr "|"
            printInColor Red "YOU"
            putStrLn ""
            putStr $ replicate 4 ' ' ++ "+-------------------------+"
            putStrLn ""


            --      4      5        6       7
            --      |      |        |       |
            --      --------        ---------
            --          |               |
            --          2               3
            --          -----------------
            -- \/               |    
            --  9               1     
            --  |               |
            --  -----------------     
            --          |
            --          item
            putStr $ replicate 6 ' '
            putStr $ replicate 10 ' ' ++ "|"
            putStrLn ""
            putStr $ replicate 15 ' '
            putStr $ printItem parent_item
            putStrLn ""

        else do --no sibling

            -- 4      5        6       7
            -- |      |        |       |
            -- --------        ---------
            --     |               |
            --     2               3
            --     |               |
            --     -----------------         
            putStr $ replicate 6 ' '
            putStr $ replicate 9 ' '
            putStr $ if snd (head l2) == Just NonExistant then "               " else "+-------------" --first half
            putStr $ if snd (l2 !!  1) == Just NonExistant then "               " else "------------+"  --second half
            putStrLn ""

            --      4      5        6       7
            --      |      |        |       |
            --      --------        ---------
            --          |               |
            --          2               3
            --          -----------------
            --                  |      
            --                  1 
            --                 YOU               
            putStr $ replicate 6 ' '
            putStr $ replicate 23 ' ' ++ ['|']
            putStrLn ""
            putStr $ replicate 6 ' '
            putStr $ replicate 22 ' ' ++ printItem (snd (head l1))
            putStrLn ""
            putStr $ replicate 29 ' '
            putStr "|"
            putStrLn ""
            putStr $ replicate 28 ' '
            printInColor Red "YOU"
            putStrLn ""
prettyPrintHelper a b c d = do
  putStrLn ""

getsibling :: BinCxt Item -> Bin Item
getsibling Hole = Leaf (False, Nothing)
getsibling (B0 a b c) = c
getsibling (B1 a b c) = b

getParentThing :: BinCxt Item -> Maybe Thing
getParentThing Hole = Nothing
getParentThing (B0 (_,a) b c) = a
getParentThing (B1 (_,a) b c) = a

getdir :: BinCxt Item -> Int
getdir Hole = 2
getdir (B0 {}) = 0
getdir (B1 {}) = 1

-- Prints the current position
prettyPrint :: BinZip Item -> IO() 
prettyPrint (b,c) = do
  let trimmed = if maxDepth c >3 
                  then trimTree c 3
                  else fill c 3
  let balanced = balanceTree trimmed
  if isBalanced balanced 
    then putStr ""
    else putStrLn "Oh no not balanced"
  let l = levelLabels balanced
  if length l <4
    then putStrLn "OH NO YOU FELL IN THE RIVER! GO BACK"
    else putStr ""
  let sibtree = getsibling b
  let tst = trimTree sibtree 1
  let tst' = fill tst 1
  let btst = balanceTree tst'
  let c = levelLabels btst
  let it = getParentThing b
  let dir = getdir b
  prettyPrintHelper l c it dir

  putStrLn ""
{-
step 1 : trim the tree to depth 4 (we need to get root, children, grandchildren, greatgrandchildren ONLY)
step 2 : balance the tree
step 3 : get the list of the tree = l 
step 4 : get the list with the sibling and the children of the sibling = c
step 5 : get the item at the parant of the current tree = it
step 6 : fuigure out if the sibling goes left or right = dir
step 6 : call prettyPrintHelper l c it dir
-}


main :: IO ()
main = do
  let l = [[(False, Just Rock)], 
            [(True, Just Crow), (True, Nothing)],
            [(False, Just NonExistant), (False, Just NonExistant), (False, Nothing), (False, Nothing)],
            [(False, Nothing), (False, Nothing), (False, Just NonExistant), (False, Just NonExistant), (False, Nothing), (False, Nothing), (False, Nothing), (False, Nothing)]]
                   
  let c = [[(False, Nothing)], 
            [(False, Nothing), (False, Nothing)]]

  prettyPrintHelper l c (Just Rock) 2