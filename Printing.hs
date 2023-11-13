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
maxDepth (Leaf _) = 1
maxDepth (Node _ left right) = 1 + max (maxDepth left) (maxDepth right)

-- Fills in the given tree to get a depth n 
fill :: Bin Item -> Int -> Bin Item
fill tree 0 = tree
fill (Leaf item) n = (Node item (fill (Leaf (Just NonExistant)) (n-1)) (fill (Leaf (Just NonExistant)) (n-1)))
fill (Node item left right) n = (Node item (balanceTree left) (balanceTree right))

-- Takes in a non-balanced tree and returns a balanced version with non-existant nodes
balanceTree :: Bin Item -> Bin Item
balanceTree (Leaf item) = (Leaf item)
balanceTree (Node item left right) = (Node item (fill left n) (fill right m)) 
  where
    n = (maxDepth (Node item left right)) - (maxDepth left) - 1
    m = (maxDepth (Node item left right)) - (maxDepth right) - 1



-- Checks if a binary tree is balanced
isBalanced :: Bin Item -> Bool
isBalanced (Leaf _) = True 
isBalanced (Node _ left right) =
  let leftHeight = maxDepth left
      rightHeight = maxDepth right
  in
    abs (leftHeight - rightHeight) <= 1 && isBalanced left && isBalanced right



-- From a binary tree returns a list if lists where each inner list has the nodes present at a given level
levelLabels :: Bin a -> [[Maybe a]]
levelLabels tree = levelLabelsHelper [tree]
  where
    levelLabelsHelper :: [Bin a] -> [[Maybe a]]
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
printItem :: Maybe Item -> String
printItem (Just Rock) = "Rock"
printItem (Just Spider) = "Spider"
printItem (Just Baby) = "Baby"
printItem (Just NonExistant) = " "
printItem Nothing = "  O  "


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
prettyPrintHelper :: [[Maybe Item]] -> [[Maybe Item]] -> Maybe Item -> Int -> IO ()
prettyPrintHelper [l1, l2, l3, l4] [c1, c2] parent_item dir = do

    let expectedLengths_l = [1, 2, 4, 8]
    let actualLengths_l = map length [l1, l2, l3, l4]

    let expectedLengths_c = [1, 2]
    let actualLengths_c = map length [c1, c2]

    if expectedLengths_l /= actualLengths_l && expectedLengths_c /= actualLengths_c
    then putStrLn "Error: The provided list isn't a valid tree"
    else do 

        -- FURTHER CHILDREN
        putStr $ replicate 6 ' '
        putStr " "
        putStr $ concatMap (\i -> if l4 !! i == (Just NonExistant) then " " else "\\/" ++ replicate 12 ' ') [0, 2, 4, 6]
        putStrLn ""

        ------- GRANDCHILDREN 

        -- 4      5        6       7
        putStr $ replicate 6 ' '
        putStr $ unwords (map (\x -> printItem x ++ replicate 8 ' ') (take 4 l3)) ++ "\n"

        -- 4      5        6       7
        -- |      |        |       |
        putStr $ replicate 6 ' '
        putStr "  "
        putStr $ unwords [if x == (Just NonExistant) then " " else "|" ++ replicate 12 ' ' | x <- l3] ++ "\n"
        
        -- 4      5        6       7
        -- |      |        |       |
        -- --------        ---------
        putStr $ replicate 6 ' '
        putStr $ if l3 !!  0 == (Just NonExistant) then "     " else "  +------" --first half of first line
        putStr $ if l3 !!  1 == (Just NonExistant) then "     " else "-------+" ++ replicate 13 ' ' --second half of first line
        putStr $ if l3 !!  2 == (Just NonExistant) then "     " else "+------" --first half of second line
        putStr $ if l3 !!  3 == (Just NonExistant) then "     " else "-------+" --second half of second line
        putStrLn ""

        -- 4      5        6       7
        -- |      |        |       |
        -- --------        ---------
        --     |               |
        putStr $ replicate 6 ' '
        putStr $ replicate 9 ' '
        if l3 !! 0 ==  (Just NonExistant) then putStr " " else putStr "|"
        putStr $ replicate 25 ' '
        if l3 !! 2 ==  (Just NonExistant) then putStr " " else putStr "|"
        putStrLn ""


        ---------- CHILDREN -------------------

        -- 4      5        6       7
        -- |      |        |       |
        -- --------        ---------
        --     |               |
        --     2               3
        putStr $ replicate 6 ' '
        putStr $ replicate 6 ' ' ++ printItem (l2 !! 0) ++ replicate 23 ' ' --space first item space
        putStr $ printItem (l2 !! 1)  --second item
        putStrLn ""

        -- 4      5        6       7
        -- |      |        |       |
        -- --------        ---------
        --     |               |
        --     2               3
        --     |               |
        putStr $ replicate 6 ' '
        putStr $ replicate 9 ' '
        putStr $ if l2 !!  0 == (Just NonExistant) then " " else "|"
        putStr $ replicate 25 ' '
        putStr $ if l2 !! 1 == (Just NonExistant) then " " else "|" 
        putStrLn ""

        -- 4      5        6       7
        -- |      |        |       |
        -- --------        ---------
        --     |               |
        --     2               3
        --     |               |
        --     -----------------
        putStr $ replicate 6 ' '
        putStr $ replicate 9 ' '
        putStr $ if l2 !!  0 == (Just NonExistant) then "               " else "+-------------" --first half
        putStr $ if l2 !!  1 == (Just NonExistant) then "               " else "------------+"  --second half
        putStrLn ""
        
        if dir == 0 then do
            -- 4      5        6       7
            -- |      |        |       |
            -- --------        ---------
            --     |               |
            --     2               3
            --     -----------------
            --             |                     \/
            putStr $ replicate 6 ' '
            putStr $ replicate 23 ' ' ++ ['|'] 
            putStr $ replicate 25 ' '
            putStr $ if c2 !!  1 == (Just NonExistant) then " " else "\\/"
            putStrLn ""

            --------- ROOT 

            -- 4      5        6       7
            -- |      |        |       |
            -- --------        ---------
            --     |               |
            --     2               3
            --     -----------------
            --             |                  \/
            --             1                   9
            putStr $ replicate 6 ' '
            putStr $ replicate 22 ' ' ++printItem (l1 !! 0) 
            putStr $ replicate 22 ' ' ++ printItem (c1 !! 0) 
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
            putStr $ replicate 19 ' ' 
            setSGR [SetColor Foreground Vivid Red]
            putStr "YOU " 
            setSGR [Reset] 
            putStr ("|" ++ replicate 26 ' ' ++ "|")
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
            --                      Empty
            putStr $ replicate 6 ' '
            putStr $ replicate 36 ' ' ++ ['|']
            putStrLn ""
            putStr $ replicate 40 ' ' 
            putStr $ printItem parent_item
            putStrLn ""

        else if dir == 1 then do
            --      4      5        6       7
            --      |      |        |       |
            --      --------        ---------
            --          |               |
            --          2               3
            --          -----------------
            -- \/               |                     
            putStr $ replicate 3 ' '
            putStr $ if c2 !!  1 == (Just NonExistant) then " " else "\\/"
            putStr $ replicate 23 ' ' ++ ['|'] 
            putStrLn ""

            --------- ROOT 

            --      4      5        6       7
            --      |      |        |       |
            --      --------        ---------
            --          |               |
            --          2               3
            --          -----------------
            -- \/               |    
            --  9               1               
            putStr $ replicate 2 ' '
            putStr $ printItem (c1 !! 0) 
            putStr $ replicate 20 ' ' ++printItem (l1 !! 0) 
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
            putStr $ replicate 4 ' '
            putStr "|"
            putStr $ replicate 19 ' ' 
            setSGR [SetColor Foreground Vivid Red]
            putStr "YOU " 
            setSGR [Reset] 
            putStr ("|" ++ replicate 26 ' ')
            putStrLn ""
            putStr $ replicate 4 ' ' ++ "+-----------------------+"
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

        else do
            --      4      5        6       7
            --      |      |        |       |
            --      --------        ---------
            --          |               |
            --          2               3
            --          -----------------
            --                  |                     
            putStr $ replicate 6 ' '
            putStr $ replicate 23 ' ' ++ ['|'] 
            putStrLn ""

            --------- ROOT 

            --      4      5        6       7
            --      |      |        |       |
            --      --------        ---------
            --          |               |
            --          2               3
            --          -----------------
            --                  |    
            --                  1 
            --              YOU |              
            putStr $ replicate 6 ' '
            putStr $ replicate 22 ' ' ++ printItem (l1 !! 0) 
            putStrLn ""
            putStr $ replicate 29 ' '
            putStr "|"
            putStrLn ""
            setSGR [SetColor Foreground Vivid Red]
            putStr $ replicate 28 ' '
            putStr "YOU"
            setSGR [Reset]
            putStrLn "" 

            



prettyPrint :: Bin Item -> IO() --might change the type signature to BinZip
prettyPrint = undefined
{-
step 1 : trim the tree to depth 4 (we need to get root, children, grandchildren, greatgrandchildren ONLY)
step 2 : balance the tree
step 3 : get the list of the tree = l 
step 4 : get the list with the sibling and the children of the sibling = c
step 5 : get the item at the parant of the current tree = it
step 6 : fuigure out if the sibling goes left or right = dir
step 6 : call prettyPrintHelper l c it dir
-}


-- TEST
main :: IO ()
main = do
    let lists = [ [(Just Rock)], [(Just Rock), Nothing], [Nothing, Nothing, (Just Rock) , (Just Rock)], [(Just Rock), (Just NonExistant), (Just NonExistant), (Just NonExistant), (Just NonExistant), (Just NonExistant), (Just NonExistant), (Just NonExistant)]]
    
    let cs = [[(Just Rock)], [Nothing, Nothing]]

    prettyPrintHelper lists cs (Just Rock) 2