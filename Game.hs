import Bin
import Cmd
import Parser

import System.IO
import System.Random
import Data.Maybe (isJust)
import Control.Concurrent (threadDelay)


-- generate a random Item (Rock, Baby, Spider, or Nothing) with 50% probability of Nothing
randomItem :: IO (Maybe Item)
randomItem = do
    randomNumber <- randomIO :: IO Int
    if randomNumber `mod` 2 == 0 -- First choose between Nothing or Just an item
        then return Nothing
        else do -- Then choose among the 3 possible items
            let n = randomNumber `mod` 3
            case n of
                0 -> return (Just Rock)
                1 -> return (Just Baby)
                _ -> return (Just Spider)

generateTree :: Int -> IO (Bin Item)
generateTree 0 = do
    item <- randomItem
    return (Leaf item)
generateTree depth = do
    item <- randomItem
    leftSubtree <- generateTree (depth - 1)
    rightSubtree <- generateTree (depth - 1)
    return (Node item leftSubtree rightSubtree)




prettyPrintBin :: Show a => Int -> Bin a -> String
prettyPrintBin maxDepth bin = prettyPrintBin' 0 "" True bin
  where
    prettyPrintBin' currentDepth prefix isTail bin =
        case bin of
            Leaf Nothing ->
                prefix ++ "└── Empty\n"
            Leaf (Just x) ->
                prefix ++ "└── " ++ show x ++ "\n"
            Node Nothing left right ->
                if currentDepth == (maxDepth - 1) 
                then prefix ++ "└── " ++ "Empty" ++ "..." ++ "\n"
                else prefix ++ "└── " ++ "Empty" ++ "\n" ++
                    prettyPrintBin' (currentDepth + 1) (prefix ++ (if isTail then "    " else "│   ")) True left ++
                    prettyPrintBin' (currentDepth + 1) (prefix ++ (if isTail then "    " else "│   ")) False right 
            Node (Just x) left right ->
                if currentDepth == (maxDepth - 1) 
                then prefix ++ "└── " ++ show x ++ "..." ++ "\n"
                else prefix ++ "└── " ++ show x ++ "\n" ++
                    prettyPrintBin' (currentDepth + 1) (prefix ++ (if isTail then "    " else "│   ")) True left ++
                    prettyPrintBin' (currentDepth + 1) (prefix ++ (if isTail then "    " else "│   ")) False right 
            


-- the top-level interactive loop
repl :: IO ()
repl = do
    putStrLn "Welcome to the Hive!\n"
    putStrLn "You must kill spiders and feed the babies."
    tree <- generateTree 5
    go (Hole, tree)  
  where
    go :: BinZip Item -> IO ()
    go z = do
        putStrLn (prettyPrintBin 3 (snd z))
        case z of
            (_, Leaf Nothing) -> putStrLn "You see an empty leaf."
            (_, Leaf (Just item)) -> putStrLn $ "You see a leaf with a " ++ show item
            (_, Node Nothing _ _) -> putStrLn "You see an empty binary node"
            (_, Node (Just item) _ _) -> putStrLn $ "You see a binary node with a " ++ show item
            
        putStr "> "
        hFlush stdout
        line <- getLine                                  -- get a line of input
        case parseInput parseCmd line of       
            
            Nothing -> do
                putStrLn "I'm sorry, I do not understand."
                go z

            Just Place_Flag -> do
                putStrLn "Not implemented"
                go z
            
            Just Go_Flag -> do
                putStrLn "Not implemented"
                go z

            Just Go_Left ->
                case z of
                    (c, Node _ t1 t2) -> go (B0 c t2, t1)
                    (c, Leaf _) -> do
                        putStrLn "You cannot climb any further."
                        go z

            --
            --Node (Nothing) (Leaf (Just Rock)) (Node (Just Spider) (Leaf (Nothing)) (Leaf (Just Baby)))
            --


            Just Go_Right ->
                case z of
                    (c, Node item t1 t2) -> go (B1 t1 c, t2)
                    (c, Leaf _) -> do
                        putStrLn "You cannot climb any further."
                        go z

            Just Go_Back ->
                case z of
                    (B0 c t2, t) -> go (c, Node Nothing t t2) --we put nothing as the item because if there was something it's already taken
                    (B1 t1 c, t) -> go (c, Node Nothing t1 t)
                    (Hole, _) -> do
                        putStrLn "You are already at the root."
                        putStrLn "You cannot go back any further."
                        go z



        
            Just Do_Collect -> do
                result <- do_collect (snd z)
                case result of
                    Just newTree -> go (fst z, newTree)
                    Nothing -> go z


            Just Do_Feed -> do
                result <- do_feed (snd z)
                case result of
                    Just newTree -> go (fst z, newTree) 
                    Nothing -> go z


            Just Do_Shoot -> do
                result <- do_shoot (snd z)
                case result of
                    Just newTree -> go (fst z, newTree) 
                    Nothing -> go z
                

            Just Quit -> do
                putStrLn "Okay."
                putStrLn "You ended the game over here:\n"
                --putStrLn (drawBinZip z)
                putStrLn "Goodbye."
                return ()





main = repl


{-prettyPrintBin :: Show a => Int -> Bin a -> String
prettyPrintBin maxDepth bin = prettyPrintBin' 0 "" True bin
  where
    prettyPrintBin' currentDepth prefix isTail bin =

        case bin of
            Leaf Nothing ->
                prefix ++ "└── Empty\n"
            Leaf (Just x) ->
                prefix ++ "└── " ++ show x ++ "\n"
            Node Nothing left right ->
                prefix ++ "└── Empty " ++ if (currentDepth == (maxDepth - 1)) then "..." else "" ++ "\n" ++ 
                prettyPrintBin' (currentDepth + 1) (prefix ++ (if isTail then "    " else "│   ")) True left ++
                prettyPrintBin' (currentDepth + 1) (prefix ++ (if isTail then "    " else "│   ")) False right
            Node (Just x) left right ->
                prefix ++ "└── " ++ show x ++ if (currentDepth == (maxDepth - 1)) then "..." else "" ++ "\n" ++
                prettyPrintBin' (currentDepth + 1) (prefix ++ (if isTail then "    " else "│   ")) True left ++
                prettyPrintBin' (currentDepth + 1) (prefix ++ (if isTail then "    " else "│   ")) False right

    hasDeeperLevels (Node _ _ _) = True
    hasDeeperLevels (Leaf _) = False-}





{-main :: IO ()
main = do
  let tree1 =
        Node (Just 1)
          (Node (Just 2) (Leaf (Just 3)) (Node Nothing (Leaf Nothing) (Leaf (Just 5))))
          (Node (Just 4) (Leaf (Just 5)) (Node (Just 6) (Leaf (Just 7)) (Leaf (Just 8))))
    
  tree2 <- generateTree 2

  --putStrLn (prettyPrintBin 3 tree1)
  --putStrLn "\n\n"
  putStrLn (prettyPrintBin 3 tree2)-}