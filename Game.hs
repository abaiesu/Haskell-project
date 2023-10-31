import Bin
import Cmd
import Parser

import System.IO
import Control.Concurrent (threadDelay)

-- the top-level interactive loop
repl :: IO ()
repl = do
    putStrLn "Welcome to the Hive!\n"
    putStrLn "You must kill spiders and feed the babies."
    go (Hole, test_tree2)  -- Assuming you have defined test_tree2 correctly
  where
    go :: BinZip Item -> IO ()
    go z = do
        case z of
            (Hole, Leaf Nothing) -> putStrLn "You see an empty leaf."
            (Hole, Leaf (Just item)) -> putStrLn $ "You see a leaf with a " ++ show item
            (Hole, Node Nothing _ _) -> putStrLn "You see an empty binary node"
            (Hole, Node (Just item) _ _) -> putStrLn $ "You see a binary node with a " ++ show item
            
        putStr "> "
        hFlush stdout
        line <- getLine                                  -- get a line of input
        putStrLn $ "Parsed input: " ++ line
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
                putStrLn "Not implemented"
                {--case z of
                    (c, Node item t1 t2) -> go (B0 c t2, t1)
                    (c, Leaf _) -> do
                        putStrLn "You cannot climb any further."
                        go z
                --}


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


test_tree2 = Node (Nothing) (Leaf (Just Rock)) (Node (Just Spider) (Leaf (Nothing)) (Leaf (Just Baby)))

main = repl
