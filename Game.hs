import Bin
import Cmd
import Parser

import System.IO
import Control.Concurrent (threadDelay)

-- the top-level interactive loop
repl :: IO ()
repl = do
  putStrLn "Welcome to the Hive !\n"
  putStrLn "You are must kill spiders and feed the babies."
  go (Hole,test_tree2 )
  where
    go :: BinZip -> IO ()
    go z = do                                          -- give the player some information
    case z of                                        -- about the current position in the tree
    (_,L)     -> putStrLn "You see a leaf."
    (_,B _ _) -> putStrLn "You see a binary node."
    putStr "> "                                      -- print the prompt
    hFlush stdout                                    -- flush standard output
    line <- getLine                                  -- get a line of input
    case parseInput parseCmd line of                 -- parse the input
        Nothing -> do
        putStrLn "I'm sorry, I do not understand."
        go z

        Just Go_Left ->
        case z of
            (c,B t1 t2) -> go (B0 c t2,t1)           -- climb up to the left
            (c,L) -> do
            putStrLn "You cannot climb any further."
            go z

        Just Go_Right ->
        case z of
            (c,B t1 t2) -> go (B1 t1 c,t2)           -- climb up to the right
            (c,L) -> do
            putStrLn "You cannot climb any further."
            go z

        Just Go_Back ->
        case z of
            (B0 c t2,t) -> go (c,B t t2)             -- climb down from the left, or
            (B1 t1 c,t) -> go (c,B t1 t)             -- climb down from the right, or
            (Hole,t) -> do                           -- already at the root
            putStrLn "You are already at the root."
            putStrLn "You go back any further."
            go z

        Just Go_Back ->
            case z of
            (B0 c t2,t) -> go (c,B t t2)             -- climb down from the left, or
            (B1 t1 c,t) -> go (c,B t1 t)             -- climb down from the right, or
            (Hole,t) -> do                           -- already at the root
                putStrLn "You are already at the root."
                putStrLn "You go back any further."
                go z
        
        Just Do_Collect -> do
            result <- Do_Collect (snd z)
            case result of
                Just newTree -> go (fst z, newTree)
                Nothing -> go z


        Just Do_Feed -> do
          result <- Do_Feed (snd z)
          case result of
            Just newTree -> go (fst z, newTree) 
            Nothing -> go z


        Just Do_Shoot -> do
          result <- Do_Shoot (snd z)
          case result of
            Just newTree -> go go (fst z, newTree) 
            Nothing -> go z
            

        Just Quit -> do
            putStrLn "Okay."
            putStrLn "You ended the game over here:\n"
            putStrLn (drawBinZip z)
            putStrLn "Goodbye."
            return ()


test_tree2 = Node (Nothing) (Leaf (Just Rock)) (Node (Just Spider) (Leaf (Nothing)) (Leaf (Just Spider)))

main = repl
