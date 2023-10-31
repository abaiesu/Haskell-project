import Bin
import Cmd
import Parser

import System.IO
import Control.Concurrent (threadDelay)

-- Define a variable to keep track of the number of rocks collected
data GameState = GameState {
    binZip :: BinZip Item,
    rocksCollected :: Int,
    spidersKilled :: Int,
    points :: Int
}

initialState :: BinZip Item
initialState = (Hole, test_tree2)

initialGame :: GameState
initialGame = GameState {
    binZip = initialState,
    rocksCollected = 0,
    spidersKilled = 0,
    points = 0
}

-- the top-level interactive loop
repl :: GameState -> IO ()
repl game = do
    putStrLn "Welcome to the Hive!\n"
    putStrLn "You must kill spiders and feed the babies."
    go game  -- Assuming you have defined test_tree2 correctly
  where
    go :: GameState -> IO ()
    go gameState = do
        let z = binZip gameState
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
                go gameState

            Just Place_Flag -> do
                putStrLn "Not implemented"
                go gameState
            
            Just Go_Flag -> do
                putStrLn "Not implemented"
                go gameState

            Just Go_Left ->
                case z of
                    (c, Node _ t1 t2) -> do
                        let newGameState = gameState { binZip = (B0 c t2, t1) }
                        go newGameState
                    (c, Leaf _) -> do
                        putStrLn "You cannot climb any further."
                        go gameState

            --
            --Node (Nothing) (Leaf (Just Rock)) (Node (Just Spider) (Leaf (Nothing)) (Leaf (Just Baby)))
            --


            Just Go_Right ->
                case z of
                    (c, Node item t1 t2) -> 
                        do
                        let newGameState = gameState { binZip = (B1 t1 c, t2)}
                        go newGameState
                    (c, Leaf _) -> do
                        putStrLn "You cannot climb any further."
                        go gameState

            Just Go_Back ->
                case z of
                    (B0 c t2, t) -> do
                        let newGameState = gameState { binZip = (c, Node Nothing t t2) }
                        go newGameState  --we put nothing as the item because if there was something it's already taken
                    (B1 t1 c, t) -> do
                        let newGameState = gameState { binZip = (c, Node Nothing t1 t)}
                        go newGameState 
                    (Hole, _) -> do
                        putStrLn "You are already at the root."
                        putStrLn "You cannot go back any further."
                        go gameState



        
            Just Do_Collect -> do
                if rocksCollected gameState < 3
                    then do
                        result <- do_collect (snd z)
                        case result of
                            Just newTree -> do
                                let newGameState = gameState { binZip = (fst z, newTree), rocksCollected = rocksCollected gameState + 1 }
                                go newGameState
                            Nothing -> go gameState
                    else do
                        putStrLn "You cannot collect more rocks. Limit reached."
                        go gameState


            Just Do_Feed -> do
                if spidersKilled gameState > 0
                    then do
                        result <- do_feed (snd z)
                        case result of
                            Just newTree -> do
                                let newGameState = gameState { binZip = (fst z, newTree), spidersKilled = spidersKilled gameState -1, points= points gameState +1 }
                                go newGameState
                            Nothing -> go gameState
                    else do
                        putStrLn "You cannot feed unless you've killed spiders."
                        go gameState


            Just Do_Shoot -> do
                result <- do_shoot (snd z)
                case result of
                    Just newTree -> do
                                let newGameState = gameState { binZip = (fst z, newTree), rocksCollected = rocksCollected gameState-1, spidersKilled = spidersKilled gameState +1 }
                                go newGameState
                    Nothing -> go gameState

                

            Just Quit -> do
                putStrLn "Okay."
                putStrLn "You ended the game over here:\n"
                --putStrLn (drawBinZip z)
                putStrLn "Goodbye."
                return ()


test_tree2 = Node (Nothing) (Leaf (Just Rock)) (Node (Just Spider) (Leaf (Nothing)) (Leaf (Just Baby)))

main = repl initialGame
