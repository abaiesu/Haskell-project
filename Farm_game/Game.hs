import Bin
import Cmd
    ( Cmd(Quit, Place_Flag, Go_Flag, Go_Left, Go_Right, Go_Back,
          Do_Collect, Do_Shoot) )
import Parser ( parseCmd, parseInput )

import System.IO ( hFlush, stdout )
import System.Random ()
import Data.Maybe (isJust)
import Control.Concurrent (threadDelay)

-- Define a variable to keep track of the number of rocks collected
data GameState = GameState {
    binZip :: BinZip Item,
    rocksCollected :: Int,
    spidersKilled :: Int,
    points :: Int
}



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
            


main :: IO ()
main = do
  let tree1 =
        Node (Just 1)
          (Node (Just 2) (Leaf (Just 3)) (Node Nothing (Leaf Nothing) (Leaf (Just 5))))
          (Node (Just 4) (Leaf (Just 5)) (Node (Just 6) (Leaf (Just 7)) (Leaf (Just 8))))
    
  tree2 <- generateTree 2

  putStrLn (prettyPrintBin 3 tree1)
  --putStrLn "\n\n"
  --putStrLn (prettyPrintBin 3 tree2)-}





-- the top-level interactive loop
repl :: IO ()
repl= do
    putStrLn "Welcome to the Hive!\n"
    putStrLn "You must kill spiders and feed the babies."
    tree <- generateTree 5
    let game = GameState {
      binZip = (Hole, tree),
      rocksCollected = 0,
      spidersKilled = 0,
      points = 0
    }
    go game
  where
    go :: GameState -> IO ()
    go gameState = do
        let (cxt, t) = binZip gameState
        q <- genCropCxt cxt 0.01
        let z = (q,t)
        --putStrLn (prettyPrintBin 3 (snd z))
        case z of
            (_, Leaf (_, Nothing)) -> putStrLn "You see an empty leaf."
            (_, Leaf (_, Just item)) -> putStrLn $ "You see a leaf with a " ++ show item
            (_, Node (_, Nothing) _ _) -> putStrLn "You see an empty binary node"
            (_, Node (_, Just item) _ _) -> putStrLn $ "You see a binary node with a " ++ show item

        putStr "> "
        hFlush stdout
        line <- getLine                                  -- get a line of input
        case parseInput parseCmd line of

            Nothing -> do
                putStrLn "I'm sorry, I do not understand."
                go gameState

            Just Go_Left ->
                case z of
                    (c, Node a t1 t2) -> do
                        let newGameState = gameState { binZip = (B0 a c t2, t1) }
                        go newGameState
                    (c, Leaf _) -> do
                        putStrLn "You cannot climb any further."
                        go gameState

            --
            --Node (Nothing) (Leaf (Just Rock)) (Node (Just Spider) (Leaf (Nothing)) (Leaf (Just Baby)))
            --


            Just Go_Right ->
                case z of
                    (c, Node a t1 t2) ->
                        do
                        let newGameState = gameState { binZip = (B1 a t1 c, t2)}
                        go newGameState
                    (c, Leaf _) -> do
                        putStrLn "You cannot climb any further."
                        go gameState

            Just Go_Back ->
                case z of
                    (Hole, _) -> do
                        putStrLn "You are already at the root."
                        putStrLn "You cannot go back any further."
                        go gameState
                    b -> do
                        let newGameState = gameState { binZip = go_back b }
                        go newGameState




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






main :: IO ()
main = repl
