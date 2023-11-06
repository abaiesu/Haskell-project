
import Bin
import Printing
import Cmd
import Parser ( parseCmd, parseInput )
import System.IO ( hFlush, stdout )
import System.Random ()
import Data.Maybe (isJust)
import Control.Concurrent (threadDelay)

-- Define a variable to keep track of the number of rocks collected
data GameState = GameState {
    binZip :: BinZip Item,
    rocksCollected :: Int,
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
    putStrLn""
    putStrLn""
    putStrLn " _____ ____      _    ____ _____  _    _     "
    putStrLn "|  ___|  _ \\    / \\  / ___|_   _|/ \\  | |    "
    putStrLn "| |_  | |_) |  / _ \\| |     | | / _ \\ | |    "
    putStrLn "|  _| |  _ <  / ___ \\ |___  | |/ ___ \\| |___ "
    putStrLn "|_|___|_| \\_\\/_/  _\\_\\____| |_/_/   \\_\\_____|"
    putStrLn "|  ___/ \\  |  _ \\|  \\/  | |                  "
    putStrLn "| |_ / _ \\ | |_) | |\\/| | |                  "
    putStrLn "|  _/ ___ \\|  _ <| |  | |_|                  "
    putStrLn "|_|/_/   \\_\\_| \\_\\_|  |_(_)                 " 
    putStrLn""
    putStrLn""
    putStrLn "Welcome to Fractal Farmer! Are you ready for a hard day of work?"
    putStrLn""
    putStrLn "Collect stones and kill crows to gain points and protect your farm"
    putStrLn""
    putStrLn "Do be aware that if you are close enough you can agitate the crows"
    putStrLn "and then crows can eat your crops and you will lose points!"
    putStrLn""
    putStrLn""
    tree <-geninftree
    let game = GameState {
      binZip = (Hole, tree),
      rocksCollected = 0,
      points = 0
    }
    go game tree
  where
    go :: GameState -> Bin Item -> IO ()
    go gameState inittree= do
        let (z2,z3) = binZip gameState
        z4 <- generateCrops z3 0.05
        (z5, neg) <- updateCrowEat z4 0 300 
        let z = (z2,z4)
        prettyPrint z
        case z of
            (_, Leaf (_, Nothing)) -> putStrLn "You see an empty leaf."
            (_, Leaf (_, Just item)) -> putStrLn $ "You see a leaf with a " ++ show item
            (_, Node (_, Nothing) _ _) -> putStrLn "You see an empty binary node"
            (_, Node (_, Just item) _ _) -> putStrLn $ "You see a binary node with a " ++ show item
        putStrLn $ "Your points are " ++ show (points gameState)
        putStrLn $ "Number of Rocks is " ++ show (rocksCollected gameState)
        putStr "> "
        hFlush stdout
        line <- getLine                                  -- get a line of input
        case parseInput parseCmd line of

            Nothing -> do
                putStrLn "I'm sorry, I do not understand."
                go gameState inittree

            Just Go_Left ->
                case z of
                    (c, Node a t1 t2) -> do
                        let newGameState = gameState { binZip = (B0 a c t2, t1), points = points gameState - neg}
                        go newGameState inittree
                    (c, Leaf _) -> do
                        putStrLn "You cannot climb any further."
                        go gameState inittree


            Just Go_Right ->
                case z of
                    (c, Node a t1 t2) ->
                        do
                        let newGameState = gameState { binZip = (B1 a t1 c, t2), points = points gameState - neg}
                        go newGameState inittree
                    (c, Leaf _) -> do
                        putStrLn "You cannot climb any further."
                        go gameState inittree

            Just Go_Back ->
                case z of
                    (Hole, _) -> do
                        putStrLn "You are already at the root."
                        putStrLn "You cannot go back any further."
                        go gameState inittree
                    b -> do
                        let newGameState = gameState { binZip = go_back b, points = points gameState - neg }
                        go newGameState inittree




            Just Do_Collect -> do
                if rocksCollected gameState < 3
                    then do
                        result <- do_collect (snd z)
                        case result of
                            Just newTree -> do
                                let newGameState = gameState { binZip = (fst z, newTree), rocksCollected = rocksCollected gameState + 1, points = points gameState - neg }
                                go newGameState inittree
                            Nothing -> go gameState inittree
                    else do
                        putStrLn "You cannot collect more rocks. Limit reached."
                        go gameState inittree


            Just Do_Shoot -> do
                result <- do_shoot (snd z)
                case result of
                    Just newTree -> do
                                let newGameState = gameState { binZip = (fst z, newTree), rocksCollected = rocksCollected gameState-1, points = points gameState+1-neg}
                                go newGameState inittree
                    Nothing -> go gameState inittree


            Just Quit -> do
                putStrLn "Okay."
                putStrLn "You ended the game over here:\n"
                prettyPrint z
                putStrLn $ "Your points are" ++ show(points gameState)
                return ()






main :: IO ()
main = repl

