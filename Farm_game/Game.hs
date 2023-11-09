
import Bin
import Printing (prettyPrint, trimTree)
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
    tree <-generateTree 5 
    let game = GameState {
      binZip = (Hole, tree),
      rocksCollected = 0,
      points = 0
    }
    go game tree
  where
    go :: GameState -> Bin Item -> IO ()
    go gameState init= do
        let (z2,z3') = binZip gameState
        z3 <- replaceLeavesWithTree 5 (trimTree z3' 5) init
        z4 <- generateCrops z3 0.05
        (z5, neg) <- updateCrowEat (trimTree z4 5) 0 3
        z6 <- populateEmptyNodes z5 2
        if neg > 0 
            then putStrLn $ "Oh NOOO the crows ate " ++ show neg ++ " crops"
        else
            putStr ""
        let z = (z2,z6)
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
                go gameState init

            Just Go_Left ->
                case z of
                    (c, Node a t1 t2) -> do
                        let newGameState = gameState { binZip = (B0 a c t2, t1), points = points gameState - neg}
                        go newGameState init
                    (c, Leaf _) -> do
                        putStrLn "You cannot climb any further."
                        go gameState init


            Just Go_Right ->
                case z of
                    (c, Node a t1 t2) ->
                        do
                        let newGameState = gameState { binZip = (B1 a t1 c, t2), points = points gameState - neg}
                        go newGameState init
                    (c, Leaf _) -> do
                        putStrLn "You cannot climb any further."
                        go gameState init

            Just Go_Back ->
                case z of
                    (Hole, _) -> do
                        putStrLn "You are already at the root."
                        putStrLn "You cannot go back any further."
                        go gameState init
                    b -> do
                        let newGameState = gameState { binZip = go_back b, points = points gameState - neg }
                        go newGameState init




            Just Do_Collect -> do
                if rocksCollected gameState < 3
                    then do
                        result <- do_collect (snd z)
                        case result of
                            Just newTree -> do
                                let newGameState = gameState { binZip = (fst z, newTree), rocksCollected = rocksCollected gameState + 1, points = points gameState - neg }
                                go newGameState init
                            Nothing -> go gameState init
                    else do
                        putStrLn "You cannot collect more rocks. Limit reached."
                        go gameState init


            Just Do_Shoot -> do
                result <- do_shoot (snd z)
                case result of
                    Just newTree -> do
                                let newGameState = gameState { binZip = (fst z, newTree), rocksCollected = rocksCollected gameState-1, points = points gameState+1-neg}
                                go newGameState init
                    Nothing -> go gameState init


            Just Quit -> do
                putStrLn "Okay."
                putStrLn "You ended the game over here:\n"
                prettyPrint z
                putStrLn $ "Your points are" ++ show(points gameState)
                return ()






main :: IO ()
main = repl

