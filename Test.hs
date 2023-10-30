import Text.Read (readMaybe)

-- Function to read a number from user input
readNumber :: IO (Maybe Int)
readNumber = do
    putStrLn "Enter a number: "
    input <- getLine
    case readMaybe input of
        Just n -> return (Just n)
        Nothing -> do
            putStrLn "Invalid input. Please enter a valid number."
            return Nothing

-- Function to get the sum of two numbers from user input
getSum :: IO ()
getSum = do
    putStrLn "Get the sum of two numbers."
    num1 <- readNumber
    num2 <- readNumber
    case (num1, num2) of
        (Just x, Just y) -> putStrLn $ "The sum of " ++ show x ++ " and " ++ show y ++ " is " ++ show (x + y)
        _ -> putStrLn "Invalid input. Please enter valid numbers."

-- The top-level interactive loop
repl :: IO ()
repl = do
    putStrLn "Welcome to the simple calculator REPL."
    putStrLn "Enter '1' to get the sum of two numbers or 'q' to quit."
    choice <- getLine
    case choice of
        "1" -> getSum
        "q" -> putStrLn "Goodbye!"
        _ -> do
            putStrLn "Invalid choice. Please enter '1' or 'q'."
            repl

main :: IO ()
main = repl