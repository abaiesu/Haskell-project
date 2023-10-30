data Item = Rock | Spider | Baby
    deriving (Show, Eq)
data Bin = Leaf (Maybe Item) | Node (Maybe Item) Bin Bin
    deriving (Show, Eq)

main :: IO()
main = do
    let test2 = Node (Just Spider) (Leaf Nothing) (Leaf (Just Baby))
    let test = Node Nothing test2 (Leaf (Just Rock))
    putStrLn $ "Test Tree2 is " ++ show test2
    putStrLn $ "Test Tree is " ++ show test