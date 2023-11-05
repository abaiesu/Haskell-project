-- File to test all our functions
-- not the helper ones just the big ones
import Bin

testing :: IO ()
testing = do
    putStrLn "Testing"
    putStrLn ""
    putStrLn "---------------------------------"
    putStrLn"            testing           "
    putStrLn "---------------------------------"
    putStrLn ""
    -- Test generateTree function
    putStrLn "Testing generateTree:"
    tree <- generateTree 10
    printLabels tree
    putStrLn ""
    putStrLn ""
    putStrLn "---------------------------------"
    putStrLn ""
    putStrLn ""
    -- Test gener
    -- Test generateCrops function
    putStrLn "\nTesting generateCrops:"
    tree <- generateCrops tree 0.5
    printLabels tree
    putStrLn ""
    putStrLn ""
    putStrLn "---------------------------------"
    putStrLn ""
    putStrLn ""
    -- Test gener
    -- Test genCropCxt function
    putStrLn "\nTesting genCropCxt:"
    let treeContext = B0 (False, Just Crow) Hole tree
    newContext <- genCropCxt treeContext 0.5
    print newContext
    putStrLn ""
    putStrLn ""
    putStrLn "---------------------------------"
    putStrLn ""
    putStrLn ""
    -- Test gener
    -- Test populateEmptyNodes function
    putStrLn "\nTesting populateEmptyNodes:"
    treeWithEmptyNodes <- generateTree 3
    populatedTree <- populateEmptyNodes treeWithEmptyNodes 5
    printLabels populatedTree
    putStrLn ""
    putStrLn ""
    putStrLn "---------------------------------"
    putStrLn ""
    putStrLn ""
    -- Test gener
    -- Test updateCrowEat function
    putStrLn "\nTesting updateCrowEat:"
    updatedTree <- updateCrowEat tree 0 1
    printLabels (fst updatedTree)
    putStrLn ""
    putStrLn ""
    putStrLn "---------------------------------"
    putStrLn ""
    putStrLn ""
    putStrLn $ "Crow Ate: " ++ show(snd updatedTree)
    putStrLn ""
    putStrLn ""
    putStrLn "---------------------------------"
    putStrLn ""
    putStrLn ""
    -- Test gener
    -- Test plug function
    putStrLn "\nTesting plug:"
    let binZip = (Hole, tree)
    let newTree = plug binZip
    printLabels newTree
