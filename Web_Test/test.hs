
{-# LANGUAGE OverloadedStrings #-}
import Graphics.UI.Threepenny
import qualified Graphics.UI.Threepenny as UI

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    button <- button #+ [string "Increment"]
    counter <- UI.span # set text "0"

    getBody window #+ [element button, element counter]

    let incrementEvent = (+1) <$ UI.click button
    counterText <- accumB 0 incrementEvent

    on UI.click button $ \_ -> do
        n <- currentValue counterText
        element counter # set text (show n)