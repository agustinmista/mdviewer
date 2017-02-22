module List (runList) where

import Types

runList :: Styles -> IO ()
runList styles = do
    putStrLn "Available styles:"
    mapM_ (putStrLn . (" * "++)) (listStyles styles)
