module Convert (runConvert) where

import Prelude hiding (writeFile)

import Text.Pandoc.UTF8

import System.FilePath
import System.Exit

import Types
import HtmlBuilder


errorMessage :: IO ()
errorMessage = die "ABORTING!"

runConvert :: Command -> Styles ->  IO ()
runConvert cmd styles = do
    
    result <- renderContents (input cmd) (styles @> cmd)
    case result of
        Nothing -> errorMessage
        Just html -> do
            writeFile output html 
                where output | usesOutput cmd = getOutput cmd
                             | otherwise      = input cmd -<.> "html"
