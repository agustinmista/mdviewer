module Convert (runConvert) where

import Prelude hiding (writeFile)

import Text.Blaze.Html.Renderer.Text

import Data.Text.Lazy
import Data.Text.Lazy.IO
import System.FilePath

import Types
import HtmlBuilder

runConvert :: Command -> Styles ->  IO ()
runConvert cmd styles = do
    
    html <- toHtml (input cmd) (styles @> cmd)
    
    let output | usesOutput cmd = getOutput cmd
               | otherwise      = input cmd -<.> "html"

    writeFile output (renderHtml html) 
