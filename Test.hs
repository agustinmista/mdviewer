module Test where

import Prelude hiding (putStrLn, readFile, writeFile)

import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Options
import Text.Pandoc.UTF8


import Text.Blaze.Html.Renderer.String

test input = do
    contents <- readFile input

    case readMarkdown def contents of
        Left err -> print err
        Right pandoc -> do
            putStrLn "Writing!"
            writeFile "out.html" (renderHtml (writeHtml def pandoc))
    
    -- case result of 
    --     Left err -> print err
    --     Right pandoc -> do
    --         putStrLn (show pandoc) 
    --         --writeFile "out.html" (renderHtml (writeHtml def html))i
