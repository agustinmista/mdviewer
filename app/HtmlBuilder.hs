{-# LANGUAGE OverloadedStrings #-}

module HtmlBuilder where

import Prelude hiding (putStrLn, readFile, writeFile, head)

import Control.Exception
import Control.DeepSeq

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes (charset, rel, type_, href)
import Text.Blaze.Html.Renderer.String

import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Options
import Text.Pandoc.UTF8


returnNothing :: SomeException -> IO (Maybe String)
returnNothing e = do
    putStrLn $ "renderContents: exception raised\n" ++ displayException e
    return Nothing


renderContents :: FilePath -> Maybe FilePath -> IO (Maybe String)
renderContents input style = handle returnNothing $ do
    result <- case style of
        Nothing  -> Just <$> toPureHtmlString input
        Just css -> Just <$> toStylishedHtmlString css input
    return $!! result


toPureHtmlString :: FilePath -> IO String
toPureHtmlString input = pureHtmlString <$> readFile input

pureHtmlString :: String -> String
pureHtmlString contents = either throw buildHtml (readMarkdown def contents)
    where buildHtml pandoc = renderHtml $ do
            docTypeHtml $ do
                head (meta ! charset "UTF-8") 
                body (writeHtml def pandoc)


toStylishedHtmlString :: FilePath -> FilePath -> IO String
toStylishedHtmlString css input = stylishedHtmlString <$> readFile css <*> readFile input
    
stylishedHtmlString :: String -> String -> String
stylishedHtmlString css contents = either throw (buildHtml css) (readMarkdown def contents)
    where buildHtml css pandoc = renderHtml $ do
            docTypeHtml $ do
                head $ do
                    meta  ! charset "UTF-8" 
                    style ! type_ "text/css"
                          $ string css
            body (writeHtml def pandoc)

