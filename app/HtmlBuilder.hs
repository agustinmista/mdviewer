{-# LANGUAGE OverloadedStrings #-}

module HtmlBuilder where

import Prelude hiding (readFile, writeFile, head)

import Data.String
import Data.Text.Lazy hiding (head)
import Data.Text.Lazy.IO hiding (putStrLn)

import Control.Exception
import Control.DeepSeq

import Text.Markdown
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes (charset, rel, type_, href)
import Text.Blaze.Html.Renderer.Text


returnNothing :: SomeException -> IO (Maybe Text)
returnNothing e = do
    putStrLn $ "renderContents: exception raised\n" ++ show e
    return Nothing

renderContents :: FilePath -> Maybe FilePath -> IO (Maybe Text)
renderContents input style = handle returnNothing $ do
    result <- case style of
        Nothing  -> toHtmlPure input 
        Just css -> toHtmlWithCss css input
    return $!! Just (renderHtml result)    

toHtmlPure :: FilePath -> IO Html
toHtmlPure input = pureHtml <$> readFile input

pureHtml :: Text -> Html
pureHtml md = do
    docTypeHtml $ do
        head $ meta ! charset "UTF-8" 
        body $ markdown def md

toHtmlWithCss :: FilePath -> FilePath -> IO Html
toHtmlWithCss css input = stylishedHtml <$> readFile css <*> readFile input 

stylishedHtml :: Text -> Text -> Html
stylishedHtml css contents = do
    docTypeHtml $ do
        head $ do
            meta  ! charset "UTF-8" 
            style ! type_ "text/css"
                  $ text (toStrict css)
        body $ markdown def contents
