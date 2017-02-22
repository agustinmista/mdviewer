{-# LANGUAGE OverloadedStrings #-}

module HtmlBuilder where

import Prelude hiding (readFile, writeFile, head)

import Data.String
import Data.Text.Lazy hiding (head)
import Data.Text.Lazy.IO

import Text.Markdown
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes (rel, type_, href)


toHtml :: FilePath -> Maybe FilePath -> IO Html
toHtml input Nothing = toHtmlPure input
toHtml input (Just css) = toHtmlWithCss css input

toHtmlPure :: FilePath -> IO Html
toHtmlPure input = pureHtml <$> readFile input

pureHtml :: Text -> Html
pureHtml md = docTypeHtml $ body $ markdown def md

toHtmlWithCss :: FilePath -> FilePath -> IO Html
toHtmlWithCss css input = stylishedHtml <$> readFile css <*> readFile input 

stylishedHtml :: Text -> Text -> Html
stylishedHtml css contents = do
    docTypeHtml $ do
        head $ do
            style ! type_ "text/css"
                  $ text (toStrict css)
        body $ markdown def contents
