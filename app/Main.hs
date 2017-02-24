{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Data.Char
import Data.Maybe
import Data.Map.Strict (Map, fromList, member, insert)

import System.Environment
import System.Directory
import System.FilePath

import Control.Exception
--import Control.Monad
import Control.Conditional

import Types
import Command
import Show
import Convert
import List
import Paths


getStyleName :: FilePath -> String
getStyleName = map toLower . takeBaseName

loadStyles :: IO Styles
loadStyles = do
    stylesPath <- getStylesPath
    whenM (not <$> doesPathExist stylesPath) 
        (error $ "error loading style files from " ++ show stylesPath)
    
    files <- listDirectory stylesPath
    let styles = filter ((== ".css") . takeExtension) files
        names = map getStyleName styles
        paths = map (stylesPath </>) styles
    return $ fromList (zip names paths)


printException :: SomeException -> IO ()
printException e = do
    exe <- getProgName
    putStrLn $ exe ++ ": " ++ show e


dispatcher :: Styles -> Command -> IO ()
dispatcher styles List = runList styles
dispatcher styles cmd = do
    (cmd', styles') <- sanitizeStyle cmd styles 
    case cmd' of
        Convert {} -> runConvert cmd' styles'
        Show {input = "" } -> do
                about <- getAboutFile
                runShow (cmd'{input = about}) styles'
        Show {} -> runShow cmd' styles'        


sanitizeStyle :: Command -> Styles -> IO (Command, Styles) 
sanitizeStyle cmd@(style -> Nothing) styles = return (cmd, styles)
sanitizeStyle cmd@(style -> Just st) styles = 
    if st `member` styles 
    then do
        putStrLn $ "Using built-in style: " ++ st
        return (cmd, styles)
    else do 
        fileExists <- doesFileExist st
        let isCSS = takeExtension st == ".css"
        if fileExists && isCSS 
            then do 
                stylesPath <- getStylesPath
                let stName = getStyleName st
                    copyPath = stylesPath </> st
                putStrLn $ "Using external style from "++ st ++" as " ++ stName 
                copyFile st copyPath
                putStrLn $ "External style copied to " ++ copyPath
                return (cmd{style = Just stName}, insert stName st styles)
            else do 
                putStrLn $ "Provided style is not built-in nor a valid CSS\
                            \ file. Using pure HTML instead"
                return (cmd {style = Nothing}, styles)


main :: IO ()
main = handle printException $ do
    command <- parseCommand
    styles <- loadStyles
    runApp command (dispatcher styles)
