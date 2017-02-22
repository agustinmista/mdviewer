module Main (main) where

import Data.Char
import Data.Map.Strict (Map, fromList)

import System.Environment
import System.Directory
import System.FilePath

import Control.Exception
import Control.Monad
import Control.Conditional

import Types
import Command
import Show
import Convert
import List
import Paths


loadStyles :: IO Styles
loadStyles = do
    stylesPath <- getStylesPath
    whenM (not <$> doesPathExist stylesPath) 
        (error $ "error loading style files from " ++ show stylesPath)
    
    files <- listDirectory stylesPath
    let styles = filter ((== ".css") . takeExtension) files
        names = map (map toLower . takeBaseName) styles
        paths = map (stylesPath </>) styles
    return $ fromList (zip names paths)


printException :: SomeException -> IO ()
printException e = do
    exe <- getProgName
    putStrLn $ exe ++ ": " ++ show e


dispatcher :: Styles -> Command -> IO ()
dispatcher styles cmd@(Show {}) = runShow cmd styles
dispatcher styles cmd@(Convert {}) = runConvert cmd styles
dispatcher styles List = runList styles


main :: IO ()
main = handle printException $ do
    command <- parseCommand
    styles <- loadStyles
    runApp command (dispatcher styles)
