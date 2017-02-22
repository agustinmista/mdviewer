module Paths where

import System.Directory
import System.FilePath

progName = "mdviewer"

stylesRelPath = "styles"
libRelPath = "lib"

getStylesSourcePath :: IO FilePath
getStylesSourcePath = makeAbsolute $ libRelPath </> stylesRelPath

getDataPath :: IO FilePath
getDataPath = getXdgDirectory XdgData progName

getConfigPath :: IO FilePath
getConfigPath = getXdgDirectory XdgConfig progName

getCachePath :: IO FilePath
getCachePath = getXdgDirectory XdgCache progName

getStylesPath :: IO FilePath
getStylesPath = (</> stylesRelPath) <$> getDataPath
