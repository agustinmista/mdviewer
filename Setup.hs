import Distribution.Simple
import Distribution.PackageDescription

import System.Directory
import System.FilePath

import Paths


copyStyle :: (FilePath, FilePath) -> IO ()
copyStyle (source, target) = do
    putStrLn $ "Copying from " ++ source ++ " to " ++ target
    copyFile source target

copyData _ _ _ _ = do
    
    -- about page
    dataPath <- getDataPath
    aboutSource <- getAboutSourcePath
    createDirectoryIfMissing True dataPath
    
    let aboutTarget = dataPath </> aboutFile
    putStrLn $ "Copying from " ++ aboutSource ++ " to " ++ aboutTarget 
    copyFile aboutSource aboutTarget 
    
    -- styles
    stylesSource <- getStylesSourcePath
    stylesTarget <- getStylesPath
    createDirectoryIfMissing True stylesTarget
    
    styles <- listDirectory stylesSource
    let paths = map (\s -> (stylesSource </> s, stylesTarget </> s)) styles
    mapM_ copyStyle paths


copyDataHooks = simpleUserHooks { postCopy = copyData }

main = defaultMainWithHooks copyDataHooks
