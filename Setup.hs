import Distribution.Simple
import Distribution.PackageDescription

import System.Directory
import System.FilePath

import Paths


copyStyle :: (FilePath, FilePath) -> IO ()
copyStyle (source, target) = do
    putStrLn $ "Copying from " ++ source ++ " to " ++ target
    copyFile source target

copyStyles _ _ _ _ = do
    sourcePath <- getStylesSourcePath
    targetPath <- getStylesPath
   
    createDirectoryIfMissing True targetPath
    
    
    styles <- listDirectory sourcePath
    let paths = map (\s -> (sourcePath </> s, targetPath </> s)) styles

    mapM_ copyStyle paths
    

copyStylesHooks = simpleUserHooks { postCopy = copyStyles }

main = defaultMainWithHooks copyStylesHooks
