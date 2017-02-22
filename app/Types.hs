module Types where

import qualified Data.Map.Strict as Map

import Data.Maybe

-- | Built-in styles
type Styles = Map.Map String FilePath

(@>) :: Styles -> Command -> Maybe FilePath
styles @> cmd = do
    st <- style cmd
    Map.lookup st styles 


listStyles :: Styles -> [String]
listStyles = Map.keys


-- | Supported commands and parameters 
data Command
    = Show   
        { input :: FilePath
        , style :: Maybe String }
    | Convert
        { input  :: FilePath
        , output :: Maybe FilePath
        , style  :: Maybe String }
    | List
    deriving (Show)


-- | Command helpers
usesStyle :: Command -> Bool
usesStyle = isJust . style

usesOutput :: Command -> Bool
usesOutput = isJust . output

getStyle :: Command -> String
getStyle = fromJust . style

getOutput :: Command -> String
getOutput = fromJust . output
