module Command (module Command, runApp) where

import Data.Maybe
import Control.Applicative

import System.Console.ArgParser
import System.Console.ArgParser.QuickParams

import Types


-- | Required to read Maybe parameters
instance RawRead a => RawRead (Maybe a) where
    rawParse s = do 
        (val, rem) <- rawParse s 
        return (Just val, rem)


-- | Main parsing function
parseCommand :: IO (CmdLnInterface Command)
parseCommand = setVersion "1.0.0"
           <$> setDescr "A basic Markdown viewer/converter with css support"
           <$> setEpilog "Some epilog"             
           <$> parseSubcommand


-- | Compose main parser using subcommand parsers
parseSubcommand :: IO (CmdLnInterface Command)
parseSubcommand = mkSubParser 
    [ ("show", mkDefaultApp showParser "show") 
    , ("convert", mkDefaultApp convertParser "convert") 
    , ("list", mkDefaultApp listParser "list") ] 


-- | Show subcommand parser
showParser :: ParserSpec Command
showParser = Show
    `parsedBy` optPos  ""      "input"  `Descr` "Markdown input file"
    `andBy`    optFlag Nothing "style"  `Descr` "Css style to embed"

-- | Convert subcommand parser
convertParser :: ParserSpec Command 
convertParser = Convert
    `parsedBy` reqPos          "input"  `Descr` "Markdown input file"
    `andBy`    optFlag Nothing "output" `Descr` "Output Html file"
    `andBy`    optFlag Nothing "style"  `Descr` "Css style to embed"

-- | List subcommand parser
listParser = pure List 
