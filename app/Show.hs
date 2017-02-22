{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Show (runShow) where

import Prelude hiding (writeFile)

import Control.Exception
import Control.Concurrent
import Control.Conditional
import Control.Monad (void) 
import Control.Monad.IO.Class

import Graphics.UI.Gtk 
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.Windows.Window
import System.Glib.UTFString
import System.FilePath
import System.Exit

import Data.Maybe
import Data.List (elemIndex)
import Data.Text.Lazy hiding (length, map)
import Data.Text.Lazy.IO (writeFile)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text

import Types
import HtmlBuilder


offsetStyleFrom :: Int -> Command -> Styles -> Command
offsetStyleFrom offset cmd styles = cmd { style = offsetStyle }
    where stylesQueue = Nothing : map Just (listStyles styles)
          currIndex = fromJust $ elemIndex (style cmd) stylesQueue
          offsetIndex = (currIndex + offset) `mod` (length stylesQueue - 1) 
          offsetStyle = stylesQueue !! offsetIndex

nextStyleFrom :: Command -> Styles -> Command
nextStyleFrom = offsetStyleFrom 1

prevStyleFrom :: Command -> Styles -> Command
prevStyleFrom = offsetStyleFrom (-1)


setInputFile :: Command -> FilePath -> Command
setInputFile cmd path = cmd { input = path } 


setContent :: WebView -> Html -> IO ()
setContent webview html = webViewLoadString webview contents Nothing baseUri
    where contents = toStrict (renderHtml html)
          baseUri  = toStrict (pack "")


makeTitle :: Command -> String
makeTitle cmd = status ++ "  -  Markdown Viewer" 
    where status | usesStyle cmd = input cmd ++ "@" ++ fromJust (style cmd)
                 | otherwise     = input cmd


genericDialogNew :: String -> Window -> IO FileChooserDialog
genericDialogNew action window = fileChooserDialogNew  
    (Just action) (Just window) 
    FileChooserActionSave 
    [ (action, ResponseAccept) 
    , ("Cancel", ResponseCancel) ]


saveDialogNew, openDialogNew :: Window -> IO FileChooserDialog
saveDialogNew = genericDialogNew "Save"
openDialogNew = genericDialogNew "Open"


whenReturnFilename :: FileChooserDialog -> (FilePath -> IO ()) -> IO ()
whenReturnFilename dialog action = do 
    response <- dialogRun dialog
    case response of
        ResponseAccept -> do
            dialogVal <- fileChooserGetFilename dialog
            case dialogVal of
                Just path -> action path
                _ -> return ()
        _ -> return ()


runShow :: Command -> Styles -> IO ()
runShow cmd styles = do
    
    -- Create an "global" state using that keeps the style and
    -- the current file displayed between different events handles
    status <- newMVar cmd

    -- Initialize the GUI
    void initGUI
    
    -- Create the widgets
    window <- windowNew
    scrolled <- scrolledWindowNew Nothing Nothing
    webview <- webViewNew
    
    -- Set widgets attributes
    window `set` [ windowTitle          := makeTitle cmd
                 , windowResizable      := True
                 , windowWindowPosition := WinPosCenter
                 , windowDefaultWidth   := 1024
                 , windowDefaultHeight  := 768
                 , containerChild       := scrolled ]

    scrolled `set` [ containerChild := webview ]
    
    html <- toHtml (input cmd) (styles @> cmd)
    webview `setContent` html

    -- Handle events
    window `on` deleteEvent $ liftIO mainQuit >> return False

    window `on` keyPressEvent $ tryEvent $ do
        "q" <- eventKeyName
        liftIO $ mainQuit >> exitSuccess
    
    window `on` keyPressEvent $ tryEvent $ do
        "F11" <- eventKeyName
        liftIO $ do 
            dec <- window `get` windowDecorated
            window `set` [ windowDecorated := not dec ]

    window `on` keyPressEvent $ tryEvent $ do
        "r" <- eventKeyName
        liftIO $ do 
            cmd' <- readMVar status 
            html' <- toHtml (input cmd') (styles @> cmd')
            webview `setContent` html'
   
    window `on` keyPressEvent $ tryEvent $ do
        "z" <- eventKeyName
        liftIO $ ifM (webViewCanGoBack webview) 
            (webViewGoBack webview)
            (webview `setContent` html)

    window `on` keyPressEvent $ tryEvent $ do
        "x" <- eventKeyName
        liftIO $ whenM (webViewCanGoForward webview) (webViewGoForward webview)

    window `on` keyPressEvent $ tryEvent $ do
        "s" <- eventKeyName
        liftIO $ do
            cmd' <- modifyMVar status $ \cmd -> do
                let cmd' = cmd `nextStyleFrom` styles 
                return (cmd', cmd')
            window `set` [ windowTitle := makeTitle cmd' ]
            html' <- toHtml (input cmd') (styles @> cmd')
            webview `setContent` html'

    window `on` keyPressEvent $ tryEvent $ do
        "a" <- eventKeyName
        liftIO $ do
            cmd' <- modifyMVar status $ \cmd -> do
                let cmd' = cmd `prevStyleFrom` styles 
                return (cmd', cmd')
            window `set` [ windowTitle := makeTitle cmd' ]
            html' <- toHtml (input cmd') (styles @> cmd')
            webview `setContent` html'
    
    window `on` keyPressEvent $ tryEvent $ do
        "o" <- eventKeyName
        liftIO $ do
            
            dialog <- openDialogNew window
            filter <- fileFilterNew
            fileFilterAddMimeType filter ("text/plain" :: String)
            fileChooserAddFilter dialog filter
            widgetShow dialog
            dialog `whenReturnFilename` \path -> do
                
                putStrLn $ "Opening file from " ++ path
                cmd' <- modifyMVar status $ \cmd -> do
                    let cmd' = setInputFile cmd path
                    return (cmd', cmd')
                window `set` [ windowTitle := makeTitle cmd' ]
                html' <- toHtml path (styles @> cmd)  
                webview `setContent` html'
            
            widgetDestroy dialog

    window `on` keyPressEvent $ tryEvent $ do
        "w" <- eventKeyName
        liftIO $ do
            
            dialog <- saveDialogNew window
            widgetShow dialog
            dialog `whenReturnFilename` \path -> do
                
                cmd' <- readMVar status
                html' <- toHtml (input cmd') (styles @> cmd')
                
                let path' = if hasExtension path
                            then path
                            else path <.> "html"
                
                putStrLn $ "Saving html file to " ++ path'
                writeFile path' (renderHtml html') 
            
            widgetDestroy dialog
   
    
    -- Start the GUI main loop
    widgetShowAll window
    mainGUI

