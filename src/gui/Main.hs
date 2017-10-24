{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes#-}


module Main where

import Prelude
import qualified Prelude as Prelude
import qualified Data.ByteString.Char8 as BS
import Control.Exception
import Database.PostgreSQL.LibPQ
import Common
import Control.Monad
import Data.Ord
import Data.List


import Graphics.UI.WX --base
import Graphics.UI.WXCore hiding (Event) --low level
--import Graphics.UI.WX.Event

process line{-cur line-} conn{-connection handle-} = do
 

  mresult <- exec conn $ BS.pack line
  case mresult of
    Just result -> do
      exec_status <- resultStatus result
      println $ "Command status = " ++ show exec_status 
      case exec_status of
        FatalError -> do
          pure ()
          resultErrorMessage result >>= \(Just x) -> (println . BS.unpack)x --bad show
          
        TuplesOk -> do
          rowNum <- ntuples result
          colNum <- nfields result

          let
            mkContentCol i = mapM (\j -> getvalue result j i >>= \(Just x) -> pure $ BS.unpack x) [0,1..rowNum-1]
            mkContent i = if colNum /= i  --usage mkList 0, returns table in this form : [[elems of col 0], [elems of col 1], ...]
              then do
                _a <- mkContentCol i
                _b <- mkContent (i + 1)
                pure (_a : _b)
              else pure []


            mkHead i = do        --same as mkList but for entry names : [[name0], [name1], ...]
              if colNum /= i then do
                _a <- fname result i >>= \(Just x)-> pure $ BS.unpack x
                _b <- mkHead (i+1)
                pure $ [_a] : _b
              else
                pure []


            mergeContentAndHead [] [] = [] --this is needed later to calculate the max length of each column (including head !)
            mergeContentAndHead (con0 : con){-content-} ((head0 : _) : head){-head-} = (head0 : con0) : mergeContentAndHead con head

            lengthsFun [] = [] --returns list of this form : [max len of elems of column 0, max len of elems of column 1, ...]
            lengthsFun (x : xs) = (length $ maximumBy (comparing length) x) : lengthsFun xs

            putPluses 0 = putStr "+"
            putPluses n = do {putStr "-"; putPluses (n-1)}

            putSpaces 0 = putStr "| "
            putSpaces n = do {putStr " "; putSpaces (n-1)}

          _content <- mkContent 0
          _head    <- mkHead 0
          let _lens =  lengthsFun $ mergeContentAndHead _content _head

          let _CONST_SPACES = 3
          
          let
            printRow [] [] = pure []  --print one row of the table, does not print \n at the end
            printRow (x : xs){-table-} (l : ls){-lengths-} =
              case x of
                (e : es) -> do
                  putStr e
                  let elen = length e
                  putSpaces (l - elen + _CONST_SPACES) 
                  next <- printRow xs ls
                  pure $ es : next
                [] -> pure []

            printCosmetics [] = pure () --bad realization for nice pixel-graphic output (---+---+)
            printCosmetics (l : ls){-lengths-} = do
               putPluses (l + 1 + _CONST_SPACES) 
               next <- printCosmetics ls
               pure ()

            printTable [] = pure () --print full table (this is called for head and content separately)
            printTable table = do
              next <- printRow table _lens
              case next of
                (x : xs') ->
                  case x of
                   (_ : _) -> do
                     putStr "\n"
                     printTablePadded next
                   [] -> pure ()
                [] -> pure ()

            printTablePadded [] = pure () --same as printTable but first column is padded with one ' '
            printTablePadded table = do {putStr " "; printTable table}

          
          printTablePadded _head
          putStr "\n"
          printCosmetics _lens
          putStr "\n"
          printTablePadded _content
          putStr "\n"


          pure ()
        _ -> pure ()
      
    Nothing ->
      println "No result"
  
  pure ()
  

loop conn = do
  println $ "Enter your query or 'exit'"
  line <- getLine
  println $ "You entered: " ++ line

  if line == "exit" then pure () else do {process line conn; loop conn}
      
      

cmd :: IO ()
cmd = do
   conn <- connectdb "host='0.0.0.0' port=32768 dbname='docker' user='docker' password='docker'" --local docker
   checked <- status conn
   case checked of
     ConnectionOk -> do
       println "Succesfully connected to db"
       loop conn
     _ -> do
       println "Connection failed!"
   finish conn
   println "Goodbye!"

main :: IO ()
main
  = start gui

hello :: IO ()
hello
  = do f    <- frame    [text := "Hello!"]
       quit <- button f [text := "Quit", on command := close f]
       set f [layout := widget quit]

       
hello1 :: IO ()
hello1
  = do -- the application frame
       f      <- frame         [text := "Hello world!", clientSize := sz 300 200]                               

       -- create file menu  
       file   <- menuPane      [text := "&File"]
       _quit  <- menuQuit file [help := "Quit the demo", on command := close f]

       -- create Help menu
       hlp    <- menuHelp      []
       about  <- menuAbout hlp [help := "About wxHaskell"]

       -- create statusbar field
       status <- statusField   [text := "Welcome to wxHaskell"]

       -- set the statusbar and menubar
       set f [ statusBar := [status]
             , menuBar   := [file,hlp]
             -- as an example, put the menu event handler for an about box on the frame.
             ,on (menu about) := infoDialog f "About wxHaskell" "This is a wxHaskell demo"
             ]

hello2 :: IO ()
hello2
  = do f      <- frame  [text := "Layout test"]
       p      <- panel  f []                       -- panel for color and tab management.
       ok     <- button p [text := "Ok", on command := close f]
       can    <- button p [text := "Cancel", on command := infoDialog f "Info" "Pressed 'Cancel'"]
       xinput <- textEntry p [text := "100", alignment := AlignRight]
       yinput <- textEntry p [text := "100", alignment := AlignRight]
       zinput <- textEntry p [text := "100", alignment := AlignRight]

       set f [defaultButton := ok
             ,layout := container p $
                        margin 10 $
                        column 5 [boxed "coordinates" (grid 5 5 [[label "x:", hfill $ widget xinput]
                                                                ,[label "y:", hfill $ widget yinput]
                                                                ,[label "z:", hfill $ widget zinput]])
                                 ,floatBottomRight $ row 5 [widget ok,widget can]]
             ] 
       return ()


gui :: IO ()
gui 
  = do f <- frame [text := "Grid test", visible := False] 
           
       -- use text control as logger
       textlog <- textCtrl f [wrap := WrapNone, enabled := False] 
       textCtrlMakeLogActiveTarget textlog
       logMessage "logging enabled"              

       -- grids
       g <- gridCtrl f []
       gridSetGridLineColour g (colorSystem Color3DFace)
       gridSetCellHighlightColour g black
       appendColumns g (head names)
       appendRows    g (map show [1..length (tail names)])
       mapM_ (setRow g) (zip [0..] (tail names))
       gridAutoSize g

       -- layout
       set f [layout := column 5 [fill (static (widget g))
                                 ,hfill $ minsize (sz 20 80) $ widget textlog]
             ]       
       focusOn g
       set f [visible := True]  -- reduce flicker at startup.
       return ()

names :: [[String]]
names
  = [["First Name", "Last Name"]
    ,["Daan","Leijen"],["Arjan","van IJzendoorn"]
    ,["Martijn","Schrage"],["Andres","Loh"]]


setRow :: Grid a -> (Int, [String]) -> IO ()
setRow g (row_, values)
  = mapM_ (\(col,value_) -> gridSetCellValue g row_ col value_) (zip [0..] values)


{--------------------------------------------------------------------------------
   Library?
--------------------------------------------------------------------------------}

gridCtrl :: Window a -> [Prop (Grid ())] -> IO (Grid ())
gridCtrl parent_ props_
  = feed2 props_ 0 $
    initialWindow $ \id_ rect' -> \props' flags ->
    do g <- gridCreate parent_ id_ rect' flags
       gridCreateGrid g 0 0 0
       set g props'
       return g

gridEvent :: Event (Grid a) (EventGrid -> IO ())
gridEvent
  = newEvent "gridEvent" gridGetOnGridEvent gridOnGridEvent



appendColumns :: Grid a -> [String] -> IO ()
appendColumns _g []
  = return ()
appendColumns g labels
  = do n <- gridGetNumberCols g
       _ <- gridAppendCols g (length labels) True
       mapM_ (\(i, label_) -> gridSetColLabelValue g i label_) (zip [n..] labels)

appendRows :: Grid a -> [String] -> IO ()
appendRows _g []
  = return ()
appendRows g labels
  = do n <- gridGetNumberRows g
       _ <- gridAppendRows g (length labels) True
       mapM_ (\(i, label_) -> gridSetRowLabelValue g i label_) (zip [n..] labels)
