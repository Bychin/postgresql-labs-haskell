{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, FlexibleContexts #-} --remove FlexibleContexts


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
import Text.Printf --printf
import Control.Concurrent.MVar





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
  conn <- connectdb "host='0.0.0.0' port=32768 dbname='docker' user='docker' password='docker'"
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
main = do 
  --conn <- connectdb ""
  myConn <- newEmptyMVar
  start $ gui myConn
  --finish conn


guiConnection :: MVar Database.PostgreSQL.LibPQ.Connection -> IO ()
guiConnection boxedConn 
  = do 
    f             <- frame       [text := "Setup connection"]
    p             <- panel     f []  -- panel for color and tab management.
    okButton      <- button    p [text := "Ok"] --TODO conn db!
    cancelButton  <- button    p [text := "Cancel",  on command := close f]
    hostInput     <- textEntry p [text := "0.0.0.0", alignment := AlignRight]
    portInput     <- textEntry p [text := "32768",   alignment := AlignRight]
    dbnameInput   <- textEntry p [text := "docker",  alignment := AlignRight]
    userInput     <- textEntry p [text := "docker",  alignment := AlignRight]
    passwordInput <- textEntry p [text := "docker",  alignment := AlignRight]

    host     <- get hostInput text
    port     <- get portInput text
    dbname   <- get dbnameInput text
    user     <- get userInput text
    password <- get passwordInput text

    set okButton [on command :=
                  do connectDatabase boxedConn (get hostInput text)
                                               (get portInput text)
                                               (get dbnameInput text)
                                               (get userInput text)
                                               (get passwordInput text)
                     checkConnection boxedConn f]
    --set s [on command := do{ i <- get s selection; set g [selection := i]} ]

    set f [defaultButton := okButton
          ,layout := container p $
                     margin 5 $
                     floatCentre $ column 0 [boxed "Connection" (grid 5 10 [[label "Host:", hfill $ widget hostInput]
                                                                           ,[label "Port:", hfill $ widget portInput]
                                                                           ,[label "DB Name:", hfill $ widget dbnameInput]
                                                                           ,[label "User:", hfill $ widget userInput]
                                                                           ,[label "Password:", hfill $ widget passwordInput]])
                                            ,row 0 [widget okButton, widget cancelButton]]
          ]
    return ()

  where
    connectDatabase :: MVar Database.PostgreSQL.LibPQ.Connection -> IO String -> IO String -> IO String -> IO String -> IO String -> IO ()
    connectDatabase conn host' port' dbname' user' password' = do
        host     <- host'
        port     <- port'
        dbname   <- dbname'
        user     <- user'
        password <- password'
        let connParams = printf "host='%s' port=%s dbname='%s' user='%s' password='%s'" host port dbname user password
        println connParams
        newConn <- connectdb $ BS.pack connParams 
        putMVar conn newConn --conn <- connectdb $ BS.pack connParams
        return ()

    --checkConnection :: MVar Database.PostgreSQL.LibPQ.Connection -> <frame type here> -> IO ()
    checkConnection connection frame
      = do
        checked <- readMVar connection >>= status
        case checked of
          ConnectionOk -> do
            println "Succesfully connected to db"
            close frame
            --readMVar connection >>= status
          _ -> do
            println "Connection failed!"


gui :: MVar Database.PostgreSQL.LibPQ.Connection -> IO ()
gui boxedConn
  = do f <- frame [text := "PostgreSQL Client"] 
       p      <- panel  f []  -- panel for color and tab management.
       
       textlog <- textCtrl p [wrap := WrapNone, enabled := False]  -- use text control as logger
       textCtrlMakeLogActiveTarget textlog

       
       connectButton     <- button p [text := "Connect", on command := do {guiConnection boxedConn;}]       
       executeButton     <- button p [text := "Execute", on command := do {loop =<< readMVar boxedConn;}]
       exitButton     <- button p [text := "Exit", on command := do { finishConnection boxedConn; close f; }] --TODO finish conn?????
       
       logMessage "logging enabled"          

       -- grids
       g <- gridCtrl p []
       gridSetGridLineColour g (colorSystem Color3DFace)
       gridSetCellHighlightColour g black
       appendColumns g (head names)
       appendRows    g (map show [1..length (tail names)])
       mapM_ (setRow g) (zip [0..] (tail names))
       gridAutoSize g
       
       -- layout
       set f [layout := container p $ column 5 [fill (dynamic (widget g))
                                 ,row 0 [widget connectButton, widget exitButton, widget executeButton]
                                 ,hfill $ minsize (sz 200 100) $ widget textlog
                                 
                                               ]
             ]       
       focusOn g
       set f [visible := True]  -- reduce flicker at startup.
       return ()

  where
    
    finishConnection :: MVar Database.PostgreSQL.LibPQ.Connection -> IO ()
    finishConnection connection
      = do
      con <- tryReadMVar connection
      case con of
        Just x -> do
          finish x
          println "Connection closed successfully"
        Nothing -> pure ()

names :: [[String]]
names
  = [["First Name", "Last Name", "INTEGER"]
    ,["Daan","Leijen", "1"],["Arjan","van IJzendoorn", "2"]
    ,["Martijn","Schrage", "3"],["Andres","Loh", "4"]]


setRow :: Grid a -> (Int, [String]) -> IO ()
setRow g (row_, values)
  = mapM_ (\(col,value_) -> gridSetCellValue g row_ col value_) (zip [0..] values)



gridCtrl :: Window a -> [Prop (Grid ())] -> IO (Grid ())
gridCtrl parent_ props_
  = feed2 props_ 0 $
    initialWindow $ \id_ rect' -> \props' flags ->
    do g <- gridCreate parent_ id_ rect' flags
       gridCreateGrid g 0 0 0
       set g props'
       gridEnableEditing g False
       return g

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

