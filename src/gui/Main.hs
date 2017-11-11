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


import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Event)

process line conn handleTable{-[[String]] -> IO-} = do
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
            mkContentRow i = mapM (\j -> getvalue result i j >>= \(Just x) -> pure $ BS.unpack x) [0,1..colNum-1]
            mkContent' i = if rowNum /= i
              then do
                _a <- mkContentRow i
                _b <- mkContent' (i+1)
                pure (_a : _b)
              else pure []

           
            
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


          handleTable =<< mkContent' 0 


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
  

loop conn handler = do
  println $ "Enter your query or 'exit'"
  line <- getLine
  println $ "You entered: " ++ line

  if line == "exit" then pure () else do {process line conn handler; loop conn handler}
      
      

main :: IO ()
main = do 
    --conn <- connectdb ""
    myConn <- newEmptyMVar
    start $ gui myConn
    --finish conn

guiConnection :: MVar Database.PostgreSQL.LibPQ.Connection -> IO () -- return bool for logger from checkConnection
guiConnection boxedConn = do 
    f             <- frame       [text := "Setup connection"]
    p             <- panel     f []
    okButton      <- button    p [text := "Ok"]
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

    set okButton [on command := do 
        connectDatabase boxedConn (get hostInput text)
                                  (get portInput text)
                                  (get dbnameInput text)
                                  (get userInput text)
                                  (get passwordInput text)
        checkConnection boxedConn f]

    set f [defaultButton := okButton
          ,layout := container p $
                     margin 5 $
                     floatCentre $ column 0 [boxed "Connection" (grid 5 10 [[label "Host:",     hfill $ widget hostInput]
                                                                           ,[label "Port:",     hfill $ widget portInput]
                                                                           ,[label "DB Name:",  hfill $ widget dbnameInput]
                                                                           ,[label "User:",     hfill $ widget userInput]
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
        putMVar conn newConn
        return ()

    checkConnection :: MVar Database.PostgreSQL.LibPQ.Connection -> Frame () -> IO () -- return bool for logger
    checkConnection connection frame = do
        checked <- readMVar connection >>= status
        case checked of
          ConnectionOk -> do
            println "Succesfully connected to db"
            close frame
            --readMVar connection >>= status
          _ -> do
            println "Connection failed!"

    {-setupHost :: String -> IO ()
    setupHost filePath = do
        answer <- lines =<< readFile filePath
        println answer
        return ()-}


gui :: MVar Database.PostgreSQL.LibPQ.Connection -> IO ()
gui boxedConn = do 
    f             <- frame [text := "PostgreSQL Client", visible := False] 
    p1            <- panel f [visible := False] -- TODO rename
    p             <- panel  f []
    textlog       <- textCtrl p [wrap := WrapNone, enabled := False] -- use text control as logger
    textCtrlMakeLogActiveTarget textlog
    logMessage "Logging was enabled"

    g             <- gridCtrl p1 []
    gridSetGridLineColour g (colorSystem Color3DFace)
    --gridSetCellHighlightColour g black

    --appendColumns g (head names);                        --  / Init insertion, TODO remove this
    --appendRows    g (map show [1..length (tail names)]); -- /
    --mapM_ (setRow g) (zip [0..] (tail names));           --/ 

    tablesText    <- staticText p []
    queryInput    <- textEntry p [text := ""]
    connectButton <- button p [text := "Connect", on command := do { guiConnection boxedConn; value <- getTablesList boxedConn; set tablesText [text := value]; }]       
    exitButton    <- button p [text := "Exit", on command := do { finishConnection boxedConn; close f; }]
    clearButton   <- button p [text := "Clear", on command := do { clearAllGrid g p1; set tablesText [text := ""]; logMessage "Grid was cleared"; }] -- better deletion of the table ???
    executeButton <- button p [text := "Execute"]

    executeButton2 <- button p [text := "Execute2", on command := do { -- TODO remove this
        clearAllGrid g p1;
        appendColumns g (head names);
        appendRows    g (map show [1..length (tail names)]);
        mapM_ (setRow g) (zip [0..] (tail names));  
        gridAutoSize g;
        showPanel p1;
    }] --          

    set executeButton [on command := do { 
        --con <- readMVar boxedConn; loop con (\_ -> pure ())
        clearAllGrid g p1;

        -- execute query, it's format should be [[String]], every element [] - row of the table, head - col's names
        -- result <- executeQuery boxedConn (get queryInput text) -- TODO finish this func

        appendColumns g (head names2);
        appendRows    g (map show [1..length (tail names2)]);
        mapM_ (setRow g) (zip [0..] (tail names2));  

        gridAutoSize g;
        showPanel p1;
        set f [ clientSize := sz 400 599 ];
        value <- getTablesList boxedConn;
        set tablesText [text := value];
    }]

    set f [ layout := grid 5 5 [
                    [ container p $ alignBottom $
                      column 5 [--fill (dynamic (widget g)),
                                row 0 [ widget connectButton
                                      , widget exitButton
                                      , widget executeButton
                                      , widget clearButton 
                                      , widget executeButton2 ] -- TODO remove this
                               ,row 1 [ hfill $ widget queryInput ] 
                               ,row 2 [ widget tablesText ]
                               ,hfill $ minsize (sz 200 100) $ widget textlog   
                               ]
                    ],
                    [ container p1 $ column 5 [fill $ dynamic $ widget g] ]
                      ]
          , clientSize := sz 400 600]       
    focusOn g
    set f [visible := True]  -- reduce flicker at startup.
    return ()

  where
    finishConnection :: MVar Database.PostgreSQL.LibPQ.Connection -> IO ()
    finishConnection connection = do
        conn <- tryReadMVar connection
        case conn of
          Just x -> do
            finish x
            println "Connection was closed successfully"
          Nothing -> pure ()

    hidePanel :: Panel () -> IO ()
    hidePanel panel = set panel [ visible := False ]

    showPanel :: Panel () -> IO ()
    showPanel panel = set panel [ visible := True ]

    clearAllGrid :: Grid () -> Panel () -> IO ()
    clearAllGrid grid panel = do
        gridClearGrid grid
        hidePanel panel
        colsNum <- gridGetNumberCols grid
        rowsNum <- gridGetNumberRows grid
        if colsNum > 0 && rowsNum > 0
            then do
                gridDeleteRows grid 0 (rowsNum + 1) True
                gridDeleteCols grid 0 (colsNum + 1) True
                return ()
            else return ()

    --executeQuery :: MVar Database.PostgreSQL.LibPQ.Connection -> IO String -> [[String]]
    --executeQuery connection query' = do
        --query <- query'

-- select tablename as table from pg_tables where schemaname = 'public';
    getTablesList :: MVar Database.PostgreSQL.LibPQ.Connection -> IO String
    getTablesList connection = do
        conn <- tryReadMVar connection
        case conn of
          Just x -> do
            let query = printf "select tablename as table from pg_tables where schemaname = 'public';"
            mresult <- exec x $ BS.pack query
            case mresult of
                Just result -> do
                    rowNum <- ntuples result
                    colNum <- nfields result
                    let mkContentCol i = mapM (\j -> getvalue result j i >>= \(Just x) -> pure $ BS.unpack x) [0,1..rowNum-1]
                    content <- pure . concat =<< mkContentCol 0
                    return content
                Nothing -> pure []
          Nothing -> pure [] 


    names :: [[String]] -- remove this
    names = 
        [ ["First Name", "Last Name", "INTEGER"]
        , ["Daan","Leijen", "1"]
        , ["Arjan","van IJzendoorn", "2"]
        , ["Martijn","Schrage", "3"]
        , ["Andres","Loh", "5"]
        ]

    names2 :: [[String]] -- remove this
    names2 = 
        [ ["Header1","Header2", "Header3"]
        , ["This","is", "testdata"]
        , ["some","new", "text"]
        , ["this","row", "last"]
        ]

    gridCtrl :: Window a -> [Prop (Grid ())] -> IO (Grid ())
    gridCtrl parent_ props_
      = feed2 props_ 0 $
        initialWindow $ \id_ rect' -> \props' flags -> do 
            g <- gridCreate parent_ id_ rect' flags
            gridCreateGrid g 0 0 0
            set g props'
            gridEnableEditing g False
            return g

    setRow :: Grid a -> (Int, [String]) -> IO ()
    setRow g (row_, values)
        = mapM_ (\(col,value_) -> gridSetCellValue g row_ col value_) (zip [0..] values)

    appendColumns :: Grid a -> [String] -> IO ()
    appendColumns _g []  = return ()
    appendColumns  g labels = do 
        n <- gridGetNumberCols g
        _ <- gridAppendCols g (length labels) True
        mapM_ (\(i, label_) -> gridSetColLabelValue g i label_) (zip [n..] labels) -- setups col's names
        return ()

    appendRows :: Grid a -> [String] -> IO ()
    appendRows _g [] = return ()
    appendRows  g labels = do 
        n <- gridGetNumberRows g
        _ <- gridAppendRows g (length labels) True
        --mapM_ (\(i, label_) -> gridSetRowLabelValue g i label_) (zip [n..] labels) -- row's names ???
        return ()
