{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}


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
import Text.Printf -- printf
import Control.Concurrent.MVar

import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Event)


process line conn handleTable{-[[String]] -> IO-} = do
  mresult <- exec conn $ BS.pack line
  case mresult of
    Just result -> do
      exec_status <- resultStatus result
      logMessage $ "Query status: " ++ show exec_status 
      case exec_status of
        FatalError -> do
          pure ()
          resultErrorMessage result >>= \(Just x) -> (logMessage . BS.unpack)x --bad show
          
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

            mkHead'' i = do
              if colNum /= i then do
                _a <- fname result i >>= \(Just x) -> pure $ BS.unpack x
                _b <- mkHead'' (i + 1)
                pure $ _a : _b
              else
                pure []
            mkHead' i = do
              a <- mkHead'' i
              pure [a]

          head' <- mkHead' 0
          cont' <- mkContent' 0
          handleTable $ head' ++ cont'

          _content <- mkContent 0
          _head    <- mkHead 0

          pure ()
        _ -> pure ()
      
    Nothing ->
      logMessage "No result"
  pure ()
  

main :: IO ()
main = do 
    myConn <- newEmptyMVar
    start $ gui myConn


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
                content <- pure . (intercalate ", ") =<< mkContentCol 0
                return content
            Nothing -> pure []
      Nothing -> pure []


guiConnection :: MVar Database.PostgreSQL.LibPQ.Connection -> TextCtrl () -> IO ()
guiConnection boxedConn tablesText = do 
    f             <- frame       [text := "Setup connection"]
    p             <- panel     f []
    okButton      <- button    p [text := "Ok"]
    cancelButton  <- button    p [text := "Cancel", on command := close f]
    hostInput     <- textEntry p [                  alignment := AlignRight]
    portInput     <- textEntry p [                  alignment := AlignRight]
    dbnameInput   <- textEntry p [text := "docker", alignment := AlignRight]
    userInput     <- textEntry p [text := "docker", alignment := AlignRight]
    passwordInput <- textEntry p [text := "docker", alignment := AlignRight]

    setupHost "/tmp/properties.tmp" hostInput portInput

    set okButton [on command := do 
        connectDatabase boxedConn (get hostInput text)
                                  (get portInput text)
                                  (get dbnameInput text)
                                  (get userInput text)
                                  (get passwordInput text)
        checkConnection boxedConn f
        value <- getTablesList boxedConn
        set tablesText [text := ("Table's names: " ++ value)];]

    set f [defaultButton := okButton
          ,layout := container p $ margin 10 $ floatCenter $
                     column 5 [grid 10 5 [[label "Host:",    hfill $ widget hostInput]
                                         ,[label "Port:",     hfill $ widget portInput]
                                         ,[label "DB Name:",  hfill $ widget dbnameInput]
                                         ,[label "User:",     hfill $ widget userInput]
                                         ,[label "Password:", hfill $ widget passwordInput]]
                              ,row 4 [widget okButton, widget cancelButton]]
          ,clientSize := sz 100 200
          ]
    focusOn okButton
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
        newConn <- connectdb $ BS.pack connParams
        tryTakeMVar conn
        tryPutMVar conn newConn
        return ()

    checkConnection :: MVar Database.PostgreSQL.LibPQ.Connection -> Frame () -> IO ()
    checkConnection connection frame = do
        checked <- readMVar connection >>= status
        case checked of
          ConnectionOk -> do
            logMessage "Succesfully connected to db"
            close frame
            --readMVar connection >>= status
          _ -> do
            tryTakeMVar connection
            logMessage "Connection failed!"

    setupHost :: FilePath -> TextCtrl () -> TextCtrl () -> IO ()
    setupHost filePath hostInput_ portInput_ = do
        read <- pure . lines =<< readFile filePath
        -- <host>\n<port> format
        let index = findIndex (== '\\') (concat read)
        case index of
            Just index -> do
                set hostInput_ [text := take index $ concat read]
                set portInput_ [text := drop (index + 2) $ concat read]
            Nothing -> pure ()


gui :: MVar Database.PostgreSQL.LibPQ.Connection -> IO ()
gui boxedConn = do 
    f             <- frame [text := "PostgreSQL Client", visible := False] 
    pg            <- panel f [visible := False]
    p             <- panel f []

    textlog       <- textCtrl p [wrap := WrapNone]
    textCtrlMakeLogActiveTarget textlog
    logMessage "Logging was enabled"

    g             <- gridCtrl pg []
    gridSetGridLineColour g (colorSystem Color3DFace)
    gridSetCellHighlightColour g black

    tablesText    <- textCtrl p [wrap := WrapNone, enabled := False]
    queryInput    <- textCtrl p [wrap := WrapNone, text := ""]
    noteInput     <- textCtrl p [wrap := WrapNone, text := ""]
    connectButton <- button p [text := "Connect", on command := do { guiConnection boxedConn tablesText; }]       
    exitButton    <- button p [text := "Exit", on command := do { finishConnection boxedConn; close f; }]
    clearButton   <- button p [text := "Clear"]
    executeButton <- button p [text := "Execute"]

    set clearButton [on command := do
        clearAllGrid g pg
        set queryInput [text := ""]
        set f [ clientSize := sz 500 400 ]
        logMessage "Grid was cleared";
        ]

    set executeButton [on command := do 
        boxedConn' <- tryReadMVar boxedConn
        query <- get queryInput text
        appendText noteInput $ query ++ "\n"
        case boxedConn' of
          Just boxedConn' -> do
            logMessage query
            process query boxedConn' ( \table -> do
                clearAllGrid g pg
                gridSetRowLabelSize g 0

                appendColumns g (head table)
                appendRows    g (map show [1..length (tail table)])
                mapM_ (setRow g) (zip [0..] (tail table))

                gridAutoSize g
                showPanel pg
                set f [ clientSize := sz 500 700 ]
                                             )
            value <- getTablesList boxedConn
            set tablesText [text := ("Table's names: " ++ value)]
           
          Nothing -> return ()
        set queryInput [text := ""]
        ]

    set f [ layout := grid 5 5 [
                    [ container p $ alignBottom $
                      column 5 [row 5 [widget connectButton, widget clearButton, alignRight $ widget exitButton]
                               ,row 0 [hfill $ widget tablesText]
                               ,row 5 [hfill $ widget queryInput, widget executeButton]
                               ,row 0 [hfill $ minsize (sz 500 150) $ widget noteInput] 
                               ,row 0 [hfill $ minsize (sz 500 100) $ widget textlog]
                               ]
                    ],
                    [ container pg $ column 5 [fill $ dynamic $ widget g] ]
                      ]
          , clientSize := sz 500 400]       
    focusOn connectButton
    set f [visible := True]  -- reduce flicker at startup.
    return ()

  where
    finishConnection :: MVar Database.PostgreSQL.LibPQ.Connection -> IO ()
    finishConnection connection = do
        conn <- tryReadMVar connection
        case conn of
          Just x -> do
            finish x
            logMessage "Connection was closed successfully"
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
