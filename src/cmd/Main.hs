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
  conn <- connectdb "host='0.0.0.0' port=32779 dbname='docker' user='docker' password='docker'"
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
main = cmd
