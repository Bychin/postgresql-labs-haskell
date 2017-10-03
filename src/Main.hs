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
      println $ "command status = " ++ show exec_status 
      case exec_status of
        FatalError ->
          --resultErrorMessage result >>= \(Just x) -> (println . show)x --bad show
          pure ()
        TuplesOk -> do
          rowNum <- ntuples result
          colNum <- nfields result
          
          {-let ij = [0,1..rowNum-1] >>= 
                \i -> [0,1..colNum-1] >>=
                \j -> [(i,j)] 
          {- same as: let ij = [(i,j) | i <- [0,1..rowNum-1], j <- [0,1..colNum-1]] -}

          let i_col = [i | i <- [0,1..colNum-1]]
          --mapM_ (\i -> putStrLn . show =<< BS.unpack $ fname result i) i_col
          mapM_ (\i-> fname result i >>= \(Just x) -> putStr $ BS.unpack x) i_col
          putStrLn "\n"
          
          --mapM_ (\(i,j)-> println . show =<< getvalue result i j) ij
          mapM_ (\(i,j)-> getvalue result i j >>= \(Just x) ->
                    if (j /= colNum - 1) then putStr $ BS.unpack x else putStrLn $ BS.unpack x) ij -}

          let
            mkListCol i = mapM (\j -> getvalue result j i >>= \(Just x) -> pure $ BS.unpack x) [0,1..rowNum-1]
            mkList i = if colNum /= i  --usage mkList 0, returns table in this form : [[elems of col 0], [elems of col 1], ...]
              then do
                next <- mkList (i + 1)
                a <- mkListCol i
                pure (a : next)
              else pure []


            mkListHead i = do        --same as mkList but for entry names : [[name0], [name1], ...]
              if colNum /= i then do
                _a <- fname result i >>= \(Just x)-> pure $ BS.unpack x
                _b <- mkListHead (i+1)
                pure $ [_a] : _b
              else
                pure []

            lengthsFun [] = [] --returns list of this form : [max len of elems of column 0, max len of elems of column 1, ...]
            lengthsFun (x : xs) = (length $ maximumBy (comparing length) x) : lengthsFun xs

            putSpaces 0 = pure ()
            putSpaces n = do {putStr " "; putSpaces (n-1)}

          _list <- mkList 0
          let lens_ =  lengthsFun _list

          let _CONST_SPACES = 5
          
          let
            printer1 [] [] = pure []
            printer1 (x : xs){-table-} (l : ls){-lengths-} =
              case x of
                (e : es) -> do
                  putStr e
                  let elen = length e
                  putSpaces (l - elen + _CONST_SPACES) 
                  
                  next <- printer1 xs ls
                  pure $ es : next
                [] -> pure []

            printer [] = pure ()
            printer table = do
              next <- printer1 table lens_
              putStr "\n"
              case next of
                xs@(x : xs') -> printer xs
                [] -> printer [] 

          printer =<< mkListHead 0
          printer _list

          pure ()
        _ -> pure ()
      
    Nothing ->
      println "no result"
  
  pure ()
  

loop conn = do
  println $ "Enter your query or 'exit'"
  line <- getLine
  println $ "you entered: " ++ line

  if line == "exit" then pure () else do {process line conn; loop conn}
      
      

main :: IO ()
main = do
   conn <- connectdb "host='195.19.32.74' port=5432 dbname='fn1132_2017' user='student' password='bmstu'" --LOL it's on github :-P
   checked <- status conn
   case checked of
     ConnectionOk -> do
       println "connected to db"
       loop conn
     _ -> do
       println "connection failed"
   
