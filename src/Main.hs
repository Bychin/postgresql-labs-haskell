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
        FatalError ->
          pure ()
          --resultErrorMessage result >>= \(Just x) -> (println . show)x --bad show
          
        TuplesOk -> do
          rowNum <- ntuples result
          colNum <- nfields result

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

            putPluses 0 = putStr " + "
            putPluses n = do {putStr "-"; putPluses (n-1)}

            putSpaces 0 = putStr "| "
            putSpaces n = do {putStr " "; putSpaces (n-1)}

          _list <- mkList 0
          let lens_ =  lengthsFun _list

          let _CONST_SPACES = 3
          
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

            printer2 [] [] = pure [] --bad realisation for nice pixel-graphic output (--- + --- +)
            printer2 (x : xs){-table-} (l : ls){-lengths-} =
              case x of
                (e : es) -> do

                  putPluses (l - 1 + _CONST_SPACES) 
                  
                  next <- printer2 xs ls
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
          printer2 _list lens_ --_list makes no sense except iteration amount
          putStrLn "" --should use this because of awful printer2
          printer _list

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
      
      

main :: IO ()
main = do
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
