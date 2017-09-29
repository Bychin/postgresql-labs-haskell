{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes#-}


module Main where

import Prelude
import qualified Prelude as Prelude
import qualified Data.ByteString.Char8 as BS
import Control.Exception
import Database.PostgreSQL.LibPQ
import Common
import Control.Monad


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
          
          let ij = [0,1..rowNum-1] >>= 
                \i -> [0,1..colNum-1] >>=
                \j -> [(i,j)] 
          {- same as: let ij = [(i,j) | i <- [0,1..rowNum-1], j <- [0,1..colNum-1]] -}
          
          mapM_ (\(i,j)-> println . show =<< getvalue result i j) ij
             
       

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
   
