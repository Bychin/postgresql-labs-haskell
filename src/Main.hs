{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes#-}


module Main where

import Prelude
import qualified Prelude as Prelude
import qualified Data.ByteString.Char8 as BS
import Control.Exception
import Database.PostgreSQL.LibPQ
import Common


process line conn = do
 

  --table_create_res <- query_ conn "create table test11112222(id integer)" :: IO [[Maybe String]]

  {-execute_ conn "insert into test11112222 values (2)"

  let trySel = do
         xs <- query_ conn "select * from test11112222" :: IO [[Maybe Int]]
         println . show $ xs

  result <- try ( trySel ) :: IO (Either SqlError ())
  case result of
    Left ex -> println $ show ex
    Right normal -> println "ok"-}

  mresult <- exec conn $ BS.pack line
  case mresult of
    Just result -> do
      exec_status <- resultStatus result
      println $ "command status = " ++ show exec_status 
      case exec_status of
        FatalError ->
          resultErrorMessage result >>= \(Just x) -> (println . show)x --bad show
        TuplesOk -> do
          rowNum <- ntuples result
          colNum <- nfields result
          
          let ij = [0,1..rowNum-1] >>= \i ->
            [0,1..colNum-1] >>= \j -> (i,j)
          
          _mapM (\(i,j)-> println . show =<< getvalue result i j) ij
             
       

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
   
