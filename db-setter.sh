#! /bin/bash

# $1 - host
# $2 - port
# $3 - dbname
# $4 - user
# $5 - password

if [[ $# -ne 5 ]]
then
  echo "Wrong amount of parameters! Exiting..."
  exit
fi

new_line="\"host='$1' port=$2 dbname='$3' user='$4' password='$5'\""
main_path=./src/Main.hs

if sed -i "s/.*conn <- connectdb.*/   conn <- connectdb $new_line/" $main_path
then
  echo "Done!"
  exit 0
else
  echo "New parameters weren't set!"
  echo "Maybe bad path to Main.hs or troubles with rwx rights!"
  exit 1
fi

