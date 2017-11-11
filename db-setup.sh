#! /bin/bash

image_name=my_postgresql
container_name=pg_test
commands_for_sql=./db-setup-commands.sql
prop_file="properties.tmp"
termination="sudo docker stop $container_name"

RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[1;32m'
NC='\033[0m' # No color



# Launching docker container
if sudo docker run --rm -P -d --name $container_name $image_name 1> /dev/null # -d Background launch
then
  echo -e "${GREEN}[1] Docker was launched...${NC}"
  port=$(sudo docker ps | grep $container_name | grep ":[0-9][0-9]*->" -o | grep "[0-9][0-9]*" -o) # Find port adress
  host=$(sudo docker ps | grep $container_name | grep "[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*" -o) # Find host adress
  echo "$host\n$port" > $prop_file
else
  echo -e "${RED}ERROR: Cannot launch docker! Exiting!${NC}"
  $termination 1> /dev/null
  rm $prop_file 2> /dev/null
  exit 1
fi


# Updating .pgpass file for auto-logging in psql
if echo "$host:$port:docker:docker:docker" >> ~/.pgpass; chmod 0600 ~/.pgpass # Auto-logging
then
  echo -e "${GREEN}[2] Auto-logging was enabled...${NC}"
else
  echo -e "${RED}ERROR: Cannot make file or execute 'chmod' command! Exiting!${NC}"
  $termination 1> /dev/null
  rm $prop_file 2> /dev/null
  exit 1
fi


# Exporting data from commands_for_sql file to database through psql
echo -e "${GREEN}[3] Launching PSQL client...${NC}"
sleep 3
if psql -h $host -p $port -d docker -U docker -W -f $commands_for_sql 1> /dev/null # No need in password because of .pgpass file
then
  echo -e "${GREEN}[4] All data was added...${NC}"
  echo -e "${GREEN}[5] Client connection was closed...${NC}"
else
  echo -e "${RED}ERROR: Cannot launch client! Exiting!${NC}"
  $termination 1> /dev/null
  rm $prop_file 2> /dev/null
  exit 1
fi


# Setting up the host port and etc in Main.hs files (will be removed in future)
setter1="./db-setter.sh $host $port docker docker docker cmd"
#setter2="./db-setter.sh $host $port docker docker docker gui"
if $setter1 1> /dev/null #; $setter2
then
  echo -e "${GREEN}[6] Main.hs was set up...${NC}"
else
  echo -e "${RED}ERROR: Troubles with db-setter.sh! Exiting!${NC}"
  $termination 1> /dev/null
  rm $prop_file 2> /dev/null
  exit 1
fi


# Building projects because of previous step (will be also removed)
if stack build
then
  echo -e "${GREEN}[7] Main.hs was built...${NC}"
  if gnome-terminal
  then
    echo ""
  else
    echo -e "${YELLOW}WARNING: Cannot open new terminal tab${NC}"
  fi
else
  echo -e "${YELLOW}WARNING: Cannot build Main.hs${NC}"
fi


echo "Enter 'stop' to stop container."
while [[ 1 -eq 1 ]]
do
  read decision
  if [[ $decision == "stop" ]]
  then
    echo -e "${GREEN}[8] Stopping docker container...${NC}"
    $termination 1> /dev/null
    rm $prop_file 2> /dev/null
    sleep 1
    if sed -i "/$host:$port:docker:docker:docker/d" ~/.pgpass
    then
      echo -e "${GREEN}[9] Auto-logging was disabled...${NC}"
    else
      echo -e "${YELLOW}WARNING: Couldn't undo auto-logging!${NC}"
    fi
    echo "Goodbye!"
    exit
  fi
done
