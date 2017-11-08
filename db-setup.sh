#! /bin/bash

image_name=my_postgresql
container_name=pg_test
commands_for_sql=./db-setup-commands.sql
termination="sudo docker stop $container_name"

RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[1;32m'
NC='\033[0m' # No color

if sudo docker run --rm -P -d --name $container_name $image_name 1> /dev/null # -d Background launch
then
  echo -e "${GREEN}[1] Docker was launched...${NC}"
  port=$(sudo docker ps | grep pg_test | grep ":[0-9][0-9]*->" -o | grep "[0-9][0-9]*" -o) # Find port adress
  host=$(sudo docker ps | grep pg_test | grep "[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*" -o) # Find host adress
else
  echo -e "${RED}ERROR: Cannot launch docker! Exiting!${NC}"
  $termination 1> /dev/null
  exit 1
fi

if echo "$host:$port:docker:docker:docker" >> ~/.pgpass; chmod 0600 ~/.pgpass # Auto-logging
then
  echo -e "${GREEN}[2] Auto-logging was enabled...${NC}"
else
  echo -e "${RED}ERROR: Cannot make file or execute 'chmod' command! Exiting!${NC}"
  $termination 1> /dev/null
  exit 1
fi

echo -e "${GREEN}[3] Launching PSQL client...${NC}"
sleep 3
if psql -h $host -p $port -d docker -U docker -w -f $commands_for_sql 1> /dev/null # No need in password because of .pgpass file
then
  echo -e "${GREEN}[4] All data was added...${NC}"
  echo -e "${GREEN}[5] Client connection was closed...${NC}"
else
  echo -e "${RED}ERROR: Cannot launch client! Exiting!${NC}"
  $termination 1> /dev/null
  exit 1
fi

prop_file="properties.txt"
touch $prop_file
printf "$host\n$port" > $prop_file


setter1="./db-setter.sh $host $port docker docker docker cmd"
#setter2="./db-setter.sh $host $port docker docker docker gui"
if $setter1 #; $setter2
then
  echo -e "${GREEN}[6] Main.hs was set up...${NC}"
else
  echo -e "${RED}ERROR: Troubles with db-setter.sh! Exiting!${NC}"
  $termination 1> /dev/null
  exit 1
fi

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
    rm $prop_file
    echo -e "${GREEN}[8] Stopping docker container...${NC}"
    $termination 1> /dev/null
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

