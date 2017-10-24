# PostgreSQL Client on Haskell

Simple shell-client and GUI-client (in the future) for your PostgreSQL database.


### WXWIDGETS library is required
>On ubuntu
```
sudo apt-get install libwxgtk3.0-dev
sudo apt-get install libwxgtk3.0-0v5-dbg
sudo apt-get install libwxgtk-media3.0
sudo apt-get install libwxgtk-webview3.0
```

Use provided bash script to set your connection's settings:
```
./db-setter.sh <host> <port> <db-name> <username> <password>
```

### Docker usage helper:
* First you need to run your docker container with name 'pg_test' using image 'eg_postgresql':
 ```
sudo docker run --rm -P --name pg_test eg_postgresql
```
>Remember that `--rm` deletes all data from container after you shut it down.

* Second launch your local psql-client to fill in your database quickly:
```
psql -h 0.0.0.0 -p 32768 -d docker -U docker --password
```
>In our case docker is on localhost, it uses standart port 32768 (32769 sometimes), it has user 'docker' with password 'docker'.

* After launching client, simply copy-past data from `db-setup-commands.txt`.
* Now setup db connection in Main.hs using `db-setter.sh`:
```
./db-setter.sh 0.0.0.0 32768 docker docker docker
```

  
### Work in progress:
* GUI
