# PostgreSQL Client on Haskell

Simple shell-client and GUI-client (in the future) for your PostgreSQL database.

Use `db-setup.sh` bash script which will:
* launch docker container (with name of `container_name` constant and using image name from `image_name` constant),
* add your connection to `~/.pgpass` file (with {database, user, password} = `docker`),
* launch `psql` with `-f` using `commands_for_sql` constant (`./db-setup-commands.sql` is set to default),
* setup connections in `Main.hs` files from `cmd` and `gui` using `db-setter.sh`,
* build both projects using `stack build`,
* open new terminal tab for You (`gnome-terminal`).

Then You can work with your database using our client (`stack exec cmd` or `stack exec gui`).

Also you can only use `db-setter.sh` to set your connection's settings in `Main.hs`:
```
./db-setter.sh <host> <port> <db-name> <username> <password> <mode>
```
> For `<mode>` choose `cmd` or `gui`, it's specifies which client mode you want to setup
___
### wxWidgets library is required
* Installation on Ubuntu:
```
sudo apt-get install libwxgtk3.0-dev
sudo apt-get install libwxgtk3.0-0v5-dbg
sudo apt-get install libwxgtk-media3.0
sudo apt-get install libwxgtk-webview3.0
```
___
### Docker usage helper:
* First you need to run your docker container with name `pg_test` using image `eg_postgresql`:
 ```
sudo docker run --rm -P --name pg_test eg_postgresql
```
> Remember that `--rm` deletes all data from container after you shut it down.

* Second launch your local psql-client to fill in your database quickly:
```
psql -h 0.0.0.0 -p 32768 -d docker -U docker --password
```
> In our case docker is on localhost, it uses standart port `32768`, database name is `docker` same as user name.

* After launching client, simply insert data.
* Now setup db connection in Main.hs using `db-setter.sh`:
```
./db-setter.sh 0.0.0.0 32768 docker docker docker cmd
```
___
### Work in progress:
* GUI
