# Gargantext with Haskell (Backend instance)

## About the project

GarganText is a collaborative web-decentralized-based macro-service
platform for the exploration of unstructured texts. It combines tools
from natural language processing, text-data-mining tricks, complex
networks analysis algorithms and interactive data visualization tools to
pave the way toward new kinds of interactions with your digital corpora.

This software is free software, developed and offered by the CNRS
Complex Systems Institute of Paris Île-de-France (ISC-PIF) and its
partners.

GarganText Project: this repo builds the
backend for the frontend server built by
[backend](https://gitlab.iscpif.fr/gargantext/haskell-gargantext).


## Installation

Disclaimer: this project is still in development, this is work in
progress. Please report and improve this documentation if you encounter issues.

### Stack setup

You need to install stack first:

```shell
curl -sSL https://get.haskellstack.org/ | sh
```

Verify the installation is complete with
```shell
stack --version
```

### With Nix setup

First install [nix](https://nixos.org/download.html): 

```shell
$ sh <(curl -L https://nixos.org/nix/install) --daemon
```

Verify the installation is complete
```shell
$ nix-env --version
nix-env (Nix) 2.3.12
```
And just build:
``` sh
stack --nix build --fast
```

### Build Core Code

NOTE: Default build (with optimizations) requires large amounts of RAM
(16GB at least). To avoid heavy compilation times and swapping out your
machine, it is recommended to `stack build` with the `--fast-` flag,
i.e.:

``` sh
stack --nix build --fast
```
or

``` sh
stack --docker build --fast
```

#### Docker

``` sh
curl -sSL https://gitlab.iscpif.fr/gargantext/haskell-gargantext/raw/dev/devops/docker/docker-install | sh
```

#### Debian

``` sh
curl -sSL https://gitlab.iscpif.fr/gargantext/haskell-gargantext/raw/dev/devops/debian/install | sh
```

#### Ubuntu

``` sh
curl -sSL https://gitlab.iscpif.fr/gargantext/haskell-gargantext/raw/dev/devops/ubuntu/install | sh
```

### Add dependencies

1. CoreNLP is needed (EN and FR); This dependency will not be needed soon.

``` sh
./devops/install-corenlp
```
### Launch Gargantext (It should have been initialized first)
Run
haskell-gargantext/devops/docker$ docker-compose up

Then 
``` sh
stack --nix install
~/.local/bin/gargantext-init "gargantext.ini"
```
Or for Docker env, first create the appropriate image:

``` sh
cd devops/docker
docker build -t cgenie/stack-build:lts-18.12-garg .
```

then run:

``` sh
stack --docker exec gargantext-init -- gargantext.ini
```

### Initialization

#### Docker

Run PostgreSQL first:

``` sh
cd devops/docker
docker-compose up
```

Initialization schema should be loaded automatically (from `devops/postgres/schema.sql`).

#### Gargantext

##### Fix the passwords

Change the passwords in gargantext.ini_toModify then move it:

``` sh
cp gargantext.ini_toModify gargantext.ini
```
(`.gitignore` avoids adding this file to the repository by mistake)


##### Run Gargantext

Users have to be created first (`user1` is created as instance):

``` sh
stack --nix install
~/.local/bin/gargantext-init "gargantext.ini"
```

For Docker env, first create the appropriate image:

``` sh
cd devops/docker
docker build -t cgenie/stack-build:lts-18.12-garg .
```

then run:

``` sh
stack --docker exec gargantext-init -- gargantext.ini
```

### Importing data

You can import some data with:
``` sh
docker run --rm -it -p 9000:9000 cgenie/corenlp-garg
stack exec gargantext-import -- "corpusCsvHal" "user1" "IMT3" gargantext.ini 10000 ./1000.csv
```

### Nix

It is also possible to build everything with [Nix](https://nixos.org/) instead of Docker:
``` sh
stack --nix build
stack --nix exec gargantext-import -- "corpusCsvHal" "user1" "IMT3" gargantext.ini 10000 ./1000.csv
stack --nix exec gargantext-server -- --ini gargantext.ini --run Prod
```

## Use Cases

### Multi-User with Graphical User Interface (Server Mode)

``` sh
~/.local/bin/stack --docker exec gargantext-server -- --ini "gargantext.ini" --run Prod
```

Then you can log in with `user1` / `1resu`.


### Command Line Mode tools

#### Simple cooccurrences computation and indexation from a list of Ngrams

``` sh
stack --docker exec gargantext-cli -- CorpusFromGarg.csv ListFromGarg.csv Ouput.json
```

### Analyzing the ngrams table repo

We store the repository in directory `repos` in the [CBOR](https://cbor.io/)
file format. To decode it to JSON and analyze, say, using
[jq](https://shapeshed.com/jq-json/), use the following command:

``` sh
cat repos/repo.cbor.v5 | stack --nix exec gargantext-cbor2json | jq .
```
### Documentation

To build documentation, run:

```sh
stack --docker build --haddock --no-haddock-deps --fast
```

(in `.stack-work/dist/x86_64-linux-nix/Cabal-3.2.1.0/doc/html/gargantext`).

## GraphQL

Some introspection information.

Playground is located at http://localhost:8008/gql

### List all GraphQL types in the Playground

```
{
  __schema {
    types {
      name
    }
  }
}
```

### List details about a type in GraphQL

```
{
  __type(name:"User") {
  	fields {
    	name
      description
      type {
        name
      }
  	}
	}
}
```
## PostgreSQL

### Upgrading using Docker

https://www.cloudytuts.com/tutorials/docker/how-to-upgrade-postgresql-in-docker-and-kubernetes/

To upgrade PostgreSQL in Docker containers, for example from 11.x to 14.x, simply run:
```sh
docker exec -it <container-id> pg_dumpall -U gargantua > 11-db.dump
```

Then, shut down the container, replace `image` section in
`devops/docker/docker-compose.yaml` with `postgres:14`. Also, it is a good practice to create a new volume, say `garg-pgdata14` and bind the new container to it. If you want to keep the same volume, remember about removing it like so:
```sh
docker-compose rm postgres
docker volume rm docker_garg-pgdata
```

Now, start the container and execute:
```sh
# need to drop the empty DB first, since schema will be created when restoring the dump
docker exec -i <new-container-id> dropdb -U gargantua gargandbV5
# recreate the db, but empty with no schema
docker exec -i <new-container-id> createdb -U gargantua gargandbV5
# now we can restore the dump
docker exec -i <new-container-id> psql -U gargantua -d gargandbV5 < 11-db.dump
```

### Upgrading using 

There is a solution using pgupgrade_cluster but you need to manage the
clusters version 14 and 13. Hence here is a simple solution to upgrade.

First save your data:
```
sudo su postgres
pg_dumpall > gargandb.dump
```

Upgrade postgresql:
```
sudo apt install postgresql-server-14 postgresql-client-14
sudo apt remove --purge postgresql-13
```
Restore your data:
```
sudo su postgres
psql < gargandb.dump
```

Maybe you need to restore the gargantua password
```
ALTER ROLE gargantua PASSWORD 'yourPasswordIn_gargantext.ini'
```
Maybe you need to change the port to 5433 for database connection in
your gargantext.ini file.




