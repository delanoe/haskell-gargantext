<div align="center"><img height="180" src="https://gitlab.iscpif.fr/gargantext/main/images/logo.png"></div>

&nbsp;
# Gargantext with Haskell (Backend instance)

![Haskell](https://img.shields.io/badge/Code-Haskell-informational?style=flat&logo=haskell&color=6144b3)&nbsp;&nbsp;![Stack](https://img.shields.io/badge/Tools-Stack-informational?style=flat&logo=&color=6144b3)&nbsp;&nbsp;![GHC](https://img.shields.io/badge/Tools-GHC-informational?style=flat&logo=&color=2E677B)&nbsp;&nbsp;![Nix](https://img.shields.io/badge/Package%20manager-Nix-informational?style=flat&logo=debian&color=6586c8)&nbsp;&nbsp;![Docker](https://img.shields.io/badge/Tools-Docker-informational?style=flat&logo=docker&color=003f8c)

#### Table of Contents
1. [About the project](#about)
2. [Example2](#example2)
3. [Third Example](#third-example)
4. [Fourth Example](#fourth-examplehttpwwwfourthexamplecom)

## About the project  <a name="about"></a>

GarganText is a collaborative web-decentralized-based macro-service
platform for the exploration of unstructured texts. It combines tools
from natural language processing, text-data-mining bricks, complex
networks analysis algorithms and interactive data visualization tools
to pave the way toward new kinds of interactions with your textual and
digital corpora.

This software is free (as "Libre" in French) software, developed by the
CNRS Complex Systems Institute of Paris Île-de-France (ISC-PIF) and its
partners.

GarganText Project: this repo builds the backend for the frontend server built by
[backend](https://gitlab.iscpif.fr/gargantext/haskell-gargantext).


## Installation

Disclaimer: since this project is still in development, this document
remains in progress. Please report and improve this documentation if you
encounter any issues.

### Prerequisite

Clone the project.
```shell
git clone https://gitlab.iscpif.fr/gargantext/haskell-gargantext.git
cd haskell-gargantext
```
### 1. Install Stack

Install [Stack (or Haskell Tool Stack)](https://docs.haskellstack.org/en/stable/):

```shell
curl -sSL https://get.haskellstack.org/ | sh
```

Verify the installation is complete with
```shell
stack --version
Version 2.9.1
```

### 2. Install Nix

Install [Nix](https://nixos.org/download.html):

```shell
$ sh <(curl -L https://nixos.org/nix/install) --daemon
```

Verify the installation is complete with
```shell
$ nix-env --version
nix-env (Nix) 2.12.0
```

> **NOTE INFO (upgrade/downgrade if needed)**
> Gargantext works with Nix 2.12.0 (older version than current 2.13.2). To downgrade your Nix version:
> `nix-channel --update; nix-env -iA nixpkgs.nixVersions.nix_2_12 nixpkgs.cacert; systemctl daemon-reload; systemctl restart nix-daemon`
> Upgrading Nix: https://nixos.org/manual/nix/unstable/installation/upgrading.html


### 3. Build Core Code

NOTE: Default build (with optimizations) requires large amounts of RAM
(16GB at least). To avoid heavy compilation times and swapping out your
machine, it is recommended to `stack build` with the `--fast` flag,
i.e.:

``` sh
stack --nix build --fast
```

If the build is finishing without error, you are ready to launch
GarganText! See next step.

&nbsp;
&nbsp;
&nbsp;
&nbsp;
&nbsp;


### Initialization

Docker-compose will configure your database and some NLP bricks (such as CoreNLP):

``` sh
# If docker is not installed:
# curl -sSL https://gitlab.iscpif.fr/gargantext/haskell-gargantext/raw/dev/devops/docker/docker-install | sh
cd devops/docker
docker compose up
```
Initialization schema should be loaded automatically (from `devops/postgres/schema.sql`).

Then install:
``` sh
stack --nix install
```

Copy the configuration file:
``` sh
cp gargantext.ini_toModify gargantext.ini
```
Do not worry, `.gitignore` avoids adding this file to the repository by
mistake, then you can change the passwords in gargantext.ini safely.

Users have to be created first (`user1` is created as instance):
``` sh
~/.local/bin/gargantext-init "gargantext.ini"
```

Launch GarganText:
``` sh
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
stack --nix build --haddock --no-haddock-deps --fast
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




