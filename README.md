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

First install [nix](https://nixos.org/guides/install-nix.html): 

```shell
curl -sSL https://nixos.org/nix/install | sh
```

Verify the installation is complete
```shell
$ nix-env
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
mv gargantext.ini_toModify gargantext.ini
```
(`.gitignore` avoids adding this file to the repository by mistake)


##### Run Gargantext

Users have to be created first (`user1` is created as instance):

``` sh
stack install
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

