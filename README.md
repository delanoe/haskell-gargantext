# Gargantext Haskell

## About this project

Gargantext is a collaborative web platform for the exploration of sets
of unstructured documents. It combines tools from natural language
processing, text-mining, complex networks analysis and interactive data
visualization to pave the way toward new kinds of interactions with your
digital corpora.

This software is a free software, developed by the CNRS Complex Systems
Institute of Paris Île-de-France (ISC-PIF) and its partners.

## Installation

Disclaimer: this project is still on development, this is work in
progress. Please report and improve this documentation if you encounter
issues.

### Build Core Code

#### Docker

``` sh
curl -sSL https://gitlab.iscpif.fr/gargantext/haskell-gargantext/raw/master/devops/docker/docker-install | sh
```

#### Debian

``` sh
curl -sSL https://gitlab.iscpif.fr/gargantext/haskell-gargantext/raw/master/devops/debian/install | sh
```

### Add dependencies

1. CoreNLP is needed (EN and FR); This dependency will not be needed
   soon.

``` sh
./devops/install-corenlp
```

2. Louvain C++ needed to draw the socio-semantic graphs

NOTE: This is already added in the Docker build.

``` sh
git clone https://gitlab.iscpif.fr/gargantext/clustering-louvain-cplusplus.git
cd clustering-louvain-cplusplus
./install
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

mv gargantext.ini_toModify gargantext.ini

(.gitignore avoids adding this file to the repository by mistake)


##### Run Gargantext

Users have to be created first (`user1` is created as instance):

``` sh
stack install
~/.local/bin/gargantext-init "gargantext.ini"
```

For Docker env, first create the appropriate image:

``` sh
cd devops/docker
docker build -t fpco/stack-build:lts-14.27-garg .
```

then run:

``` sh
stack --docker run gargantext-init -- gargantext.ini
```

### Importing data

You can import some data with:
``` sh
docker run --rm -it -p 9000:9000 cgenie/corenlp-garg
stack exec gargantext-import -- "corpusCsvHal" "user1" "IMT3" gargantext.ini 10000 ./1000.csv
```

## Use Cases

### Multi-User with Graphical User Interface (Server Mode)

``` sh
~/.local/bin/stack --docker exec gargantext-server -- --ini "gargantext.ini" --run Prod
```

Then you can log in with `user1:1resu`.


### Command Line Mode tools

#### Simple cooccurrences computation and indexation from a list of Ngrams

stack --docker exec gargantext-cli -- CorpusFromGarg.csv ListFromGarg.csv Ouput.json


Haskell fused-effects-profile
https://www.simplehaskell.org/
## REPL tips

Some tips to use the Haskell REPL.

### Query database for node

``` haskell
:set -XFlexibleContexts
:m + Data.Aeson
:m + Data.Proxy
let getN n = getNode n  :: Cmd GargError (Gargantext.Core.Types.Node Value)
runCmdRepl $ getN <id>
```
