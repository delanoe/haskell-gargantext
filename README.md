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
  curl -sSL https://gitlab.iscpif.fr/gargantext/haskell-gargantext/raw/master/devops/docker-install | sh

#### Debian
  curl -sSL https://gitlab.iscpif.fr/gargantext/haskell-gargantext/raw/master/devops/debian-install | sh

### Add dependencies

1. CoreNLP is needed (EN and FR); This dependency will not be needed
   soon.
  - wget https://dl.gargantext.org/coreNLP.tar.bz2
  - tar xvjf coreNLP.tar.bz2
  - ./startServer.sh

2. Louvain C++ needed to draw the socio-semantic graphs
  - git clone https://gitlab.iscpif.fr/gargantext/clustering-louvain-cplusplus.git
  - cd clustering-louvain-cplusplus
  - ./install

### Initialization

Users has to be created first
1. stack ghci
2. runCmd insertUsersDemo
Then you can log in with user1:1resu

## Use Cases

### Multi-User with Graphical User Interface (Server Mode)

~/.local/bin/stack --docker exec gargantext-server -- --ini "gargantext.ini" --run Prod


### Command Line Mode tools

#### Simple cooccurrences computation and indexation from a list of Ngrams

stack --docker exec gargantext-cli -- CorpusFromGarg.csv ListFromGarg.csv Ouput.json

