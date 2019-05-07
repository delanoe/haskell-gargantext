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

### Docker
curl -sSL https://gitlab.iscpif.fr/gargantext/haskell-gargantext/raw/master/devops/docker-install | sh

### Debian
curl -sSL https://gitlab.iscpif.fr/gargantext/haskell-gargantext/raw/master/devops/debian-install | sh


## Use Cases

### Multi-User with Graphical User Interface (Server Mode)

~/.local/bin/stack --docker exec gargantext-server -- --ini "gargantext.ini" --run Prod


### Command Line Mode tools

#### Simple cooccurrences computation and indexation from a list of Ngrams

stack --docker exec gargantext-cli -- CorpusFromGarg.csv ListFromGarg.csv Ouput.json

