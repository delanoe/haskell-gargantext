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

### Front-End

Front-End is written in [Purescript](http://www.purescript.org/).
Then Gargantext Front-End is developed in another git repository. 
Before building the whole repo you need:

1. Get the source code:
    git clone https://gitlab.iscpif.fr/gargantext/purescript-gargantext.git
2. cd purescript-gargantext
3. read the README.md to install it


### Back-End

Back-End of Gargantext rely on several backends:
1. Haskell backend with orchestrator
2. Python backend
3. Others backends

Docker will be created to ease the backends installation.

## Haskell Backend

On Linux Debian (and Ubuntu?), install Dependencies.
sudo apt-get install libbz2-dev lipq-dev
(In the near future, we will use Nix.)

### Database: install, configure and populate 

1. Postgresql installation
``shell
sudo apt update && sudo apt install postgresql-9.6
sudo su postgres
psql
``

2. Configuration

first create your local database:
``sql
create role gargantua with password "see gargantext.ini";
create database gargandb with owner gargantua;
``

3. Populate

second get a dump/schema of the database:
md5sum gargandb.gz == 2c97ea9cfb67cd9767b779632a71e19f  gargandb.gz

third insert the database in gargandb
``shell
gunzip gargandb.zip
psql gargandb < gargandb
``


### Install Stack
- https://docs.haskellstack.org/en/stable/README/
- curl -sSL https://get.haskellstack.org/ | sh

### Get the orchestrator library

git clone https://github.com/np/servant-job.git

### Get the clustering louvain library

git clone https://gitlab.iscpif.fr/gargantext/clustering-louvain.git

## Building and installing
stack install

## Run Gargantext
~/.local/bin/gargantext --run Mock --port 8008
