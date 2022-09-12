#!/bin/bash

# To be executed at the root of the project

# backup

sudo apt update
sudo apt upgrade

sudo sed -i "s/buster/bullseye/g" /etc/apt/sources.list
sudo apt update
sudo apt -yy dist-upgrade

git pull origin dev
./bin/install

#./bin/psql gargantext.ini < devops/postgres/upgrade/0.0.6.0.sql
#./bin/psql gargantext.ini < devops/postgres/upgrade/0.0.6.1.sql

# exec script haskell upgrade

sudo sed -i "s/bullseye/bookworm/g" /etc/apt/sources.list
sudo apt update
sudo apt -yy dist-upgrade
sudo apt install -y postgresql-14 libpq-dev
sed -i "s/DB_PORT = 5432/DB_PORT = 5433/" gargantext.ini

DBPASS=$(grep "DB_PASS" gargantext.ini | sed "s/^.*= //")


sudo -i -u postgres bash << EOF 
psql < 'ALTER ROLE gargantua password \'$DBPASS\'';
EOF




./bin/psql gargantext.ini < devops/postgres/upgrade/0.0.6.2.sql





