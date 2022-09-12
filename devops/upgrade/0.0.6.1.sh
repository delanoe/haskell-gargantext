#!/bin/bash

# To be executed at the root of the project
# To upgrade from 0.0.5.9 to 0.0.6.2


sudo apt update
sudo apt -yy upgrade

sudo sed -i "s/buster/bullseye/g" /etc/apt/sources.list
sudo apt update
sudo apt -yy dist-upgrade

git pull origin dev
./bin/install

# Database upgrade
echo "0.0.6.0 SQL upgrade"
./bin/psql gargantext.ini < devops/postgres/upgrade/0.0.6.0.sql

echo "Executing script haskell upgrade"
~/.local/bin/gargantext-upgrade

echo "0.0.6.1 SQL upgrade"
./bin/psql gargantext.ini < devops/postgres/upgrade/0.0.6.1.sql

sudo -i -u postgres bash << EOF 
pg_dumpall > /tmp/backup.dump
EOF

sudo sed -i "s/bullseye/bookworm/g" /etc/apt/sources.list
sudo apt update
sudo apt -yy dist-upgrade
sudo apt install -y postgresql-14 libpq-dev
sudo apt remove --purge postgresql-11 postgresql-13
sudo apt autoremove


sudo -i -u postgres bash << EOF 
psql < /tmp/backup.dump
EOF

sed -i "s/DB_PORT = 5432/DB_PORT = 5434/" gargantext.ini

# be sure the DB password is the right one
DBPASS=$(grep "DB_PASS" gargantext.ini | sed "s/^.*= //")
echo $DBPASS

sudo -i -u postgres psql << EOF 
ALTER ROLE gargantua password '${DBPASS}';
EOF

echo "0.0.6.2 SQL upgrade"
./bin/psql gargantext.ini < devops/postgres/upgrade/0.0.6.2.sql

# Make sure compilation is ok
./bin/install

echo "Upgrade is over"




