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

```sql
INSERT INTO auth_user (password, is_superuser, username, first_name, last_name, email, is_staff, is_active)
  VALUES ('1resu', true, 'user1', 'user', '1', 'a@localhost', true, true);

-- nodetype NodeUser has id 1
-- inserted user_id = 3
INSERT INTO nodes (typename, user_id, name)
  VALUES (1, 3, 'user1');
  
-- same for master user -- 'gargantua'
INSERT INTO auth_user (password, is_superuser, username, first_name, last_name, email, is_staff, is_active)
  VALUES ('autnagrag, true, 'gargantua, 'gargantua, '1', 'g@localhost', true, true);

-- nodetype NodeUser has id 1
-- inserted user_id = 5
INSERT INTO nodes (typename, user_id, name)
  VALUES (1, 5, 'gargantua);
```
