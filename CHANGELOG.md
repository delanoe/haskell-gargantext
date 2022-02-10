## Version 0.0.5.5.6
* [BACK][FIX] ./bin/psql gargantext.ini < devops/posgres/upgrade/0.0.5.5.6.sql
* [FRONT] fix NodeType list show (Nodes options)

## Version 0.0.5.5.5
* [FORNT] fix Graph Explorer search ngrams
* [FRONT] fix NodeType list show (main Nodes)

## Version 0.0.5.5.4
* [BACK][OPTIM] NgramsTable scores
* [BACK] bin/client script to analyze backend performance and reproduce bugs
* [FRONT] Adding Language selection

## Version 0.0.5.5.3
* [BACK] Adding a Max limit for others lists.

## Version 0.0.5.5.2
* [BACK][OPTIM] Index on node_node_ngrams to seed up ngrams table score
  queries. Please execute the upgrade SQL script
  devops/postgres/0.0.5.5.2.sql

## Version 0.0.5.5.1
* [BACK] FIX Graph Explorer search with selected ngrams
* [FRONT] Clean CSS

## Version 0.0.5.5
* [FRONT] Visio frame removed, using a new tab instead (which is working)
* [BACK] Scores on the docs view fixed

## Version 0.0.5.3
* [FRONT] SSL local option

## Version 0.0.5.2
* [QUAL] Scores in Ngrams Table fixed during workflow and user can
  refresh it if needed.

## Version 0.0.5.1
* [OPTIM] Upgrade fix with indexes and scores counts

## Version 0.0.5
* [OPTIM][DATABASE] Upgrade Schema, move conTexts in contexts table which requires a version bump.

## Version 0.0.4.9.9.6
* [BACK] PubMed parser fixed
* [FRONT] Visio Frame resized

## Version 0.0.4.9.9.5
* [FIX] Chart Sort

## Version 0.0.4.9.9.4
* [FEAT] Corpus docs download

## Version 0.0.4.9.9.3
* [BACK] Graph update with force option

## Version 0.0.4.9.9.2
* [BACK] Opaleye Upgrade

## Version 0.0.4.9.9.1
* [FRONT] 350-dev-graph-search-in-forms-not-labels
* [FRONT] 359-dev-input-with-autocomplete

## Version 0.0.4.9.9
* [FIX] Continuous Integration (CI)

## Version 0.0.4.9.8
* [FEAT] All backend routes with clients functions

## Version 0.0.4.9.7
* [FEAT] Searx API done (needs a fix for language selection)

## Version 0.0.4.9.6
* [UX] GT.query forces trees reload for async tasks

## Version 0.0.4.9.5
* [FEAT] Order 2 fixed with filtered edges

## Version 0.0.4.9.4
* [FEAT] Order 1 similarity validated and optimized

## Version 0.0.4.9.3
* [FIX] Node Calc import + more flexible delimiter for CSV parser

## Version 0.0.4.9.2
* [FEAT] Node Calc Parsing added (in tests)

## Version 0.0.4.9.1
* [FIX] Graph Screenshot

## Version 0.0.4.9
* [FEAT] Graph with order 1 and order 2 and node size

## Version 0.0.4.8.9
* BACKEND: fix psql function util without sensitive data
* FRONTEND: fix folder navigation (up link)

## Version 0.0.4.8.8
* FIX for CI

## Version 0.0.4.8.7
* FIX the graph generation (automatic/default, renewal, any distance)

## Version 0.0.4.8.6
* FIX the ngrams grouping

## Version 0.0.4.8.5
* Unary document insertion: Doc table is reloaded after upload

## Version 0.0.4.8.4
* Migration: instance dev is now dev.sub.gargantext.org

## Version 0.0.4.1
* Refact/code design better syntax for DataType fields

## Version 0.0.4
* Fix the search in Title and abstracts.
* [UPGRADE] execute devops/postgres/upgrade/0.0.4.sql to your database to upgrade it

## Version 0.0.3.9.1
* Graph Update fix
* Document view: full text removed

## Version 0.0.0.2
* Fix the community detection.
* TextFlow starts to make sense

## Version 0.0.0.1
* Very first version (main functions ready for tests) of Haskell Version
  of Gargantext. Previous versions (3) were written with another
  language and another framework (Python/Javascript mainly).
