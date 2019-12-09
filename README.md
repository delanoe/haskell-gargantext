# Gargantext Instance, Backend

[Release soon, help welcome]

## About this project

Gargantext is a collaborative web protocol for the exploration of sets
of unstructured texts.

Scientificatlly, it combines tools from natural language processing,
text-mining, complex networks analysis and interactive data
visualization to pave the way toward new kinds of interactions with your
digital corpora.

Technically, there is a [client](#gargantext-purescript) which connects
to the instance. If the client is try to be ergonomic, it only ask for
the instance API, either locally or remote.

This software is a free software, developed by the CNRS Complex Systems
Institute of Paris Île-de-France (ISC-PIF) and its partners.

## Public Rest API for Gargantext

## General API Information
* The base endpoint is: **https://api.gargantext.org**
* All endpoints return either a JSON object or array.
* Data is returned in **ascending** order. Oldest first, newest last.
* All time and timestamp related fields are in **milliseconds**.

## HTTP Return Codes

* HTTP `4XX` return codes are used for malformed requests;
  the issue is on the sender's side.
* HTTP `403` return code is used when the WAF Limit (Web Application Firewall) has been violated.
* HTTP `429` return code is used when breaking a request rate limit.
* HTTP `418` return code is used when an IP has been auto-banned for continuing to send requests after receiving `429` codes.
* HTTP `5XX` return codes are used for internal errors; the issue is on
  Binance's side.
  It is important to **NOT** treat this as a failure operation; the execution status is
  **UNKNOWN** and could have been a success.


## Error Codes
* Any endpoint can return an ERROR

Sample Payload below:
```javascript
{
  "code": -1121,
  "msg": "Invalid symbol."
}

```
## General Information on Endpoints
* For `GET` endpoints, parameters must be sent as a `query string`.
* For `POST`, `PUT`, and `DELETE` endpoints, the parameters may be sent as a
  `query string` or in the `request body` with content type
  `application/x-www-form-urlencoded`. You may mix parameters between both the
  `query string` and `request body` if you wish to do so.
* Parameters may be sent in any order.
* If a parameter sent in both the `query string` and `request body`, the
  `query string` parameter will be used.


# Endpoint security type
* Each endpoint has a security type that determines the how you will
  interact with it. This is stated next to the NAME of the endpoint.
    * If no security type is stated, assume the security type is NONE.
* API-keys are passed into the Rest API via the `X-GARG-APIKEY`
  header.
* API-keys and secret-keys **are case sensitive**.
* API-keys can be configured to only access certain types of secure endpoints.
 For example, one API-key could be used for WRITE only, while another API-key
 can access (READ) everything except for WRITE routes.
* By default, API-keys can access all secure routes.

Security Type | Description
------------ | ------------
NONE  | Endpoint can be accessed freely.
WRITE | Endpoint requires sending a valid API-Key and signature.
USER_DATA | Endpoint requires sending a valid API-Key and signature.
USER_TEXT | Endpoint requires sending a valid API-Key.
TEXT_DATA | Endpoint requires sending a valid API-Key.

* `WRITE` and `USER_DATA` endpoints are `SIGNED` endpoints.


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

Users has to be created first (user1 is created as instance):

- stack install
- ~/.local/bin/gargantext-init "gargantext.ini"

## Use Cases

### Multi-User with Graphical User Interface (Server Mode)

~/.local/bin/stack --docker exec gargantext-server -- --ini "gargantext.ini" --run Prod
Then you can log in with user1:1resu


### Command Line Mode tools

#### Simple cooccurrences computation and indexation from a list of Ngrams

stack --docker exec gargantext-cli -- CorpusFromGarg.csv ListFromGarg.csv Ouput.json

