# Issue Tracker

## Setup

```
$ nix-shell
$ cabal repl

$ import DB.PostgreSQL
$ createDbTables
$ testInsert

$ import API
$ runServer
```

## Run example client

```
$ nix-shell
$ cabal repl
$ import API
$ main
```

## Testing

```
curl -XGET -v "http://sam%40example.com:332532dcfaa1cbf61e2a266bd723612c@localhost:3008/"

curl -v -H 'Authorization: Bearer [YOUR-JWT-TOKEN]' 'http://localhost:3008/'


curl -XPOST -H "Content-Type: application/json" -H "Authorization: Bearer [YOUR-JWT-TOKEN]" --data '{"issueBlueprintTitle":"Cmdline2"}' localhost:3008/

```
