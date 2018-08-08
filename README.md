# Issue Tracker

## Setup

1. Configure Postgres according to config.json

2. In an appropriate REPL:

```
$ import DB.PostgreSQL
$ createDbTables

$ import API
$ seed
$ runServer
```

## Run example client

```
$ import API
$ main
```

## Testing
After executing `runServer`:

```
# Get issues
curl -XGET -v "http://test%40example.com:dummy@localhost:3008/"
curl -v -H 'Authorization: Bearer [YOUR-JWT-TOKEN]' 'http://localhost:3008/'

# Add issue
curl -XPOST -H "Content-Type: application/json" --data '{"issueBlueprintTitle":"My Title"}' "http://test%40example.com:dummy@localhost:3008/"
curl -XPOST -H "Content-Type: application/json" -H "Authorization: Bearer [YOUR-JWT-TOKEN]" --data '{"issueBlueprintTitle":"My Title"}' localhost:3008/

# Add user (no authentication needed)
curl -XPOST -H "Content-Type: application/json" --data '{"userBlueprintEmail": "sam@example.com", "userBlueprintFirstName": "Samuel", "userBlueprintLastName": "Evans-Powell", "userBlueprintPassword": "asdf"}' "http://localhost:3008/user"
```
