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
curl --cacert cacert.pem -XGET -v "https://test%40example.com:dummy@localhost:3008/issues"
curl --cacert cacert.pem -v -H 'Authorization: Bearer [YOUR-JWT-TOKEN]' 'https://localhost:3008/issues'

# Add issue
curl --cacert cacert.pem -XPOST -H "Content-Type: application/json" --data '{"issueBlueprintTitle":"My Title"}' "https://test%40example.com:dummy@localhost:3008/issues"
curl --cacert cacert.pem -XPOST -H "Content-Type: application/json" -H "Authorization: Bearer [YOUR-JWT-TOKEN]" --data '{"issueBlueprintTitle":"My Title"}' "localhost:3008/issues"

# Add user (no authentication needed)
curl --cacert cacert.pem -XPOST -H "Content-Type: application/json" --data '{"userBlueprintEmail": "sam@example.com", "userBlueprintFirstName": "Samuel", "userBlueprintLastName": "Evans-Powell", "userBlueprintPassword": "asdf"}' "https://localhost:3008/users"

# Update issue status
curl --cacert cacert.pem -XPOST -H "Content-Type: application/json" --data '"Closed"' "https://test%40example.com:dummy@localhost:3008/issues/1/update-status"

# Post comment
curl --cacert cacert.pem -XPOST -H "Content-Type: application/json" --data '{"commentBlueprintForIssue":1, "commentBlueprintBody":"Great issue!"}' "https://test%40example.com:dummy@localhost:3008/comments"
```
