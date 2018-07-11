# Issue Tracker

## Testing

```
curl -XGET -v 'http://servant:server@localhost:8080/'

curl -v -H 'Authorization: Bearer [YOUR-JWT-TOKEN]' 'http://localhost:8080/'

curl -XPOST -H "Content-Type: application/json" -H "Authorization: Bearer [YOUR-JWT-TOKEN]" --data '{"issueBlueprintTitle":"Cmdline2", "issueBlueprintSubmitter":1}' localhost:8080/

```
