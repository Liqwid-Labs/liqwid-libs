# Example setup and usage.
                                                                      
Running this server, using `cabal run example` will spin up a server.
                                                                      
Using `curl`, we can now query it:
``` sh
curl -X POST \
     -d '[]' \
     -H 'Content-Type: application/json' \
     localhost:3939/query-script/alwaysSucceeds \
 | jq
```

Which will result in:
```json
{
  "cborHex": "4746010000222499",
  "hash": "fc61e623d413aa67dc9367e8e48f5ab7f38093e871af6d9dd27b717e",
  "rawHex": "46010000222499"
}
```

