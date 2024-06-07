# Example setup and usage.
                                                                      
Running this server, using `cabal run example serve` will spin up a server.
                                                                      
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
  "rawHex": "46010000222499"
}
```

