#!/bin/bash

# Name first argument to be clear
FOURMOLU=$1

# Extensions necessary to tell fourmolu about 
EXTENSIONS="-o -XTypeApplications -o -XTemplateHaskell -o -XPatternSynonyms"
SOURCES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs')

"${FOURMOLU}" --mode check --check-idempotence $EXTENSIONS $SOURCES
