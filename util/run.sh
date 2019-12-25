#!/bin/bash

# cd to project root
cd "$( cd "$(dirname "$0")" ; pwd -P )"/..

# run
echo -e "word1 word5\nword2 word6\nword3 word7\nword4 word8" | \
runhaskell -i"src" -i"src/Chart" "src/Main" -h -d " " 

# run again
runhaskell -isrc -isrc/Chart "src/Main" -h -d " " "util/test0" "util/test1"
