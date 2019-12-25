#!/bin/bash

# cd to project root
cd "$( cd "$(dirname "$0")" ; pwd -P )"/..

# compile
ghc -O1 -i"src" -i"src/Chart" "src/Main"

# cleanup intermediate files
rm -rf src/*.o src/*.hi src/*/*.o src/*/*.hi

# move executable to bin
mv src/Main bin/chart

