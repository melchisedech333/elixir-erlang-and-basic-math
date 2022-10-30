#!/bin/bash
#
# I Love Iesus s2
#

clear

# Validation.
if [ "$#" -ne 1 ]; then
    echo "Use: ./build-elixir.sh \"2 - basic arithmetic\""
    exit 2
fi

if [ -d "$1" ] 
then
    echo "Elixir source: \"$1\"" 
else
    echo "Directory not found: \"$1\""
    exit 2
fi

# Build modules.
echo "Build modules..."

cd "0 - utils"
rm -rf *.beam

elixirc utils.ex
elixirc division.ex

cd "../1 - project/"
rm -rf *
cd "../0 - utils/"
mv *.beam "../1 - project/"
cd ..

# Copy app files.
cd "$1"
cp -R * "../1 - project/"
cd ..

# Build app.
echo "Running..."
echo ""

cd "1 - project"
rm -rf *.erl

elixir app.ex

rm -rf *


