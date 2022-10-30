#!/bin/bash
#
# I Love Iesus s2
#

clear

# Validation.
if [ "$#" -ne 1 ]; then
    echo "Use: ./build-erlang.sh \"2 - basic arithmetic\""
    exit 2
fi

if [ -d "$1" ] 
then
    echo "ErLang source: \"$1\"" 
else
    echo "Directory not found: \"$1\""
    exit 2
fi

# Build modules.
echo "Build modules..."

cd "0 - utils"
rm -rf *.beam

erl -compile utils.erl
erl -compile division.erl

cd "../1 - project/"
rm -rf *
cd "../0 - utils/"
mv *.beam "../1 - project/"
cp *.erl "../1 - project/"
cd ..

# Copy app files.
cd "$1"
cp -R * "../1 - project/"
cd ..

# Build app.
echo "Running..."
echo ""

cd "1 - project"
rm -rf *.ex

erl -compile app.erl
# erl -noshell -s app app -s init stop
erl -noshell -s app ihs -s init stop

rm -rf *


