#!/bin/bash
set -e

rm -f dist/*.js

TIME=$(date +%s)

sed -i '' 's/dist\/[0-9]*\.js/dist\/'$TIME'\.js/' index.html 

pulp build && psc-bundle output/*/*.js --module Main > dist/${TIME}.js
