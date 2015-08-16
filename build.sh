#!/bin/bash
set -e

rm -f dist/*.js

TIME=$(date +%s)

sed -i '' 's/dist\/[0-9]*\.min\.js/dist\/'$TIME'\.min\.js/' index.html 

pulp build && psc-bundle output/*/*.js --module Main > dist/${TIME}.js

closure-compiler dist/${TIME}.js > dist/${TIME}.min.js
