#!/bin/bash

set -e

js="elm.js"
min="$build/elm.min.js"

## CHECK FOR BINARIES

if ! [ -x "$(command -v uglifyjs)" ]
then
  echo 'Error: need uglifyjs to be available for minification.'
  echo 'You can run `npm install --global uglify-js` to get it.'
  exit 1
fi

echo "======== DEPENDENCIES ====================="
cat elm.json

echo ""
echo "======== PROJECT SIZE ====================="
find src -name '*.elm' | xargs wc -l

echo ""
echo "======== COMPILE ====================="
time elm make --optimize --output=$js src/Main.elm

echo ""
echo "======== MINIFY ======================="
uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=$min
echo "Initial size: $(cat $js | wc -c) bytes  ($js)"
echo "Minified size:$(cat $min | wc -c) bytes  ($min)"
echo "Gzipped size: $(cat $min | gzip -c | wc -c) bytes"

rm "$js"
