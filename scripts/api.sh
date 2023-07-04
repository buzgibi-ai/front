#!/bin/sh

url=$1
file=$2
api=$3

abort()
{
    echo >&2 '
***************
*** ABORTED ***
***************
'
    echo "An error occurred. Exiting..." >&2
    exit 1
}

trap 'abort' 0

set -e

echo >&2 '
************
*** START *** 
************
'

generate() { 
  node api-downloader.mjs $url $file
  openapi-generator-cli \
  generate -i $file -g javascript -o src/core/Buzgibi/Api/Foreign/$api \
  --additional-properties=usePromises=true,emitModelMethods=true
   find src/ -type f -name "*.js" -exec js-beautify -r {} \;
}

generate

trap : 0

echo >&2 '
************
*** DONE *** 
************
'