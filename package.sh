#!/bin/bash

# exports current dir to Downloads and zips up
DEST=/Users/floswald/Downloads/package-submission

cp -r $(pwd) $DEST

# cleanup
cd $DEST
find . -name ".DS_Store" -type f -delete

rm $DEST/package.sh
rm $DEST/README.md

# remove all output but keep dir
cd output && rm -rf $(ls -A | grep -v "^\.gitkeep$")

# zip it up and remove original
cd $DEST/..
zip -rq package-submission.zip package-submission 
rm -rf package-submission





