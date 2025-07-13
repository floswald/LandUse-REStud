#!/bin/bash

# exports current dir to Downloads and zips up
DEST=/Users/floswald/Downloads/package-submission

cp -rf $(pwd) $DEST

# cleanup
cd $DEST
find . -name ".DS_Store" -type f -delete

rm $DEST/package.sh
rm $DEST/README.md
rm $DEST/README.html
rm -rf $DEST/README_files

rm -f $DEST/code/LandUseR/*.log

# remove all output but keep dir
cd output && rm -rf $(ls -A | grep -v "^\.gitkeep$")

# remove git
cd $DEST && rm -rf .git .gitignore

# zip it up
cd $DEST/..
zip -rq package-submission.zip package-submission 





