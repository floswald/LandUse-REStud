#!/usr/bin/env fish

# exports current dir to Downloads and zips up
set DEST /Users/floswald/Downloads/package-submission

rm -rf $DEST
mkdir -p $DEST; and cp -rf (pwd) $DEST

# cleanup
cd $DEST
find . -name ".DS_Store" -type f -delete

rm -f $DEST/package.sh
rm -f $DEST/README.md
rm -f $DEST/README.html
rm -f $DEST/README.tex
rm -rf $DEST/README_files

# remove all .log files using find instead of glob
find $DEST/code/LandUseR -name "*.log" -type f -delete 2>/dev/null

# remove all output but keep dir (only if output directory exists)
if test -d output
    cd output; and rm -rf (ls -A | grep -v "^\.gitkeep\$")
    cd $DEST
end

# remove git
cd $DEST; and rm -rf .git .gitignore

# zip it up
cd $DEST/..
zip -rq package-submission.zip package-submission