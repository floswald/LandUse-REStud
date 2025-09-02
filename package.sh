#!/usr/bin/env fish

# exports current dir to Downloads and zips up
set DEST /Users/floswald/Downloads/package-submission
set PKG $DEST/ReplicationPackage-submit

rm -rf $DEST
mkdir -p $DEST; and cp -rf (pwd) $DEST

# cleanup
cd $PKG
find . -name ".DS_Store" -type f -delete

rm -f $PKG/package.sh
rm -f $PKG/README.md
rm -f $PKG/README.html
rm -f $PKG/README.tex
rm -rf $PKG/README_files

# remove all .log files using find instead of glob
find $PKG/code/LandUseR -name "*.log" -type f -delete 2>/dev/null

# remove all output but keep dir (only if output directory exists)
if test -d output
    cd output; and rm -rf (ls -A | grep -v "^\.gitkeep\$")
    cd $PKG
end

# remove git
cd $PKG; and rm -rf .git .gitignore

# zip it up
cd $PKG/..
zip -rq package-submission.zip ReplicationPackage-submit