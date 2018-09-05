#! /bin/bash

OUTDIR="bundle"

if [ -d "$OUTDIR" ]; then
   echo -- Removing directory $OUTDIR
   rm -rv $OUTDIR
fi

if [ -f "$OUTDIR.zip" ]; then
   echo -- Removing zipped $OUTDIR.zip
   rm -v "$OUTDIR.zip"
fi

echo -- Coping files from bin to $OUTDIR 
cp -rv bin bundle

echo -- Removing debug code
sed -i "" -e '/#DEBUG-BEGIN/,/#DEBUG-END/d' $OUTDIR/app.js

echo -- Minifying app.js
java -jar compiler.jar \
   --compilation_level ADVANCED\
   --language_in ECMASCRIPT6\
   --language_out ECMASCRIPT6\
   --js_output_file $OUTDIR/app.min.js\
   $OUTDIR/app.js

echo -- Updating index.html so its load minified app
sed -i "" s/app\.js/app.min.js/ $OUTDIR/index.html

echo -- Zipping
zip -r -9 -x $OUTDIR/app.js -o $OUTDIR $OUTDIR

echo -- Bundle Size
ls -l "$OUTDIR.zip" | awk '{print $5 kB}'

echo DONE
