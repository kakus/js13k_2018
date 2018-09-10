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

echo -- Copying source
cp src/Main.ts $OUTDIR

echo -- Removing debug code
sed -i "" -e '/#DEBUG-BEGIN/,/#DEBUG-END/d' $OUTDIR/Main.ts

echo -- Compiling
tsc -t es6 --outFile $OUTDIR/app.js $OUTDIR/Main.ts

echo -- Minifying app.js
java -jar compiler.jar \
   --compilation_level ADVANCED\
   --language_in ECMASCRIPT6\
   --language_out ECMASCRIPT6\
   --js_output_file $OUTDIR/app.min.js\
   $OUTDIR/app.js

echo -- Updating index.html so its load minified app
# sed -i "" s/app\.js/app.min.js/ $OUTDIR/index.html
sed -i "" "s/ src=.*\"//" $OUTDIR/index.html
sed -i "" "/<script/r $OUTDIR/app.min.js" $OUTDIR/index.html

echo -- Minifying html
html-minifier --collapse-whitespace --remove-comments --remove-optional-tags\
              --remove-redundant-attributes --remove-script-type-attributes\
              --remove-tag-whitespace --use-short-doctype --minify-css true\
              -o $OUTDIR/index.html $OUTDIR/index.html

echo -- Zipping
zip -9 -o $OUTDIR $OUTDIR/index.html $OUTDIR/s.png

echo -- Bundle Size
ls -l "$OUTDIR.zip" | awk '{print $5 kB}'

echo DONE
