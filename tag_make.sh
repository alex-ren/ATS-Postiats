rm -rf MYTAGS_PATS_ATS2JSON
find ${PATSHOME}/libatsyn2json -name "*.sats" -exec atsopt -o MYTAGS_PATS_ATS2JSON --taggen -s {} \;
find ${PATSHOME}/libatsyn2json -name "*.dats" -exec atsopt -o MYTAGS_PATS_ATS2JSON --taggen -d {} \;

rm -rf MYTAGS_ALL
cat MYTAGS_ATS_JSON >> MYTAGS_ALL
cat MYTAGS_PATS_SRC >> MYTAGS_ALL 
cat MYTAGS_PATS_ATS2JSON >> MYTAGS_ALL 
cat MYTAGS_ATS_PRELUDE >> MYTAGS_ALL 
java -jar ats-lang-tools.jar -c --input MYTAGS_ALL --output tags

