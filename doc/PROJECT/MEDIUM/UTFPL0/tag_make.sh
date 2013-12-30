
rm -rf MYTAGS_PATS_UTFPL
find ${PATSHOME}/doc/PROJECT/MEDIUM/UTFPL0 -name "*.sats" -exec patsopt --output-a MYTAGS_PATS_UTFPL --taggen -s {} \;
find ${PATSHOME}/doc/PROJECT/MEDIUM/UTFPL0 -name "*.dats" -exec patsopt --output-a MYTAGS_PATS_UTFPL --taggen -d {} \;

rm -rf MYTAGS_ALL

cat MYTAGS_PATS_UTFPL >> MYTAGS_ALL

java -jar ${PATSHOME}/ats-lang-tools.jar -c --input MYTAGS_ALL --output tags

