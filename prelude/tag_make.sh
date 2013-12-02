
PATH_PRELUDE=${PATSHOME}/prelude
MYTAGS_PATS_PRELUDE_PATS=${PATH_PRELUDE}/MYTAGS_PATS_PRELUDE_PATS

rm -rf ${MYTAGS_PATS_PRELUDE_PATS}
find ${PATH_PRELUDE}/SATS \( -name "CODEGEN" -o -name "DOCUGEN" \) -prune -o -name "*.sats" \
  -exec patsopt --output-a ${MYTAGS_PATS_PRELUDE_PATS} --taggen -s {} \;
find ${PATH_PRELUDE}/DATS \( -name "CODEGEN" -o -name "DOCUGEN" \) -prune -o -name "*.dats" \
  -exec patsopt --output-a ${MYTAGS_PATS_PRELUDE_PATS} --taggen -d {} \;

java -jar ${PATSHOME}/ats-lang-tools.jar -c --input ${MYTAGS_PATS_PRELUDE_PATS} --output ${PATH_PRELUDE}/tags


