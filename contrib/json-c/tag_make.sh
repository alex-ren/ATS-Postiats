
PATH_JSONC=${PATSHOME}/contrib/json-c
MYTAGS_JSONC_PATS=${PATH_JSONC}/MYTAGS_JSONC_PATS

rm -rf ${MYTAGS_JSONC_PATS}
find ${PATH_JSONC} -name "*.sats" -exec patsopt --output-a ${MYTAGS_JSONC_PATS} --taggen -s {} \;
find ${PATH_JSONC} -name "*.dats" -exec patsopt --output-a ${MYTAGS_JSONC_PATS} --taggen -d {} \;

java -jar ${PATSHOME}/ats-lang-tools.jar -c --input ${MYTAGS_JSONC_PATS} --output ${PATH_JSONC}/tags


