
PATH_PATS_SRC=${PATSHOME}/src
MYTAGS_PATS_SRC_ATS=${PATSHOME}/MYTAGS_PATS_SRC_ATS

rm -rf ${MYTAGS_PATS_SRC_ATS}
find ${PATH_PATS_SRC} -name "*.sats" \
  -exec atsopt -o ${MYTAGS_PATS_SRC_ATS} --taggen -s {} \;
find ${PATH_PATS_SRC} -name "*.dats" \
  -exec atsopt -o ${MYTAGS_PATS_SRC_ATS} --taggen -d {} \;

java -jar ${PATSHOME}/ats-lang-tools.jar -c --input ${MYTAGS_PATS_SRC_ATS} --output ${PATSHOME}/tags


