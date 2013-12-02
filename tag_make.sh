
rm -rf ${PATSHOME}/MYTAGS_PATS_ATS2JSON_ATS
find ${PATSHOME}/libatsyn2json -name "*.sats" -exec atsopt -o ${PATSHOME}/MYTAGS_PATS_ATS2JSON_ATS --taggen -s {} \;
find ${PATSHOME}/libatsyn2json -name "*.dats" -exec atsopt -o ${PATSHOME}/MYTAGS_PATS_ATS2JSON_ATS --taggen -d {} \;

rm -rf ${PATSHOME}/MYTAGS_ALL

cat ${PATSHOME}/MYTAGS_ATS_JSON >> ${PATSHOME}/MYTAGS_ALL
cat ${PATSHOME}/MYTAGS_ATS_PRELUDE >> ${PATSHOME}/MYTAGS_ALL 

cat ${PATSHOME}/MYTAGS_PATS_SRC_ATS >> ${PATSHOME}/MYTAGS_ALL 
cat ${PATSHOME}/MYTAGS_PATS_ATS2JSON_ATS >> ${PATSHOME}/MYTAGS_ALL 

java -jar ${PATSHOME}/ats-lang-tools.jar -c --input ${PATSHOME}/MYTAGS_ALL --output ${PATSHOME}/tags


