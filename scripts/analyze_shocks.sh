#! /bin/bash

EXPERIMENT_FOLDER=$1

grep -i train ${EXPERIMENT_FOLDER}/sessions.yaml | cut -f 2 -d ":" > shock_sessions_temp.txt
while read SESSION_FOLDER;
do
  echo ${EXPERIMENT_FOLDER}/${SESSION_FOLDER}
  Rscript scripts/analyze_session.R ${EXPERIMENT_FOLDER} ${SESSION_FOLDER} TRAIN # > /dev/null 2>&1
done < shock_sessions_temp.txt;
