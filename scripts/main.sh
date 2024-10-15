#! /bin/bash

# Analyze all the folders stored in args.txt
# Format of args.txt
# /path/to/database/experiment session PRE
# /path/to/database/experiment session TRAIN
# /path/to/database/experiment session POST

while IFS="," read -r -a ARGS_ARR;
do
  echo ${ARGS_ARR[*]}
  Rscript script.R "${ARGS_ARR[0]}" "${ARGS_ARR[1]}" ${ARGS_ARR[2]}  > /dev/null 2>&1
done < args.txt 
