#! /bin/bash

EXPERIMENT_FOLDER=$1

rm subplots.txt
touch subplots.txt
while read SESSION;
do
   STEP=$(echo ${SESSION} | cut -f 1 -d ":")
   SESSION_FOLDER=$(echo ${SESSION} | cut -f 2 -d ":" | tr -d " ") 

   if [[ $STEP == *"pre"* ]]; then
     SUBPLOT=$(ls ${EXPERIMENT_FOLDER}/${SESSION_FOLDER}/PRE_GLOBAL_7mm/*.png)
   elif [[ $STEP == *"post"* ]]; then
     SUBPLOT=$(ls ${EXPERIMENT_FOLDER}/${SESSION_FOLDER}/POST_GLOBAL_7mm/*.png)
   else
     SUBPLOT=$(ls ${EXPERIMENT_FOLDER}/${SESSION_FOLDER}/Fast_look_PLOT/*_Fast_look_PLOT.png)
   fi
   echo ${SUBPLOT} >> subplots.txt
done <  ${EXPERIMENT_FOLDER}/sessions.yaml

magick  @subplots.txt -append ${EXPERIMENT_FOLDER}/$(basename ${EXPERIMENT_FOLDER}).png

