#! /bin/bash

FOLDER="./figures/2024-10-27"
for i in `seq 1 5`
do
  ~/opt/Inkscapev1.4.AppImage ${FOLDER}/Fig${i}/Figure_${i}m.svg -d 200 -C --batch-process --export-type=pdf --export-filename=${FOLDER}/Fig${i}/Figure_${i}m.pdf
done
find ${FOLDER} -regex .*m.pdf | sort > pdfs.txt
pdftk $(cat pdfs.txt) cat output idoc_figures.pdf