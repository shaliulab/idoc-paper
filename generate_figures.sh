#! /bin/bash

eval "$(~/mambaforge/bin/conda shell.bash hook)"

conda activate R42
Rscript 2_panel1.R
Rscript 3_panel2.R
Rscript 4_panel3.R
Rscript 5_panel4.R
Rscript 5_1_panel4_sleep_data.R
Rscript 5_2_panel4_sleep_data.R
Rscript 6_panel5.R
