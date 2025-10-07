

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  message(
    "Usage:
    Rscript analyze_session.R <working_directory> <experiment_folder> <session> [subtitle]
    "
  )
  stop("")
  
}

library(idocr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(readr)


working_directory <- args[1]
experiment_folder <- args[2]
session <- args[3]
stopifnot(session %in% c("PRE", "POST", "TRAIN"))
if (length(args) == 4) {
  subtitle <- args[4]
} else {
  subtitle <- ""
}

script_name <- tryCatch(
  scriptName::current_filename(),
  error = function(e) {
    "analyze_session.R"
  }
)

wd <- getwd()


source("scripts/analyze_session_lib.R")


message(paste0("Running ", script_name))

src_file <- tryCatch(
  rstudioapi::getActiveDocumentContext()$path,
  error = function(e) {
    print(e)
    file.path(getwd(), script_name)
  }
)

setwd(working_directory)


machine_name <- get_machine_name(working_directory, experiment_folder)
write_analysis_params(machine_name)

################################# experimenter#############################
experimenter <- ""

experiment_type <- paste0("Aversive_Memory_", session, "_paired")


nrow <- 1
ncol <- 20
plot_height <- 5
plot_width <- 25

# Change the name of the labels as you please
# the first label should match treatment_A on your paradigm
# the second label should match treatment_B on your paradigm
labels <- c("CS+", "AIR")
CSplus_idx <- 1 # or 2 depending on   which treatment is CS+


# Minimal number of exits required to consider the data
# of one animal significant and then compute a index from it
min_exits_required <- 3

# Apply a time offset to the treatment time series
# to account for the time it takes for odour to
# arrive to the chambers
# Units in seconds
delay <- 2


#### Probably you dont want to change this
#### Please change these numbers only if you know what you are doing

# Define a distance from the center of the chamber
# to the decision zone
# Units in mm
border_mm <- 5

# Behavioral masking
# Ignore exits happening this amount of seconds
# after the previous exit
# to avoid counting the same exit
# as two exits happening within ridiculously little time
mask_duration <- 1

treatments <- c(
  "TREATMENT_A" = "TREATMENT_A",
  "TREATMENT_B"= "TREATMENT_B"
)

# Analysis mask
# This is a list of numeric vectors of length 2
# The name of the vector should represent some block or interval of your experiment
# i.e. pre conditioning, post conditioning, etc
# The vector should delimit the start and end time of the block in seconds
# Passing this argument to idocr() will cause R to generate a subfolder
# in the experiment data folder, for each element in the list
# The folder will have the name of the element in the list
# e.g. this list will create a subfolder called EVENT1 and another called EVENT2
# Each of them will contain a pdf and png version of the plot but only the interval
# when the mask is active is analyzed. It is marked accordingly on the plot
# Moreover, you get SUMMARY and PI .csv files

if (session %in% c("PRE", "POST")) {
  borders <- 5:10
  masks <- lapply(borders, function(i) {
    list(
      GLOBAL = c(0, Inf),
      T1 = c(58, 122) + delay,
      T2 = c(178, 242) + delay
    )
  })

  i <- 1
  for (border_mm in borders) {
    names(masks[[i]]) <- c(
      paste0(session, "_GLOBAL_", border_mm, "mm"),
      paste0(session, "_1_", border_mm, "mm"),
      paste0(session, "_2_", border_mm, "mm")
    )
    analysis_mask <- masks[[i]]

    experiment_folder_01_Paired_1 <- experiment_folder
    newfolder <- paste0("#", experimenter, "_", experiment_folder_01_Paired_1, "_PLOTS & PI-DZ_", border_mm, "mm")
    dir.create(file.path(experiment_folder_01_Paired_1, newfolder), )

    p <- idocr(
      experiment_folder = experiment_folder,
      treatments = treatments,
      border_mm = border_mm,
      min_exits_required = min_exits_required,
      src_file = src_file,
      subtitle = subtitle,
      delay = delay,
      CSplus_idx = CSplus_idx,
      mask_duration = mask_duration,
      analysis_mask = analysis_mask,
      labels = labels,
      nrow = nrow, ncol = ncol,
      height = plot_height, width = plot_width,
    )
    i <- i + 1
  }
} else if (session == "TRAIN") {
  border_mm <- 7
  use_default_roi_centers(".", experiment_folder)
  analysis_mask <- list(Fast_look_PLOT = c(0, Inf))
  p <- idocr(
    experiment_folder = experiment_folder,
    treatments = treatments,
    border_mm = border_mm,
    min_exits_required = min_exits_required,
    src_file = src_file,
    subtitle = subtitle,
    delay = delay,
    CSplus_idx = CSplus_idx,
    mask_duration = mask_duration,
    analysis_mask = analysis_mask,
    labels = labels,
    nrow = nrow, ncol = ncol,
    height = plot_height, width = plot_width
  )
}
setwd(wd)
