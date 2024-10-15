library(idocr2)
library(dplyr)
library(ggplot2)
library(cowplot)
library(readr)


args <- commandArgs(trailingOnly = TRUE)
working_directory <- args[1]
experiment_folder <- args[2]
test <- args[3]
print(args)


script_name <- tryCatch(
  scriptName::current_filename(),
  error = function(e) {
    "analyze_session.R"
})

message(paste0("Running ", script_name))

src_file <- tryCatch(
  rstudioapi::getActiveDocumentContext()$path,
  error = function(e) {
    print(e)
    file.path(getwd(), script_name)
  })

stopifnot(file.exists(src_file))

setwd(working_directory)


get_path_to_default_roi_centers <- function(machine_name) {
  return(
    file.path("~/opt/idocr/inst/roi_centers", machine_name, "ROI_CENTER.csv")
  )
}

write_analysis_params <- function(machine_name) {
  configs <- list(
    IDOC_001 = list(
      pixel_to_mm_ratio = 2.3,
      limits = c(-70, 70)
    ),
    IDOC_002 = list(
      pixel_to_mm_ratio = 6.16,
      limits = c(-155, 155)
    )
  )
  config <- configs[[machine_name]]
  yaml::write_yaml(x=config, file="analysis_params.yaml")

}

get_suffix <- function(working_directory, experiment_folder) {
  experiment_dir <- file.path(working_directory, experiment_folder)
  meta_file <- grep(x=list.files(experiment_dir), pattern = "METADATA.csv", value=TRUE)
  stopifnot(length(meta_file)==1)
  suffix <- substr(meta_file, 21, 52)
  return(suffix)
}
get_machine_name <- function(working_directory, experiment_folder) {
  machine_names <- c(
    "7eb8e224bdb944a68825986bc70de6b1" = "IDOC_001",
    "74f75f830109411ba67f74ecb268f9ef" = "IDOC_002"
  )
  suffix <- get_suffix(working_directory, experiment_folder)

  machine_name <- machine_names[suffix]
  return(machine_name)
}

use_default_roi_centers <- function(working_directory, experiment_folder) {
  
  suffix <- get_suffix(working_directory, experiment_folder)
  experiment_dir <- file.path(working_directory, experiment_folder)
  dest_path <- file.path(experiment_dir, paste0(experiment_folder, "_", suffix, "_ROI_CENTER.csv"))
  stopifnot(!file.exists(dest_path))
  machine_name <- get_machine_name(working_directory, experiment_folder)
  src_path <- get_path_to_default_roi_centers(machine_name)
  file.copy(
    src_path,
    dest_path
  )
  print(paste0(src_path, " --> ", dest_path))
}

machine_name <- get_machine_name(working_directory, experiment_folder)
write_analysis_params(machine_name)

#################################experimenter#############################
experimenter = ""

experiment_type = paste0("Aversive_Memory_", test, "_paired")
CS_plus = "OCT"
concentration = "1:500"                
US_Volt_pulses = "US = ES_75V 12 pulses 1/4sec_1X"
Food = "SA-ATR-"
Incubator_Light = "Blue"
Genotype = "X"


nrow <- 1
ncol <- 20
plot_height <- 5
plot_width <- 25

# Change the name of the labels as you please
# the first label should match treatment_A on your paradigm
# the second label should match treatment_B on your paradigm
labels <- c("OCT", "AIR")
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
  "TREATMENT_A",
  "TREATMENT_B"
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

if (test %in% c("PRE", "POST")) {
  
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
      paste0(test, "_GLOBAL_", border_mm, "mm"),
      paste0(test, "_1_", border_mm, "mm"),
      paste0(test, "_2_", border_mm, "mm")
    )
    analysis_mask <- masks[[i]]

    experiment_folder_01_Paired_1 = experiment_folder
    newfolder <- paste0("#", experimenter, "_" ,experiment_folder_01_Paired_1, "_PLOTS & PI-DZ_",border_mm,"mm")
    dir.create(file.path(experiment_folder_01_Paired_1, newfolder), )

    p <- idocr(
      experiment_folder = experiment_folder,
      treatments = treatments,
      border_mm = border_mm,
      min_exits_required = min_exits_required,
      src_file = src_file,
      subtitle = paste0(
        experimenter,"_",experiment_type, ", ", CS_plus, ", ",
        concentration, " & ", US_Volt_pulses, ", ", Genotype , ", ",
        Food, ", ", Incubator_Light
      ),
      delay = delay,
      CSplus_idx = CSplus_idx,
      mask_duration = mask_duration,
      analysis_mask = analysis_mask,
      labels = labels,
      nrow=nrow, ncol=ncol,
      height=plot_height, width=plot_width,
    )
    i <- i+1

  }

} else if (test == "TRAIN") {
 
  border_mm <- 7
  use_default_roi_centers(working_directory, experiment_folder)
  analysis_mask <- list(Fast_look_PLOT = c(0, Inf))
  p <- idocr(
    experiment_folder = experiment_folder,
    treatments = treatments,
    border_mm = border_mm,
    min_exits_required = min_exits_required,
    src_file = src_file,
    subtitle = paste0(
      experimenter,"_",experiment_type, ", ", CS_plus, ", ",
      concentration, " & ", US_Volt_pulses, ", ", Genotype , ", ",
      Food, ", ", Incubator_Light
    ),
    delay = delay,
    CSplus_idx = CSplus_idx,
    mask_duration = mask_duration,
    analysis_mask = analysis_mask,
    labels = labels,
    nrow=nrow, ncol=ncol,
    height=plot_height, width=plot_width
  )
}