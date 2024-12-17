###################
# Antonio Ortega
#
# antonio.ortega@kuleuven.be
#
# Script to copy IDOC data database
#
# Instructions
# 1_tidy_datav2.R.ipynb creates the file index.csv,
# which contains one row per experiment in the IDOC database
# required to generate the paper figures
#
# Example
#
# idoc_folder,N,PRE_files,POST_files
# /idoc_data/IDOC/2021-09-idoc_data/2021-09-30 (CH1),9,PI_PRE_1_7mm.csv|PI_PRE_2_7mm.csv,PI_POST_1_7mm.csv|PI_POST_2_7mm.csv
# ...
#
#
# dest_folder must point to a folder which will store the mirrored IDOC database
# the script will print one line for every experiment in the mirror
# if dry_run is set to True, the actual copying will not happen


dest_folder <- "./IDOC_dataset"
dry_run <- FALSE
mm_decision_zone <- 7

copy_experiment_folder <- function(idoc_folder, dest_folder, pre_session, post_session, mm_decision_zone = 7, ...) {
  dest_folder <- file.path(dest_folder, paste0(substr(basename(idoc_folder), 1, 7), "-idoc_data"))
  copy_session_folder(idoc_folder, pre_session, dest_folder, ...)
  copy_session_folder(idoc_folder, post_session, dest_folder, ...)
  file.copy(file.path(idoc_folder, "sessions.yaml"), file.path(dest_folder, basename(idoc_folder)))
}


list_session_files <- function(session_folder) {
  files <- c(
    list.files(path = session_folder, pattern = "ORIGINAL.avi"),
    list.files(path = session_folder, pattern = ".csv")
  )
  trial_1 <- list.files(path = session_folder, pattern = paste0("....?_1_", mm_decision_zone), include.dirs = TRUE, full.names = TRUE)
  trial_2 <- list.files(path = session_folder, pattern = paste0("....?_2_", mm_decision_zone), include.dirs = TRUE, full.names = TRUE)

  if(length(trial_1) == 0 | length(trial_2) == 0) {
    return(NULL)
  }
  files <- list(
    root = files,
    trial_1 = list.files(trial_1),
    trial_2 = list.files(trial_2)
  )
  names(files)[2] <- basename(trial_1)
  names(files)[3] <- basename(trial_2)
  
  return(files)
}

copy_session_folder <- function(idoc_folder, session, dest_folder, dry_run=FALSE) {
  session_folder <- file.path(idoc_folder, session)
  folder_without_root <- basename(idoc_folder)

  idoc_folder_dest <- file.path(dest_folder, folder_without_root)
  session_folder_dest <- file.path(idoc_folder_dest, session)
  
  dir.create(session_folder_dest, showWarnings = FALSE, recursive = TRUE)
  files <- list_session_files(session_folder)
  if(is.null(files)) {
    return(NULL)
  }
  
  for (file in files$root) {
    src_file <- file.path(session_folder, file)
    if (!dry_run) file.copy(src_file, session_folder_dest)
    else print(paste0(src_file, " --> ", session_folder_dest))
  }
  trial_1 <- basename(names(files)[2])
  trial_2 <- basename(names(files)[3])

  dest <- file.path(session_folder_dest, trial_1)
  dir.create(dest, showWarnings = F)
  for (file in files[[trial_1]]) {
    src_file <- file.path(session_folder, trial_1, file)
    if (!dry_run)  file.copy(src_file,  dest)
    else print(paste0(src_file, " --> ", dest))
  }
  
  dest <- file.path(session_folder_dest, trial_2)
  dir.create(dest, showWarnings = F)
  for (file in files[[trial_2]]) {
    src_file <- file.path(session_folder, trial_2, file)
    if (!dry_run)  file.copy(src_file,  dest)
    else print(paste0(src_file, " --> ", dest))
  }
}
  

index <- data.table::fread("index.csv")

for (i in 1:nrow(index)) {
  idoc_folder <-index[i, idoc_folder]
  print(idoc_folder)
  pre_session <- rev(unlist(strsplit(index[i, PRE_files[[1]]], split = "/")))[3]
  post_session <- rev(unlist(strsplit(index[i, POST_files[[1]]], split = "/")))[3]
  copy_experiment_folder(idoc_folder, dest_folder, pre_session, post_session, mm_decision_zone = mm_decision_zone, dry_run = dry_run)
}


ethoscope_files <- yaml::read_yaml("figures/paper/Fig4/fig4_ethoscope_database.yaml")
dir.create("ethoscope_data/results", recursive = TRUE, showWarnings = FALSE)
for (dbfile in ethoscope_files) {
  dest <- file.path("./", dbfile)
  dest_dir <- dirname(dest)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(dbfile, dest)
  print(paste0(dbfile, " --> ", dest))
}
