
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
  yaml::write_yaml(x = config, file = "analysis_params.yaml")
}

get_suffix <- function(working_directory, experiment_folder) {
  experiment_dir <- file.path(working_directory, experiment_folder)
  meta_file <- grep(x = list.files(experiment_folder), pattern = "METADATA.csv", value = TRUE)
  print(getwd())
  
  if(length(meta_file) == 0) {
    stop(paste0("METADATA.csv not found in ", experiment_dir))
  } else if (length(meta_file)>1) {
    stop(paste0("Multiple METADATA.csv found in ", experiment_dir))
  }
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
  # stop if the file already exists! (stop if not not exists = stop if exists)
  if(file.exists(dest_path)) {
    warning(paste0(dest_path, " exists"))
    return(NULL)
    
  }
  machine_name <- get_machine_name(working_directory, experiment_folder)
  src_path <- get_path_to_default_roi_centers(machine_name)
  file.copy(
    src_path,
    dest_path
  )
  print(paste0(src_path, " --> ", dest_path))
}