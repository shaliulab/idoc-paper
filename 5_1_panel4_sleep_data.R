library(scopr)
library(ggplot2)
library(data.table)
library(ggprism)
library(ggetho)

theme_set(ggprism::theme_prism(base_size=25))
parse_interval <- function(intervals, idx) {
  
  x <-  sapply(intervals, function(interval) {
    interval_zt <- unlist(strsplit(interval, split="-"))[[idx]]
    out <- as.numeric(gsub(x=interval_zt, pattern="ZT", replacement=""))
    return(out)
  })
  return(x)
}

load_ethoscope_data_for_idoc_paper <- function() {
  metadata <- data.table::fread("metadata.csv")
  metadata <- metadata[experiment%in% c("24hr LTM", "24hr LTM GTACR"),]
  metadata <- metadata[!is.na(date),]
  metadata <- metadata[Genotype=="Iso31",]
  metadata <- metadata[interactor%in% c("NONE", "DefaultStimulator"),]
  metadata[, .N, by=.(interactor, experiment, Training)]
  metadata$Training <- factor(metadata$Training, levels=c("No_training", "6X_Massed", "6X_Spaced"))
  metadata[is.na(PRE_ROI), PRE_ROI := "NONE"]
  metadata[is.na(POST_ROI), POST_ROI := "NONE"]
  
  metadata_etho <- data.table::fread("metadata_etho.csv")
  metadata_etho[ , interval := interactor_time_window]
  
  metadata <- rbind(
    metadata_etho[, .(Files, PRE_ROI="NONE", POST_ROI="NONE", region_id, machine_name, date, reference_hour, Training, experiment, Genotype, interactor, interval, User)],
    metadata[, .(Files, PRE_ROI, POST_ROI, region_id, machine_name, date, reference_hour, Training, experiment, Genotype, interactor, interval, User)]
  )
  metadata[, interval1:="NONE"]
  metadata[, interval2:="NONE"]
  metadata[interval!="No_stimulator", interval1 := parse_interval(interactor_time_window, 1)]           
  metadata[interval!="No_stimulator", interval2 := parse_interval(interactor_time_window, 2)]
  metadata$date <- as.character(metadata$date)
  
  metadata_linked <- scopr::link_ethoscope_metadata(metadata, result_dir="/ethoscope_data/results")
  dt <- scopr::load_ethoscope(
    metadata_linked,
    verbose=FALSE,
    reference_hour=NA,
    cache="/ethoscope_data/cache",
    FUN=sleepr::sleep_annotation, velocity_correction_coef=0.0048, time_window_length=10, min_time_immobile=300
  )
  dt_bin <- behavr::bin_apply_all(dt, y="asleep", x_bin_length=behavr::mins(30), summary_FUN=mean)
  dt_bin$asleep <- dt_bin$asleep*30
  metadata_linked<-dt_bin[,meta=TRUE]
  
  metadata_linked <- merge(
    metadata_linked[, .(id, machine_name, date=as.character(substr(datetime, 1, 10)), region_id)],
    metadata,
    by=c("machine_name", "date", "region_id"), all=TRUE
  )
  setkey(metadata_linked, id)
  setmeta(dt_bin, metadata_linked)
  saveRDS(object = dt_bin, file = "dt_bin.RDS")
  ggplot(data=behavr::rejoin(dt_bin)[Genotype=="Iso31" & interactor=="DefaultStimulator",], aes(x=t, y=asleep, color=Training)) +
    stat_pop_etho() +
    scale_x_hours(name="ZT")
}