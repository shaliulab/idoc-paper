process_sleep_dataset <- function(panel4_data, periods) {
  dt_bin <- readRDS("dt_bin.RDS")
  metadata_no_training <- dt_bin[, meta = T][PRE_ROI == "NONE", ]
  metadata <- dt_bin[, meta = T][PRE_ROI != "NONE", ]
  metadata <- merge(
    panel4_data[, .(Files, PRE_ROI = as.character(PRE_ROI), POST_ROI = as.character(POST_ROI))],
    metadata,
    by = c("Files", "PRE_ROI", "POST_ROI"), all.x = TRUE, all.y = FALSE
  )
  # flies in metadata with Training = NA are flies that are dropped in previous notebooks bcs they dont pass the criteria
  metadata$PRE_ROI <- as.character(metadata$PRE_ROI)
  metadata$POST_ROI <- as.character(metadata$POST_ROI)
  metadata <- rbind(metadata, metadata_no_training)
  data.table::setkey(metadata, id)
  behavr::setmeta(dt_bin, metadata)
  dt_bin <- dt_bin[!is.na(xmv(Training)), ]

  id <- "id"
  group <- "Training"

  dt_bin_full <- behavr::rejoin(dt_bin)
  table(dt_bin_full$Training)

  dt_bin_full_wide <- add_ghost_data(dt_bin_full, id = id, x = "t", columns = c("asleep"), group = group, to_wide = TRUE)

  dt_bin_full_wide$zt <- dt_bin_full_wide$t / 3600
  out <- dt_bin_full_wide[, setdiff(colnames(dt_bin_full_wide), c("zt", "t")), with = FALSE]
  print(dim(out))


  # testing sleep amount differences over time
  dt_counts <- dt_bin_full[, .N, by = .(t, Training)][, .N, by = t]
  time_testing <- dt_counts[N == 3, t]
  execute_test <- function(test, df, comparison) {
    test_out <- test(
      df[Training == comparison[1], asleep],
      df[Training == comparison[2], asleep],
      alternative = "greater",
      paired = FALSE
    )
    effect_size <- test_out$estimate[2] - test_out$estimate[1]

    return(list(p = test_out$p.value, effect_size = effect_size))
  }

  comparisons <- list(
    c("6X_Spaced", "6X_Massed"),
    c("6X_Spaced", "No_training")
  )
  test_out <- list()
  dt_bin_full[, t_foo := t]
  for (comparison in comparisons) {
    test_out[[comparison[2]]] <- dt_bin_full[
      t %in% time_testing,
      execute_test(t.test, .SD, comparison),
      by = t
    ]
    test_out[[comparison[2]]][["Training"]] <- comparison[2]
  }
  test_out <- do.call(rbind, test_out)
  sem <- function(x) {
    return(sd(x) / sqrt(length(x)))
  }
  summ_data <- dt_bin_full[, .(id, t, asleep, Training)][, .(mu = mean(asleep), sem = sem(asleep)), by = .(t, Training)]
  summ_data$Training <- factor(as.character(summ_data$Training), levels = c(trainings, "No_training"))
  winner <- summ_data[Training != "6X_Spaced", .(Training = .SD[which.max(mu), Training]), by = t]
  winner$winner <- TRUE
  test_out <- merge(
    test_out,
    winner,
    all = TRUE,
    by = c("t", "Training")
  )
  test_out[is.na(winner), winner := FALSE]
  test_out$symbol <- ""
  test_out$symbol_ml <- ""
  
  test_out[p < 0.05, symbol := "*"]
  test_out[p < 0.01, symbol := "**"]
  test_out[p < 0.005, symbol := "***"]
  test_out[p < 0.05, symbol_ml := "*"]
  test_out[p < 0.01, symbol_ml := "*\n*"]
  test_out[p < 0.005, symbol_ml := "*\n*\n*"]
  
  test_out[, zt := t / behavr::hours(1)]



  sleep_period <- do.call(rbind, lapply(periods, function(zt) {
    t1 <- behavr::hours(zt[1])
    t2 <- behavr::hours(zt[2])
    out <- dt_bin_full[
      (t >= t1 & t < t2),
      .(asleep = sum(asleep)),
      by = .(id, Training, Files, PRE_ROI, POST_ROI)
    ]
    out[, interval := paste0("ZT", stringr::str_pad(zt[1], pad = "0", width = 2), "-ZT", stringr::str_pad(zt[2], pad = "0", width = 2))]
  }))

  return(list(data = summ_data, significance = test_out, periods = sleep_period))
}
