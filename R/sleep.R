
preprocess_sleep_data <- function(data, group) {
  if (!is.null(group)) {
    if (!(group %in% colnames(data))) {
      data$group__ <- group
    } else {
      data$group__ <- data[[group]]
    }
  } else {
    data$group__ <- "A"
  }


  dt_sleep <- melt(
    data = data[test == "POST", ],
    id.vars = c("fly_name_reference", "group__"),
    value.name = "value",
    variable.name = "ZT",
    measure.vars = zts$zt_all,
  )
  dt_interactions <- melt(
    data = data[test == "POST", ],
    id.vars = c("fly_name_reference", "group__"),
    value.name = "value",
    variable.name = "ZT",
    measure.vars = paste0(zts$zt_all, "_interactions"),
  )

  dt <- rbind(
    cbind(dt_sleep, variable = "asleep"),
    cbind(dt_interactions, variable = "interactions")
  )


  dt[, t := as.numeric(gsub(x = gsub(pattern = "ZT", x = ZT, replacement = ""), pattern = "_interactions", replacement = "")) * 3600]
  dt <- dt[order(fly_name_reference), ]
  missing_data <- dt[variable == "asleep", .(missing = all(is.na(value))), by = fly_name_reference]
  missing_data_animals <- missing_data[missing == TRUE, fly_name_reference]
  dt <- dt[!(fly_name_reference %in% missing_data_animals), ]
  dt_summ <- dt[, .(mu = mean(value, na.rm = TRUE), sigma = sd(value, na.rm = TRUE), n = .N), by = .(t, group__, variable)]
  dt_summ[, sem := sigma / sqrt(n)]
  # dt_summ <- dt_summ[ t >= behavr::hours(5),]
  return(list(dt = dt, dt_summ = dt_summ))
}

sleep_plot <- function(dt, point_size = 3, line_size = 2, color_by_group = TRUE, colors = NULL) {
  y_lab <- "Sleep (per 30 min bin)"

  if (color_by_group) {
    stopifnot(length(colors) == length(unique(dt$group__)))
    sleep_plot <- ggplot(data = dt[variable == "asleep", ], aes(x = t, y = mu, color = group__, group = group__)) +
      # ggforce::geom_circle(aes(x0=t, y0=mu, r=.05)) +
      geom_point(size = point_size) +
      geom_line(size = line_size) +
      geom_errorbar(aes(ymax = mu + sem, ymin = mu - sem)) +
      scale_y_continuous(name = y_lab, breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 30, limits = c(0, 1)) +
      scale_color_manual(values = colors)
  } else {
    transformation_factor <- 0.1
    dt[, mu_transformed := mu]
    dt[, sem_transformed := sem]

    dt[variable == "interactions", mu_transformed := mu * transformation_factor]
    dt[variable == "interactions", sem_transformed := sem * transformation_factor]
    print(transformation_factor)

    sleep_plot <- ggplot() +
      # ggforce::geom_circle(aes(x0=t, y0=mu, r=.05)) +
      geom_point(data = dt[variable == "asleep", ], size = 3, aes(x = t, color = variable, group = variable, y = mu)) +
      geom_line(data = dt[variable == "asleep", ], aes(x = t, color = variable, group = variable, y = mu)) +
      geom_errorbar(data = dt[variable == "asleep", ], aes(x = t, color = variable, group = variable, y = mu, ymax = mu + sem, ymin = mu - sem)) +
      geom_point(data = dt[variable == "interactions", ], size = 3, aes(x = t, color = variable, group = variable, y = mu_transformed)) +
      geom_line(data = dt[variable == "interactions", ], aes(x = t, color = variable, group = variable, y = mu_transformed)) +
      geom_errorbar(data = dt[variable == "interactions", ], aes(x = t, color = variable, group = variable, y = mu_transformed, ymax = mu_transformed + sem_transformed, ymin = mu_transformed - sem_transformed)) +
      scale_y_continuous(
        name = ylab, breaks = seq(0, 1, 0.2), labels = seq(0, 1, 0.2) * 30,
        sec.axis = sec_axis(trans = ~ . / transformation_factor, name = "Interactions")
      ) +
      scale_color_manual(values = c("interactions" = "blue", "asleep" = "black"))
  }
  sleep_plot <- sleep_plot +
    scale_x_hours(breaks = behavr::hours(seq(4, 30, 2)), labels = seq(4, 30, 2) %% 24, name = "ZT") +
    stat_ld_annotations()

  return(sleep_plot)
}
