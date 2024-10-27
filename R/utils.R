library(data.table)
library(ggplot2)

make_annotation_df <- function(df, variable, test, ...) {
  var__ <- std_error <- PI_std <- N <- PI <- . <- NULL

  values <- levels(df[[variable]])
  if (is.null(values)) {
    values <- unique(df[[variable]])
  }
  min_n_points <- 2

  test_out <- lapply(values, function(val) {
    x <- df[df[[variable]] == val & test == "PRE", PI]
    y <- df[df[[variable]] == val & test == "POST", PI]
    if (length(x) < min_n_points | is.null(test)) {
      return(list(p.value = NA, estimate = NA))
    }
    test(
      x = x,
      y = y,
      ...
    )
  })
  p_values <- sapply(test_out, function(x) x$p.value)
  estimates <- sapply(test_out, function(x) x$estimate)


  stars <- ifelse(
    p_values > 0.05,
    "NS",
    ifelse(
      p_values > 0.01,
      "*",
      ifelse(
        p_values > 0.005,
        "**",
        "***"
      )
    )
  )
  p_values <- sapply(p_values, function(p) max(p, 0.001))

  annotation_df <- data.table(
    y_position = 0.5,
    p = round(p_values, 4),
    estimate = estimates,
    stars = stars
  )
  annotation_df[[variable]] <- factor(values, levels = values)
  annotation_df$p <- ifelse(
    annotation_df$p == 0.001,
    "< 0.001",
    as.character(annotation_df$p)
  )
  # divide by two because each fly is represented twice (once for the pre and once for the post)
  annotation_df$N <- sapply(
    annotation_df[[variable]], function(val) {
      nrow(df[df[[variable]] == val, ]) / 2
    }
  )

  df$var__ <- df[[variable]]
  stats_df <- df[, .(
    PI = mean(PI),
    PI_median = median(PI),
    PI_std = sd(PI)
  ), by = .(var__, test)]
  stats_df[[variable]] <- stats_df$var__
  stats_df$var__ <- NULL
  annotation_df <- merge(annotation_df, stats_df, by = variable)

  annotation_df[, std_error := PI_std / sqrt(N)]
  annotation_df$group__ <- annotation_df[[variable]]
  annotation_df
}

add_n_annotation <- function(panel, annotation_df, x_annotation = NULL, y_annotation = -Inf, text_vjust = 0, text_hjust = 0, textsize = TEXT_SIZE, family = FONT, angle = 0) {
  if (!is.null(x_annotation)) {
    panel <- panel +
      geom_text(
        data = annotation_df, y = y_annotation, size = textsize, angle = angle,
        family = family, hjust = text_hjust, vjust = text_vjust,
        mapping = aes(label = paste0("n = ", N)),
        x = x_annotation
      )
  } else {
    panel <- panel +
      geom_text(
        data = annotation_df, y = y_annotation, size = textsize, angle = angle,
        family = family, hjust = text_hjust, vjust = text_vjust,
        mapping = aes(label = paste0("n = ", N), x_annotation = group__),
      )
  }
  return(panel)
}


export_csvs <- function(panel_data, grouping_column, groups, figure_count, columns, y_column = "POST") {
  y_columns <- list()
  available_groups <- c()
  panel_data[["group__"]] <- panel_data[[grouping_column]]

  for (group in groups) {
    panel_data_subset <- panel_data[
      group__ == group,
    ]
    if (!is.null(columns)) {
      panel_data_subset <- panel_data_subset[, columns, with = F]
      if (nrow(panel_data_subset) == 0) {
        warning(paste0("No data found for ", group))
        next
      }
      out <- paste0(OUTPUT_FOLDER, "/Fig", substr(figure_count, 1, 1), "/Figure_", figure_count, "_", group, ".csv")
      message(out)
      data.table::fwrite(
        x = panel_data_subset,
        file = out,
        quote = TRUE
      )
    }
    available_groups <- c(available_groups, group)
    y_columns <- c(y_columns, list(panel_data_subset[[y_column]]))
  }

  y_columns <- Reduce(Cbind, y_columns)
  colnames(y_columns) <- available_groups
  out <- paste0(OUTPUT_FOLDER, "/Fig", substr(figure_count, 1, 1), "/Figure_", figure_count, "_boxplot.csv")
  message(out)
  data.table::fwrite(x = y_columns, file = out, quote = TRUE)
}



melt_idoc_data <- function(panel_data) {
  panel_data_long <- melt(panel_data, measure.vars = c("PRE", "POST"), value.name = "PI", variable.name = "test")
  panel_data_long[, test := factor(test, levels = c("PRE", "POST"))]
  return(panel_data_long)
}
