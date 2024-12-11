# source("tests.R")
# source("tests.R")
# source("constants.R")
library(data.table)
library(ggplot2)
library(ggsignif)


preprocess_summary_data_deltaPI <- function(data, group, test, value.var = "PI") {
  test__ <- NULL
  data$group__ <- data[[group]]
  data$test__ <- data[[test]]
  data <- dcast(data, id + group__ ~ test__, value.var = value.var)
  data[[value.var]] <- data$POST - data$PRE
  return(data)
}

preprocess_summary_data_postPI <- function(data, group, test, value.var = "PI") {
  test__ <- NULL
  data$group__ <- data[[group]]
  data$test__ <- data[[test]]
  data <- data[test == "POST", ]
  data[[test]] <- data$test__
  data[, test__ := NULL]
  return(data)
}

preprocess_function_default <- preprocess_summary_data_postPI
y_axis_label_default <- "Post-conditioning PI"

# preprocess_function_default <- preprocess_summary_data_deltaPI
# y_axis_label_default <- "ΔPI"

summary_plot <- function(
    data, group, comparisons,
    annotation_y,
    test = unpaired_t_test,
    map_signif_level = TRUE,
    colors = NULL,
    x_labels_angle = 0,
    starsize = STARSIZE,
    y_limits = NULL,
    percentile = c(0.025, 0.975),
    preprocess_function = preprocess_function_default,
    y_axis_label = y_axis_label_default,
    expansion_y_top = EXPANSION_Y_TOP,
    expansion_y_bottom = EXPANSION_Y_BOTTOM,
    y_breaks = waiver(),
    geom = "boxplot",
    textsize = N_TEXT_SIZE,
    point_size = SUMMARY_PLOT_POINT_SIZE,
    family = FONT,
    vjust = 0,
    text_hjust = 0.5,
    text_vjust = 0,
    y_annotation_n = -1,
    angle_n = 0) {
  PI <- group__ <- . <- outlier <- N <- NULL
  stopifnot(length(comparisons) == length(annotation_y))

  data <- data.table::copy(data)
  data <- preprocess_function(
    data = data, group = group,
    test = "test", value.var = "PI"
  )
  data[, outlier := FALSE]
  
  stopifnot("PI" %in% colnames(data))

  if (!is.null(colors)) {
    gg <- ggplot(data = data, aes(x = group__, y = PI, fill = group__))
  } else {
    gg <- ggplot(data = data, aes(x = group__, y = PI))
  }
  data_summ <- data[, .(
    ymin = quantile(PI, percentile[1]),
    lower = quantile(PI, 0.25),
    middle = median(PI),
    upper = quantile(PI, 0.75),
    ymax = quantile(PI, percentile[2]),
    N = .N
  ), by = .(group__)]
  for (grp in data_summ$group__) {
    data[group__ == grp & (PI < data_summ[group__ == grp, ymin] | PI > data_summ[group__ == grp, ymax]), outlier := TRUE]
  }

  thickness <- 1.5
  if (geom == "boxplot") {
    gg <- gg +
      geom_boxplot(data = data_summ, stat = "identity", aes(x = group__, y = NULL, ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax), size = thickness, fatten = 0.75) +
      geom_segment(data = data_summ, aes(x = as.numeric(group__) - 0.25, xend = as.numeric(group__) + 0.25, y = ymin, yend = ymin), linewidth = thickness) + # Lower whisker
      geom_segment(data = data_summ, aes(x = as.numeric(group__) - 0.25, xend = as.numeric(group__) + 0.25, y = ymax, yend = ymax), linewidth = thickness) + # Upper whisker
      geom_jitter(data = data[outlier == TRUE, ], aes(x = group__, y = PI), width = 0.1)
  } else if (geom == "sina") {
    gg <- gg + ggforce::geom_sina(data = data, aes(x = group__, y = PI, col = group__), size = point_size)
  } else if (geom == "violin+sina") {
    median_data <- data[, .(PI = mean(PI)), by=group__]
    gg <- gg + geom_violin(data = data, aes(x = group__, y = PI), fill = NA, col = "black") +
    ggforce::geom_sina(data = data, aes(x = group__, y = PI, col = group__), size = point_size) +
    geom_segment(
      data = median_data,
      mapping = aes(x=as.numeric(group__)-0.2, xend = as.numeric(group__)+0.2, y=PI),
      linewidth = 2, alpha=0.7
    )
    
  }
  if (!is.null(colors)) {
    if (!length(colors) == length(unique(data$group))) {
      print(paste0(length(colors) != length(unique(data$group))))
    }
    gg <- gg +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors)
  }
  if (!is.null(y_annotation_n)) {
      
    gg <- add_n_annotation(
      gg,
      data_summ,
      x_annotation = NULL,
      y_annotation = y_annotation_n,
      text_vjust = text_vjust,
      text_hjust = text_hjust,
      family = family,
      textsize = textsize,
      angle = angle_n
    )
  }

  if (x_labels_angle == 0) {
    gg <- gg + scale_x_discrete(name = "", expand=expansion(add=c(0.5, 0.5)))
  } else {
    gg <- gg + scale_x_discrete(name = "", expand=expansion(add=c(0.5, 0.5)))
  }

  gg <- gg + scale_y_continuous(
    name = y_axis_label, breaks = y_breaks,
    expand = expansion(add = c(0, 0))
  )

  for (i in seq_along(comparisons)) {
    comparison <- comparisons[[i]]
    
    n1 <- data_summ[group__ == comparison[1], N]
    n2 <- data_summ[group__ == comparison[2], N]
    if (n1 > 2 & n2 > 2) {
      gg <- gg + geom_signif(
        comparisons = list(comparison),
        y_position = annotation_y[i],
        map_signif_level = map_signif_level,
        tip_length = 0,
        test = test,
        family = family,
        vjust = vjust,
        textsize = starsize,
        size = 1
      )
      test_out <- test(
        data[group__ == comparison[1], PI],
        data[group__ == comparison[2], PI],
        alternative = "greater"
      )
    } else {
      test_out <- list(p.value = NA, estimate = NA)
    }
    print(paste0(
      "Comparison ", paste(comparison, collapse = " vs "),
      " P value: ", test_out$p.value,
      " Effect size: ", test_out$estimate
    ))
  }
  gg <- gg + coord_cartesian(
    clip = "off",
    # xlim=0.5+c(0, length(unique(data_summ$group__))),
    ylim = y_limits
  ) + summary_plot_theme

  n_facets <- length(unique(data$group__))
  data$group__ <- NULL
  return(list(gg = gg, n_facets = n_facets))
}



save_summ_plot <- function(plot, ratio, size_unit = 5, ...) {
  width <- plot$n_facets * size_unit
  height <- size_unit * ratio
  svg(width = width, height = height, ...)
  print(plot$gg)
  dev.off()
}

