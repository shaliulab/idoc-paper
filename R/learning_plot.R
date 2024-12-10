# source("tests.R")
# source("utils.R")
# source("constants.R")
library(data.table)
library(ggplot2)
library(ggsignif)

#' @param data Data frame with columns:
#' * PI: numeric from -1 to 1
#' * test: one of PRE or POST
#' * id: unique to each animal. The same animal must have one PRE and one POST value,
#'     and no more
#' * a column named according to the input argument 'group', used to separate
#'     animals by some category e.g. genotype, treatment, etc
#'  @param group A column in the data frame data, see argument data 
learning_plot <- function(
    data, group, direction = "horizontal",
    test = paired_t_test,
    map_signif_level = TRUE,
    y_limits = c(-1, 1),
    trend_statistic = TREND_STATISTIC,
    error_statistic = ERROR_STATISTIC,
    colors = NULL,
    y_annotation = NULL,
    x_annotation = 1.5,
    text_hjust = 0.5,
    y_annotation_n = -1,
    text_y_size = TEXT_SIZE,
    title_y_size = TITLE_SIZE,
    starsize = STARSIZE,
    textsize = N_TEXT_SIZE,
    y_step = 0.5,
    expansion_y_bottom = EXPANSION_Y_BOTTOM,
    expansion_y_top = EXPANSION_Y_TOP,
    y_breaks = waiver(),
    distribution_color = DISTRIBUTION_COLOR,
    linewidth = LINEWIDTH,
    point_size = POINT_SIZE,
    linewidth_mean = LINEWIDTH_MEAN,
    point_size_mean = POINT_SIZE_MEAN,
    family = FONT,
    vjust = VJUST,
    angle_n = 45,
    text_vjust = 0,
    offset=0
    ) {
  if (is.null(group)) {
    df$group__ <- "A"
  } else if (!(group %in% colnames(data))) {
    data$group__ <- group
  } else {
    data$group__ <- data$group__ <- data[[group]]
  }

  . <- std_error <- id <- annotations <- x <- PI <- group__ <- N <- NULL

  group <- "group__"

  annotation_df <- make_annotation_df(
    df = data, variable = group,
    test = test,
    trend_statistic = trend_statistic,
    error_statistic = error_statistic,
    alt = "greater"
  )
  data$x <- ifelse(data$test == "PRE", 1+offset, 2-offset)
  annotation_df$x <- ifelse(annotation_df$test == "PRE", 1+offset, 2-offset)
  n_facets <- length(unique(data$group__))

  panel <- ggplot(data = data, aes(x = x, y = PI)) +
    geom_point(
      size = point_size,
      color = distribution_color
    ) +
    geom_line(
      aes(group = id),
      color = distribution_color,
      linewidth = linewidth
    )
  # browser()
  panel <- add_trend_geom(
    panel, annotation_df,
    colors = colors,
    point_size = point_size_mean,
    linewidth = linewidth_mean,
    ERRORBAR_WIDTH
  )
  panel <- add_n_annotation(
    panel, annotation_df,
    text_vjust = text_vjust,
    text_hjust = text_hjust,
    textsize = textsize,
    family = family,
    x_annotation = x_annotation,
    y_annotation = y_annotation_n,
    angle = angle_n
  )
  panel <- add_facet(panel, direction)
  panel <- panel +
    scale_y_continuous(breaks = y_breaks, expand = expansion(add = c(0, 0))) +
    scale_x_continuous(expand = expansion(add=c(0,0)))
  

  if (!is.null(test)) {
    panel <- tryCatch({
      add_significance_marks(
        panel, test, annotation_df, y_annotation, vjust,
        textsize = starsize,
        map_signif_level = map_signif_level,
        family = family
      )},
      error = function(e) {
        print(e)
        return(panel)
    })
  }
  panel <- panel + coord_cartesian(clip = "off", ylim = y_limits) + learning_plot_theme

  data$group__ <- NULL
  return(list(
    gg = panel, n_facets = n_facets,
    direction = direction, annotation = annotation_df
  ))
}


save_learning_plot <- function(plot, ratio, size_unit = 5, ...) {
  if (plot$direction == "horizontal") {
    width <- plot$n_facets * size_unit
    height <- size_unit * ratio
  } else if (plot$direction == "vertical") {
    height <- plot$n_facets * size_unit / ratio
    width <- size_unit * ratio
  }
  suppressWarnings({
    svg(width = width, height = height, ...)
    print(plot$gg)
    dev.off()
  })
}

add_trend_geom <- function(
    panel, annotation_df,
    colors = NULL,
    point_size = POINT_SIZE_MEAN,
    linewidth = LINEWIDTH_MEAN,
    errorbar_width = ERRORBAR_WIDTH) {

  error <- group__ <- x <- PI <- NULL
  
  
  margin <- 0.0 # 0.04
  annotation_df[, margin := margin]
  annotation_df[, ymin := ifelse((PI - error) < (PI - margin), (PI - error), (PI - margin))]
  annotation_df[, ymax := ifelse((PI + error) > (PI + margin), (PI + error), (PI + margin))]

  
  if (is.null(colors)) {
    panel <- panel +
      geom_line(
        data = annotation_df,
        aes(
          x = x, y = PI,
          color = group__,
          group = group__
        ),
        linewidth = linewidth
      ) +
      geom_point(
        data = annotation_df,
        mapping = aes(
          x = x, y = PI,
          color = group__,
          group = group__
        ),
        size = point_size
      )
  } else {
    stopifnot(length(colors) == length(unique(annotation_df$group__)))
    panel <- panel +
      geom_line(
        data = annotation_df,
        aes(
          x = x, y = PI,
          col = group__,
          group = group__
        ),
        linewidth = linewidth
      ) +
      geom_point(
        data = annotation_df,
        size = point_size,
        # shape = 1,
        mapping = aes(
          color = group__,
          x = x, y = PI,
          group = group__
        )
      ) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
  }
  

  return(panel)
}

add_significance_marks <- function(
    panel, test, annotation_df, y_annotation,
    vjust, textsize, map_signif_level, family
  ) {
  group__ <- estimate <- p <- NULL

  if (map_signif_level) {
    panel <- panel + geom_signif(
      data = annotation_df,
      mapping = aes(annotations = stars),
      xmin = 1, xmax = 2,
      y_position = y_annotation, test = test,
      manual = TRUE, tip_length = 0,
      family = family, vjust = vjust,
      textsize = textsize, size = 1,
    )
  } else {
    panel <- panel + geom_signif(
      data = annotation_df,
      mapping = aes(annotations = p),
      xmin = 1, xmax = 2,
      y_position = y_annotation, test = test,
      manual = TRUE, tip_length = 0,
      family = family, vjust = vjust
    )
  }

  for (group in unique(annotation_df$group__)) {
    p <- annotation_df[group__ == group, p]
    estimate <- annotation_df[group__ == group, estimate][1]

    print(paste0(
      "Group ", group, " P value: ", p,
      " Effect size: ", estimate
    ))
  }
  return(panel)
}



add_facet <- function(panel, direction) {
  if (direction == "horizontal") {
    panel <- panel + facet_grid(. ~ group__)
  } else if (direction == "vertical") {
    panel <- panel + facet_grid(group__ ~ .)
  }
  return(panel)
}
