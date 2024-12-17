library(ggplot2)
library(data.table)


make_schematic <- function(n=4, point_size=5) {
  x <- y <- NULL

  # Data setup
  orb2dq <- "orb2ΔQ"

  data <- data.table(
    Group = rep(
      c("24 hr LTM", "20 min STM", "CXM", orb2dq),
      times = c(1, 2, 2, 2)
    ),
    x = c(1, 2, 2, 3, 3, 4, 4),
    y = c(4, 4, 2, 4, 1, 3, 1)
  )
  data[, Group := factor(Group, levels = c(orb2dq, "CXM", "20min STM", "24hr LTM"))]
  data[, x := x*(n*2)/4-1]

  print(head(data))
  gg <- ggplot(data, aes(x = x, y = y)) +
    geom_point(size = point_size) + # Add points
    scale_y_continuous(
      expand = expansion(add=c(0.5, .5)),
      limits = c(0, 4), breaks = 1:4,
    ) +
    theme_void() +
    scale_x_continuous(limits = c(0, n * 2), expand = expansion(add=c(0, 0))) +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      strip.text.x = element_blank()
    ) +
    geom_hline(yintercept = 2:4-0.5, linetype = "solid", linewidth = 1) # Add horizontal lines
  
  return(gg)
}