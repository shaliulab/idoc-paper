library(ggtext)
library(ggprism)

sleep_plot_theme <- theme_prism() + theme(
  axis.text = element_text(size = TEXT_SIZE),
  axis.title = element_text(size = TITLE_SIZE),
  axis.text.y = element_text(size = TEXT_SIZE),
  strip.text = element_blank(),
  plot.tag = element_text(size = PLOT_TAG_SIZE),
  legend.key.width = unit(1, "null"),
  legend.text = ggtext::element_markdown(size = LEGEND_TEXT_SIZE)
)

learning_plot_theme <- sleep_plot_theme + theme(
  axis.title.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank(),
  axis.line.x = element_blank()
)
summary_plot_theme <- learning_plot_theme
