source("R/tests.R", local = T)
source("R/utils.R", local = T)
source("R/constants.R", local = T)
source("R/Cbind.R", local = T)
source("R/plot.R", local = T)
source("R/themes.R", local = T)
source("R/learning_plot.R", local = T)
source("R/summary_plot.R", local = T)


data <- data.table::fread(file = "tidy_data_wide.csv")

experiments <- c("24hr LTM")
wild_types <- c("Iso31")
intervals <- c("No_stimulator", "ZT05-ZT11")
valid_reasons <- c("", "?", "Human-override", "Machine-override", "AOJ-override")

panel5_data <- data[
  PRE_Reason %in% valid_reasons &
    POST_Reason %in% valid_reasons &
    region_id != "NONE" &
    interval %in% intervals &
    experiment %in% experiments &
    Genotype %in% wild_types &
    Training == "6X_Spaced" &
    substr(Files, 1, 4) == "2023",
]
panel5_data[, interval := factor(interval, levels=intervals)]

columns <- c(
  "Files", "idoc_folder", "PRE_ROI", "POST_ROI", "User",
  "Genotype", "experiment", "PRE", "POST", "SD_status", "interval"
)

panel5_data[interval=="ZT05-ZT11", .(n = .N), by=Files]


export_csvs(panel5_data, "interval", intervals, 5, columns)

panel5_data_long <- melt_idoc_data(panel5_data)
panel5A <- learning_plot(
  panel5_data_long, "interval",
  map_signif_level = TRUE,
  test = paired_t_test,
  colors = colors_panel5[1:length(intervals)],
  y_annotation = 1.1,
  y_limits = c(-1, 1),
  text_hjust = 1,
  text_vjust = 2.5,
  textsize = 3
)
panel5A$gg <- panel5A$gg + scale_x_discrete(expand = expansion(add=c(.25,.25)))

panel5B <- summary_plot(
  data = panel5_data_long, "interval",
  comparisons = list(
    c("ZT05-ZT11", "No_stimulator")
  ),
  annotation_y = 1.1,
  test = unpaired_t_test,
  colors = colors_panel5[1:length(intervals)],
  y_limits = c(-1, 1),
  percentile = c(0.025, 0.975),
  map_signif_level = T,
  geom = "violin+sina",
  textsize = 3,
  text_hjust = 1,
  text_vjust = 2.5,
  angle_n = 45
)
design <- "ABC"
gg <- plot_spacer() + plot_spacer() + plot_spacer() + ggplot() + learning_plot_theme + panel5A$gg + guides(color = "none") +
  scale_fill_manual(values = colors_panel5, labels = c("No stimulator", "ZT05-ZT11")) +
  (panel5B$gg + guides(color = "none", fill = "none")) + plot_spacer() + plot_spacer() + plot_spacer() +
  plot_annotation(tag_levels = list(c("A", "B", "C"))) +
  plot_layout(nrow = 3, heights = c(.2, 1, .2), widths = c(1.2, .4, .6), guides = "collect") &
  theme(legend.position = "none")

gg
ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig5/Figure_5.pdf"), width = 210, height = 100, unit = "mm", dpi="retina")
ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig5/Figure_5.svg"), width = 210, height = 100, unit = "mm", dpi="retina")
