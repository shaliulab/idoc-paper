source("R/tests.R", local = T)
source("R/utils.R", local = T)
source("R/constants.R", local = T)
source("R/Cbind.R", local = T)
source("R/plot.R", local = T)
source("R/themes.R", local = T)
source("R/learning_plot.R", local = T)
source("R/summary_plot.R", local = T)

data <- data.table::fread(file = "tidy_data_wide.csv")
sheets <- c("20min STM", "1hr STM", "3hr STM")
experiments <- c("20min STM", "1hr STM", "3hr STM")
wts <- c("Iso31", "MB010B.(II)SPARC-Chrimson ISO", "MB010B.(II)SPARC-GFP ISO")
valid_reasons <- c("", "?", "Human-override", "Machine-override", "AOJ-override")
panel2_data <- data[
  PRE_Reason %in% valid_reasons &
    POST_Reason %in% valid_reasons &
    experiment %in% experiments &
    (Genotype != "Iso31" | experiment != "20min STM") &
    Genotype %in% wts
]
panel2_data[, experiment := factor(experiment, levels = experiments)]


columns <- c("idoc_folder", "PRE_ROI", "POST_ROI", "Genotype", "experiment", "PRE", "POST")
export_csvs(panel2_data, "experiment", experiments, 2, columns)

panel2_data_long <- melt_idoc_data(panel2_data)

panel2A <- learning_plot(
  panel2_data_long, "experiment", "horizontal",
  y_limits = c(-1, 1),
  colors = colors_panel2[1:length(experiments)], test = paired_t_test,
  map_signif_level = TRUE,
  y_annotation = 0.75,
  textsize = 4,
  angle_n = 45,
  text_vjust = 1.5,
  text_hjust = 1
)

ref_pi <- panel2A$annotation[test == "POST" & group__ == "20min STM", PI]
df <- panel2A$annotation[test == "POST", .(group__, norm_PI = PI / ref_pi, std_error)]
ggplot(data = df, aes(x = group__, y = norm_PI, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = group__, ymin = norm_PI - std_error / ref_pi, ymax = norm_PI + std_error / ref_pi), width = .1) +
  scale_x_discrete(labels = c(0.3, 1, 3), name = "Time after training (h)") +
  scale_y_continuous(limits = c(0, 2), breaks = c(0, 1), labels = c(0, 100), name = "STM %") +
  geom_hline(yintercept = 1, linetype = "dashed")



panel2B <- summary_plot(
  data = panel2_data_long,
  group = "experiment",
  comparisons = list(
    c("3hr STM", "20min STM"),
    c("3hr STM", "1hr STM")
  ),
  map_signif_level = T,
  annotation_y = c(0.85, 0.65),
  colors = colors_panel2[1:length(experiments)],
  y_limits = c(-1, 1),
  percentile = c(0.025, 0.975),
  geom = "violin+sina",
  text_vjust = 1.5,
  textsize = 4,
  angle_n = 45,
  text_hjust = 1
)


panelA <- panel2A$gg + guides(color = "none", fill = "none")
panelB <- panel2B$gg + guides(color = "none", fill = "none")
design <- "
  AAAAABBBCCC
"
gg <- ggplot() +
  learning_plot_theme +
  panelA +
  panelB +
  plot_spacer() +
  plot_spacer() +
  plot_spacer() +
  plot_annotation(tag_levels = list(c("A", "B", "C"))) +
  plot_layout(design = design) &
  theme(
    plot.margin = unit(c(0,0,20,0), "pt"),
    plot.tag = element_text(size = PLOT_TAG_SIZE, vjust = +0)
  )
gg

ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig2/Figure_2.pdf"), width = 190, height = 100, unit = "mm")
ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig2/Figure_2.svg"), width = 190, height = 100, unit = "mm")
