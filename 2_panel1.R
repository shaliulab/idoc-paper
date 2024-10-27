source("R/tests.R", local = T)
source("R/utils.R", local = T)
source("R/constants.R", local = T)
source("R/Cbind.R", local = T)
source("R/plot.R", local = T)
source("R/themes.R", local = T)
source("R/learning_plot.R", local = T)
source("R/summary_plot.R", local = T)

data <- data.table::fread(file = "tidy_data_wide.csv")

# No Iso31 should be used here because
# 1) Unpaired data is only SPARC-Chrimson
# 2) Iso31 data is more stupid and comes from IDOC 2
wts <- c("MB010B.(II)SPARC-Chrimson ISO", "MB010B.(II)SPARC-GFP ISO")
experiments <- c("20min STM", "20min STM unpaired", "20min STM dunce", "20min STM orco")
valid_reasons <- c("", "?", "Human-override", "Machine-override", "AOJ-override")
panel1_data <- data[
  PRE_Reason %in% valid_reasons &
  POST_Reason %in% valid_reasons &
  Genotype %in% wts &
  experiment %in% experiments,
]

data[, experiment := factor(experiment, levels = experiments)]

columns <- c("idoc_folder", "PRE_ROI", "POST_ROI", "Genotype", "experiment", "PRE", "POST")
export_csvs(panel1_data, "experiment", experiments, 1, columns)

panel1_data_long <- melt_idoc_data(panel1_data)


panel1A <- learning_plot(
  panel1_data_long,
  "experiment",
  direction = "horizontal",
  y_annotation = 1,
  colors = colors_panel1[1:length(unique(panel1_data_long$experiment))],
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, 0.5),
  distribution_color = "#CBCBCB",
  text_vjust = 1.5
)

panel1B <- summary_plot(
  panel1_data_long,
  group = "experiment",
  test=unpaired_t_test,
  colors=colors_panel1,
  comparisons=list(c("20min STM unpaired", "20min STM")),
  annotation_y=1.0,
  y_limits=c(-1, 1),
  y_breaks=seq(-1, 1, 0.5),
  preprocess_function=preprocess_summary_data_postPI,
  geom="violin+sina",
  text_vjust=1.5
)

panelA <- panel1A$gg + guides(color="none") +
  scale_fill_manual(values=colors_panel1, labels=c("Paired", "Unpaired"))
panelB <-  panel1B$gg +
  guides(fill="none", color="none")
gg <- guide_area() + panelA + plot_spacer() + panelB + plot_spacer() + 
  plot_annotation(tag_levels = list(c("E", "F"))) +
  plot_layout(guides = "collect", heights=c(.2,  1, .2, .7, .2), nrow=6) & 
  theme(legend.position = "top") 
ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig1/Figure_1.pdf"), width=100, height=230, unit="mm")
ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig1/Figure_1.svg"), width=100, height=230, unit="mm")