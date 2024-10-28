source("R/tests.R", local = T)
source("R/utils.R", local = T)
source("R/constants.R", local = T)
source("R/Cbind.R", local = T)
source("R/plot.R", local = T)
source("R/themes.R", local = T)
source("R/learning_plot.R", local = T)
source("R/summary_plot.R", local = T)

data <- data.table::fread(file = "tidy_data_wide.csv")

wild_types <- c("Iso31", "MB010B.(II)SPARC-Chrimson ISO", "MB010B.(II)SPARC-GFP ISO")
experiments <- c("24hr LTM", "24hr LTM CXM", "20min STM")
valid_reasons <- c("", "?", "Human-override", "Machine-override", "AOJ-override")

data <- data[
  PRE_Reason %in% valid_reasons &
    POST_Reason %in% valid_reasons &
    interval == "No_stimulator",
]
data <- data[food == "cornmeal" | (food == "NONE" & experiment == "20min STM")]
panel3_data <- data[(
  experiment %in% c("24hr LTM", "24hr LTM CXM") &
    Genotype %in% wild_types &
    Training == "6X_Spaced"
) | (
  Genotype %in% "orb2"
)]

groups <- c("24hr LTM-Iso31", "24hr LTM CXM-Iso31", "24hr LTM-orb2", "20min STM-orb2")

panel3_data[, Group := paste(experiment, ifelse(Genotype == "orb2", "orb2", "Iso31"), sep = "-")]
panel3_data[, Group := factor(Group, levels = groups)]

columns <- c("idoc_folder", "PRE_ROI", "POST_ROI", "User", "SD_status", "interval", "Genotype", "experiment", "PRE", "POST")
export_csvs(panel3_data, "Group", groups, 3, columns)

panel3_data_long <- melt_idoc_data(panel3_data)


panel3A <- learning_plot(
  panel3_data_long, "Group",
  y_limits = c(-1, 1),
  y_annotation = 1.3,
  colors = colors_panel3,
  vjust = 0,
  text_hjust = 1,
  text_vjust = 1,
  textsize = 5
)
panel3B <- summary_plot(
  panel3_data_long,
  "Group",
  comparisons = list(
    c("24hr LTM CXM-Iso31", "24hr LTM-Iso31"),
    c("24hr LTM-orb2", "24hr LTM-Iso31"),
    c("24hr LTM-orb2", "20min STM-orb2")
  ),
  annotation_y = c(1, 1.2, 1),
  y_limits = c(-1, 1),
  colors = colors_panel3,
  percentile = c(0.025, 0.975),
  geom = "violin+sina",
  text_hjust = 0.5,
  text_vjust = 1.5,
  vjust = .2,
  textsize = 5
)
panel3B


panelA <- panel3A$gg + guides(fill = "none", color = "none")
panelB <- panel3B$gg +
  guides(fill = "none", color = "none") + scale_color_manual(
    labels = c("24hr LTM Iso31", "24hr LTM Iso31 + CXM", "24hr LTM *orb2Δ*", "20min *orb2Δ*"),
    values = colors_panel3
  )

design <- "
######
AAABBB
######
CCC###
"

gg <- ggplot() +
  learning_plot_theme +
  panelA +
  panelB  +
  plot_annotation(tag_levels = list(c("A", "B", "C"))) +
  plot_layout(design = design, heights = c(.4, 1, .4, 1)) &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = PLOT_TAG_SIZE, vjust = +13),
    legend.text = ggtext::element_markdown(size = LEGEND_TEXT_SIZE, hjust = 0.5)
  )
gg
suppressWarnings({
  ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig3/Figure_3.pdf"), width = 210, height = 240, units = "mm")
  ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig3/Figure_3.svg"), width = 210, height = 240, units = "mm")
  print(gg)
})
