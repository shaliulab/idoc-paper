source("R/tests.R", local = T)
source("R/utils.R", local = T)
source("R/constants.R", local = T)
source("R/Cbind.R", local = T)
source("R/plot.R", local = T)
source("R/themes.R", local = T)
source("R/learning_plot.R", local = T)
source("R/summary_plot.R", local = T)
source("4_panel3_schematic.R", local = T)

data <- data.table::fread(file = "tidy_data_wide.csv")

wild_types <- c("Iso31", "MB010B.(II)SPARC-Chrimson ISO", "MB010B.(II)SPARC-GFP ISO")
experiments <- c("24hr LTM", "24hr LTM CXM", "20min STM")
valid_reasons <- c("", "?", "Human-override", "Machine-override", "AOJ-override")

data <- data[
  PRE_Reason %in% valid_reasons &
    POST_Reason %in% valid_reasons &
    interval == "No_stimulator",
]

# data[experiment == "24hr LTM inChamber", experiment := "24hr LTM"]

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
panel3_data[experiment=="24hr LTM", .(n=.N, dPOST=mean(POST-PRE)), by=.(Genotype, Files)]

new_data <- panel3_data[substr(Files, 1, 9)=="2024-12-1",]


columns <- c("idoc_folder", "PRE_ROI", "POST_ROI", "User", "SD_status", "interval", "Genotype", "experiment", "PRE", "POST")
export_csvs(panel3_data, "Group", groups, 3, columns)


panel3_data[, .(n=.N, POST=mean(POST), dPOST=mean(POST-PRE)), by=.(Group)]

panel3_data_long <- melt_idoc_data(panel3_data)
new_data_long <- melt_idoc_data(new_data)
learning_plot(
  new_data_long,
  "Group",
  y_limits = c(-1, 1),
  y_annotation = 1.3,
  y_annotation_n = NULL,
  colors = colors_panel3,
  vjust = 0,
  text_hjust = 1,
  text_vjust = 1,
  textsize = 5,
  point_size_mean = POINT_SIZE_MEAN*0.7,
  offset = 0.15
)

panel3_data[, .(n=.N, POST=mean(POST)), by=.(Group, Genotype)]

panel3A <- learning_plot(
  panel3_data_long,
  "Group",
  y_limits = c(-1, 1),
  y_annotation = 1.3,
  y_annotation_n = NULL,
  colors = colors_panel3,
  vjust = 0,
  text_hjust = 1,
  text_vjust = 1,
  textsize = 5,
  point_size_mean = POINT_SIZE_MEAN*0.7,
  offset = 0.15
)
panel3B <- summary_plot(
  panel3_data_long,
  "Group",
  comparisons = list(
    c("24hr LTM CXM-Iso31", "24hr LTM-Iso31"),
    c("24hr LTM-orb2", "24hr LTM-Iso31"),
    c("24hr LTM-orb2", "20min STM-orb2")
  ),
  annotation_y = c(1.1, 1.25, 1.1),
  y_annotation_n = NULL,
  y_limits = c(-1, 1),
  colors = colors_panel3,
  percentile = c(0.025, 0.975),
  geom = "violin+sina",
  text_hjust = 0.5,
  text_vjust = 1.5,
  vjust = .2,
  textsize = 5
)


panelA <- panel3A$gg + guides(fill = "none", color = "none")
panelB <- panel3B$gg +
  guides(fill = "none", color = "none") + scale_color_manual(
    labels = c("24hr LTM Iso31", "24hr LTM Iso31 + CXM", "24hr LTM *orb2Δ*", "20min *orb2Δ*"),
    values = colors_panel3
  )



point_size<-3
schematic1  <- make_schematic(n=4, point_size=point_size)
schematic2  <- make_schematic(n=4, point_size=point_size)


design <- "
  BBBCCC
"

gg_horizontal <- panelA +
  panelB  +
  plot_annotation(tag_levels = list(c("B", "C"))) +
  plot_layout(design=design) &
  theme(
    legend.position = "bottom",
    plot.margin = unit(c(20,20,20,0), "pt"),
    legend.text = ggtext::element_markdown(size = LEGEND_TEXT_SIZE, hjust = 0.5)
  )
suppressWarnings({
  ggsave(plot = gg_horizontal, filename = paste0(OUTPUT_FOLDER, "/Fig3/Figure_3_horizontal.svg"), width = 120, height = 120*6/9, units = "mm")
  print(gg_horizontal)
})



design <- "
#########
AAAA#BBBB
#####CCCC
DDDD#####
EEEE#####
"
gg <- (ggplot() + learning_plot_theme) +
  (panelA + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))) +
  schematic1 + 
  (panelB + theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))) +
  schematic2 +
  plot_layout(design = design, heights = c(.4, 1, .3, 1, .3)) &
  theme(
    legend.position = "bottom",
    legend.text = ggtext::element_markdown(size = LEGEND_TEXT_SIZE, hjust = 0.5)
  )


suppressWarnings({
  ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig3/Figure_3.pdf"), width = 210, height = 290, units = "mm")
  ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig3/Figure_3.svg"), width = 210, height = 290, units = "mm")
  print(gg)
})


