source("R/tests.R", local = T)
source("R/utils.R", local = T)
source("R/constants.R", local = T)
source("R/Cbind.R", local = T)
source("R/plot.R", local = T)
source("R/themes.R", local = T)
source("R/learning_plot.R", local = T)
source("R/summary_plot.R", local = T)

data <- data.table::fread(file = "tidy_data_wide.csv")
genotypes <- c("MB010B.(II)SPARC-Chrimson ISO", "MB010B.(II)SPARC-GFP ISO")

experiments <- c("20min STM", "20min STM unpaired")
groups <- c("20min STM", "20min STM unpaired")
CSs <- c("OCT")
valid_reasons <- c("", "?", "Human-override", "Machine-override", "AOJ-override")
panel1_data <- data[
  PRE_Reason %in% valid_reasons &
    POST_Reason %in% valid_reasons &
    Genotype %in% genotypes &
    experiment %in% experiments &
    CS %in% CSs
]

panel1_data[, Group := experiment]
panel1_data[, experiment := factor(experiment, levels = experiments)]
groups_in_figure_1 <- c("20min STM", "20min STM unpaired")
groups_in_figure_1 <- experiments
comparisons <- list(
  c("20min STM unpaired", "20min STM")
)


panel1_data <- panel1_data[
  Group %in% groups_in_figure_1
]
panel1_data[, Group := factor(Group, levels = groups_in_figure_1)]


panel1_data[experiment=="20min STM unpaired", .(Files, PRE_ROI)]

columns <- c("idoc_folder", "PRE_ROI", "POST_ROI", "Genotype", "experiment", "CS", "PRE", "POST")
export_csvs(panel1_data, "Group", groups, 1, columns)

panel1_data_long <- melt_idoc_data(panel1_data)[, .(Group, id, PI, test, Genotype, CS)]

panel1A <- learning_plot(
  panel1_data_long,
  "Group",
  direction = "horizontal",
  y_annotation = 1,
  colors = colors_panel1[1:length(groups_in_figure_1)],
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, 0.5),
  text_vjust = 1.5,
  angle_n = 0,
  offset = 0.25,
  correction = "bonferroni"
)

panel1B <- summary_plot(
  panel1_data_long,
  group = "Group",
  colors = colors_panel1,
  comparisons = comparisons,
  annotation_y = c(1, 0.9, 1.05)[1:length(comparisons)],
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, 0.5),
  geom = "violin+sina",
  text_vjust = 1.5,
  correction = "bonferroni"
)
panelA <- panel1A$gg + guides(color = "none") +
  scale_fill_manual(values = colors_panel1, labels = c("Paired", "Unpaired"))
panelB <- panel1B$gg +
  guides(fill = "none", color = "none")


template <- ggplot() + learning_plot_theme


design <- "
 AABB
 AABB
 AABB
 AACC
 AACC
 DDEE
 DDEE
 DDFF
 DDFF
 DDFF
"


gg <- template + template +
  template + template +
  (panelA + 
     theme(plot.margin = unit(c(0, 0, 40, 0), "pt"))) + 
  (panelB + 
     theme(plot.margin = unit(c(30, 0, 0, 0), "pt"))) + 
  plot_annotation(tag_levels = list(c(LETTERS[1:6]))) +
  plot_layout(design=design)
gg
ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig1/Figure_1.pdf"), width = 210, height = 277, unit = "mm")
ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig1/Figure_1.svg"), width = 210, height = 277, unit = "mm")
gg
