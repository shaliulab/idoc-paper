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
genotypes <- c("MB010B.(II)SPARC-Chrimson ISO", "MB010B.(II)SPARC-GFP ISO", "dnc")

experiments <- c("20min STM", "20min STM unpaired", "20min STM dunce", "20min STM orco", "Learning all ITO", "Learning")
groups <- c("20min STM", "20min STM unpaired", "orco", "dunce")
valid_reasons <- c("", "?", "Human-override", "Machine-override", "AOJ-override")
panel1_data <- data[
  PRE_Reason %in% valid_reasons &
    POST_Reason %in% valid_reasons &
    Genotype %in% genotypes &
    experiment %in% experiments,
]

panel1_data[, Group := experiment]
panel1_data[Genotype == "dnc" & experiment %in% c("Learning all ITO", "Learning"), Group := "dunce"]
panel1_data[Genotype == "dnc" & experiment %in% c("20min STM"), Group := "dunce"]
panel1_data[Genotype == "orco" & experiment %in% c("Learning all ITO", "Learning"), Group := "orco"]
panel1_data[Genotype == "orco" & experiment %in% c("20min STM"), Group := "orco"]

panel1_data[, experiment := factor(experiment, levels = experiments)]
unique(panel1_data$Group)

groups_in_figure_1 <- c("20min STM", "20min STM unpaired", "dunce", "orco")
panel1_data <- panel1_data[
  Group %in% groups_in_figure_1
]
panel1_data[, Group := factor(Group, levels = groups)]



columns <- c("idoc_folder", "PRE_ROI", "POST_ROI", "Genotype", "experiment", "PRE", "POST")
export_csvs(panel1_data, "Group", groups, 1, columns)

panel1_data[Group == "dunce", c("POST_1", "POST_2", columns), with=F]

panel1_data_long <- melt_idoc_data(panel1_data)[, .(Group, id, PI, test)]
panel1_data_long <- rbind(
  panel1_data_long,
  data.table(
    Group = c("orco", "orco"),
    id = c(-1, -1),
    PI = c(0, 0),
    test = c("PRE", "POST")
  )
)


panel1A <- learning_plot(
  panel1_data_long,
  "Group",
  direction = "horizontal",
  y_annotation = 1,
  colors = colors_panel1[1:length(groups_in_figure_1)],
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, 0.5),
  text_vjust = 1.5,
  angle_n = 0
)
panel1A$gg

panel1B <- summary_plot(
  panel1_data_long,
  group = "Group",
  colors = colors_panel1,
  comparisons = list(
    c("20min STM unpaired", "20min STM"),
    c("orco", "20min STM"),
    c("dunce", "20min STM")
  ),
  annotation_y = c(0.75, 0.9, 1.05),
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, 0.5),
  geom = "violin+sina",
  text_vjust = 1.5
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
 DDEE
 DDFF
 DDFF
 DDFF
 DDFF
"


gg <- template + template +
  template + template +
  (panelA + theme(plot.margin = unit(c(0, 0, 30, 0), "pt"))) + 
  (panelB + theme(plot.margin = unit(c(10, 0, 0, 0), "pt"))) + 
  plot_annotation(tag_levels = list(c(LETTERS[1:6]))) +
  plot_layout(design=design) & theme(
    plot.tag = element_text(size = PLOT_TAG_SIZE, vjust = +3)
  )
gg
ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig1/Figure_1.pdf"), width = 210, height = 297, unit = "mm")
ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig1/Figure_1.svg"), width = 210, height = 297, unit = "mm")
gg
