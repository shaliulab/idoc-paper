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
genotypes <- c("MB010B.(II)SPARC-Chrimson ISO", "MB010B.(II)SPARC-GFP ISO")

experiments <- c("20min STM")
groups <- c("20min STM")
valid_reasons <- c("", "?", "Human-override", "Machine-override", "AOJ-override")
panel1_data <- data[
  PRE_Reason %in% valid_reasons &
    POST_Reason %in% valid_reasons &
    Genotype %in% genotypes &
    experiment %in% experiments,
]

panel1_data[Files=="2024-12-19_14-23-17_ET_010BIso31_STM_MCH", .(Files, Genotype, experiment, PRE, PRE_Reason, POST, POST_Reason)]

panel1_data[, Group := experiment]
panel1_data[, experiment := factor(experiment, levels = experiments)]

groups_in_figure_1 <- c("20min STM")
groups_in_figure_1 <- experiments


panel1_data <- panel1_data[
  Group %in% groups_in_figure_1
]
panel1_data[, Group := factor(Group, levels = groups_in_figure_1)]


columns <- c("idoc_folder", "PRE_ROI", "POST_ROI", "Genotype", "experiment", "CS", "PRE", "POST")
# export_csvs(panel1_data, "Group", groups, 1, columns)
panel1_data_long <- melt_idoc_data(panel1_data)[, .(Group, id, PI, test, Genotype, CS, Files)]

panel1_data_long_by_user <- melt_idoc_data(panel1_data)[, .(Group, id, PI, POST_ROI, test, Genotype, CS, User, Files)]
panel1A_user <- learning_plot(
  panel1_data_long_by_user[CS=="OCT",],
  "User",
  direction = "horizontal",
  y_annotation = 1,
  colors = colors_panel1[1:length(unique(panel1_data_long_by_user$User))],
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, 0.5),
  text_vjust = 1.5,
  angle_n = 0,
  offset=0.25
)

sparcs_data <- panel1_data_long_by_user[grepl(pattern="ET", x=User) & CS=="OCT" & Genotype=="MB010B.(II)SPARC-GFP ISO",]

dcast(sparcs_data, Group + id + Genotype + CS + POST_ROI + User + Files ~ test, value.var="PI")

panel1A_sparcs <- learning_plot(
  sparcs_data,
  "Files",
  direction = "horizontal",
  y_annotation = 1,
  colors = colors_panel1[1:length(unique(panel1_data_long_by_user$User))],
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, 0.5),
  text_vjust = 1.5,
  angle_n = 0,
  offset=0.25
)


panel1_4M1O_data <- panel1_data[CS=="4M1O", .(Files, CS, PRE_ROI, POST_ROI, PRE, POST)]
mean(panel1_4M1O_data$POST)



panel1A_4M1O <- learning_plot(
  panel1_data_long_by_user[CS=="4M1O",],
  "Files",
  direction = "horizontal",
  y_annotation = 1,
  colors = colors_panel1[1:length(unique(panel1_data_long_by_user$User))],
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, 0.5),
  text_vjust = 1.5,
  angle_n = 0,
  offset=0.25
)
panel1A_4M1O

learning_plot(
  panel1_data_long_by_user[CS=="OCT" & User=="ET&AOJ",],
  "User",
  direction = "horizontal",
  y_annotation = 1,
  colors = colors_panel1[1:length(unique(panel1_data_long_by_user$User))],
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, 0.5),
  text_vjust = 1.5,
  angle_n = 0,
  offset=0.25
)

panel1_data_by_odor <- panel1_data[Genotype%in%genotypes & experiment ==  "20min STM" & grepl("ET", x=User),]
panel1_data_by_odor <- panel1_data_by_odor[, .(Group, id, PRE_ROI, POST_ROI, PRE, POST, Genotype, CS, Dilution=factor(Dilution), User, Files)]
panel1_data_long_by_odor <- melt_idoc_data(panel1_data_by_odor)

panel1A_odor <- learning_plot(
  panel1_data_long_by_odor[CS=="MCH", ],
  "Dilution",
  direction = "horizontal",
  y_annotation = 1,
  colors = colors_panel1[1:length(unique(panel1_data_long_by_odor$CS))],
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, 0.5),
  text_vjust = 1.5,
  angle_n = 0,
  offset=0.25
)
panel1A_odor



panel1A_2u_oct <- learning_plot(
  panel1_data_long[Genotype=="2u" & CS == "OCT" ,],
  "Group",
  direction = "horizontal",
  y_annotation = 1,
  # colors = colors_panel1[1:length(groups_in_figure_1)],
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, 0.5),
  text_vjust = 1.5,
  angle_n = 0,
  offset=0.25
)

panel1A_2u_mch <- learning_plot(
  panel1_data_long[Genotype=="2u" & CS == "MCH" ,],
  "Group",
  direction = "horizontal",
  y_annotation = 1,
  # colors = colors_panel1[1:length(groups_in_figure_1)],
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, 0.5),
  text_vjust = 1.5,
  angle_n = 0,
  offset=0.25
)
