source("R/tests.R", local = T)
source("R/utils.R", local = T)
source("R/constants.R", local = T)
source("R/Cbind.R", local = T)
source("R/plot.R", local = T)
source("R/themes.R", local = T)
source("R/learning_plot.R", local = T)
source("R/summary_plot.R", local = T)

data <- data.table::fread(file = "tidy_data_wide.csv")
valid_reasons <- c(
  "", "?",
  # "Human-override",
  # "Pre-aversion", "Pre-attraction",
  "Machine-override", "AOJ-override")

genotypes <- "2u"
panel1_data <- data[
  PRE_Reason %in% valid_reasons &
    POST_Reason %in% valid_reasons &
    Genotype %in% genotypes &
    experiment %in% c("Learning", "Learning all ITO", "20min STM"),
]
panel1_data[, .(n=.N), by=Files]
panel1_data$Group <- factor("2u", levels=c("2u"))
panel1_data[, .(Files, id, POST, PRE_ROI, Group, experiment)]
panel1_data_long <- melt_idoc_data(panel1_data)

panel1_data[, .(n=.N, POST=mean(POST)), by=.(CS, Files)]

# data.table::fwrite(
#   x = panel1_data[, .(Files, PRE_ROI, POST_ROI, PRE, POST, Genotype, experiment)],
#   file = "2u_metadata.csv"
# )
panel1A <- learning_plot(
  panel1_data_long[, .(id, PI, test, Group)],
  "Group",
  direction = "horizontal",
  y_annotation = 1,
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, 0.5),
  text_vjust = 1.5,
  angle_n = 0,
  map_signif_level = F,
  textsize=5
)
panel1A$gg + theme(plot.margin = unit(c(40, 0, 20, 0), "pt"))

