source("R/tests.R", local = T)
source("R/utils.R", local = T)
source("R/constants.R", local = T)
source("R/Cbind.R", local = T)
source("R/plot.R", local = T)
source("R/themes.R", local = T)
source("R/learning_plot.R", local = T)
source("R/summary_plot.R", local = T)
source("R/prism_compat.R", local = T)
source("5_1_panel4_sleep_data.R", local = T)
source("5_2_panel4_sleep_data.R", local = T)


experiments <- c("24hr LTM")
trainings <- c("6X_Massed", "6X_Spaced")
all_levels <- c(trainings, "No_training")

genotypes <- c("Iso31")
intervals <- c("No_stimulator")
valid_reasons <- c("", "?", "Human-override", "Machine-override", "AOJ-override")
periods <- list(
  c(5, 11),
  c(12, 18)
)

data <- data.table::fread(file = "tidy_data_wide.csv")
data[, date := as.character(date)]
panel4_data <- data[
  PRE_Reason %in% valid_reasons &
    POST_Reason %in% valid_reasons &
    Training %in% trainings &
    Genotype %in% genotypes &
    interval %in% intervals &
    experiment %in% experiments &
    region_id != "NONE" &
    US %in% c("75V 0Ω", "75V")
]
panel4_data[, Training := factor(Training, levels = trainings)]

columns <- c(
  "Files", "idoc_folder", "PRE_ROI", "POST_ROI",
  "User", "Genotype", "experiment",
  "PRE", "POST", "SD_status", "interval"
)
export_csvs(panel4_data, "Training", trainings, "4A", columns)


panel4_data[, .N, by=Genotype]
panel4_data[, .N, by=.(experiment, Files)]


dt_bin <- load_ethoscope_data_fig4()
yaml::write_yaml(
  x = unique(sapply(dt_bin[, meta=TRUE]$file_info, function(fi) fi$path)),
  file = file.path(OUTPUT_FOLDER, "Fig4/fig4_ethoscope_database.yaml")
)

sleep_dataset <- process_sleep_dataset_fig4(panel4_data, periods, all_levels, dt_bin)

sleep_data <- sleep_dataset$data
significance_data <- sleep_dataset$significance
sleep_accum <- sleep_dataset$periods


panel4_data <- keep_only_with_ethoscope_data(panel4_data, sleep_accum)
panel4_data_long <- melt_idoc_data(panel4_data)


panel4A <- learning_plot(
  data = panel4_data_long[Training %in% c("6X_Spaced", "6X_Massed"), ],
  "Training",
  y_annotation = .7,
  y_annotation_n = -1,
  y_limits = c(-1, 1),
  text_vjust = +2.5,
  text_hjust = 1,
  textsize = 4,
  colors = colors_panel4[1:2],
  correction = "bonferroni"
)
panel4A$gg <- panel4A$gg + scale_x_discrete(expand = expansion(add=c(.25,.25)))

breaks <- behavr::hours(seq(4, 24, 2))
panel4C <- ggplot() +
  ggetho::stat_ld_annotations(
    data = sleep_data[t >= behavr::hours(4) & t <= behavr::hours(24), ],
    mapping = aes(x = t, y = mu, col = Training),
    height=1, alpha=0.2
  ) +
  geom_line(
    data = sleep_data[t >= behavr::hours(4) & t <= behavr::hours(24), ],
    mapping = aes(x = t, y = mu, col = Training),
    linewidth = 1
  ) +
  geom_errorbar(
   data = sleep_data[t >= behavr::hours(4) & t <= behavr::hours(24), ],
   aes(x = t, fill = Training, ymin = mu - sem, ymax = mu + sem),
   linewidth = .5, col = "black", width=behavr:::hours(0.25)
 ) +
 geom_point(
   data = sleep_data[t >= behavr::hours(4) & t <= behavr::hours(24), ],
   aes(x = t, y = mu, fill = Training, col = Training),
   size=2
 ) +
 scale_x_hours(
   name = "ZT", breaks = breaks,
   labels = (breaks %% behavr::hours(24)) / behavr::hours(1)
 ) +
 scale_color_manual(values = colors_panel4[1:length(all_levels)]) +
 scale_y_continuous(name = "Time asleep per\n30 min bin (min)", expand = expansion(add = c(0, 0))) +
 geom_signif(
   data = significance_data[Training == "No_training", ],
   mapping = aes(
     xmin = t - behavr::mins(10),
     xmax = t + behavr::mins(10),
     group = t,
     annotations = symbol_ml
   ), hjust = 0,
   segment_color = "white",
   tip_length = 0, y_position = 25, manual = TRUE, textsize = STARSIZE, lineheight=.35
 ) +
 coord_cartesian(clip = "off", ylim = c(0, 30), xlim = behavr::hours(c(4, 24))) +
 sleep_plot_theme
panel4C

panel4C <- list(gg = panel4C)


sleep_accum[, Training := factor(as.character(Training), levels = all_levels)]
export_csvs(sleep_accum, "Training", all_levels, "4C", NULL, "asleep")



preprocess_function <- function(data, group, test, value.var = "PI") {
  data$group__ <- data[[group]]
  data[[value.var]] <- data$asleep
  return(data)
}

sleep_spaced <- sleep_accum[interval=="ZT05-ZT11" & Training == "6X_Spaced", .(Files, PRE_ROI = as.integer(PRE_ROI), POST_ROI = as.integer(POST_ROI), asleep)]
learning_spaced <- panel4_data[Training == "6X_Spaced", .(Files, PRE_ROI = as.integer(PRE_ROI), POST_ROI = as.integer(POST_ROI), PRE, POST, POST_1, POST_2)]

spaced_data <- merge(sleep_spaced, learning_spaced, by = c("Files", "PRE_ROI", "POST_ROI"))
ggplot(data=spaced_data, mapping = aes(x=asleep, y = POST)) + geom_point() + geom_smooth(method="lm") + labs(y="POST PI")


y_max <- 360
panel4D_all <- lapply(periods, function(period) {
  period_str <- paste0(
    "ZT",
    stringr::str_pad(string = period[1], pad = "0", width = 2),
    "-ZT",
    stringr::str_pad(string = period[2], pad = "0", width = 2)
  )
  
  df <- sleep_accum[interval == period_str & Training %in% all_levels, ]
  panel <- summary_plot(
    data = df,
    group = "Training",
    comparisons = list(
      c("6X_Spaced", "6X_Massed"),
      c("6X_Spaced", "No_training"),
      c("No_training", "6X_Massed")
    ),
    map_signif_level = T,
    annotation_y = c(375, 355, 415),
    colors = colors_panel4[1:length(all_levels)],
    y_limits = c(0, y_max),
    percentile = c(0.025, 0.975),
    preprocess_function = preprocess_function,
    y_axis_label = paste0(period_str, " sleep (min)"),
    y_breaks = seq(0, y_max, 60),
    geom = "violin+sina",
    text_hjust = 1,
    text_vjust = 2.5,
    textsize = 4,
    angle_n = 45,
    correction = "bonferroni"
  )

  panel$gg <- panel$gg +
    scale_color_manual(
      values = colors_panel4[1:length(all_levels)],
      labels = all_levels
    ) +
    guides(color = guide_legend(override.aes = list(
      linetype = 1, # Set line type to solid
      shape = 15, # Remove points from the legend
      size = 10
    ))) + guides(fill = "none")
  panel
})

layout <- "
AAAAAAAABBBBBBBBBB
#FFFFFFFFFFFFFFFFF
CCCCCCDDDDDDEEEEEE
"

bottom_height <- 2.5
middle_height <- 2.2


gg <- ggplot() +
  learning_plot_theme +
  ggplot() +
  learning_plot_theme +
  panel4A$gg +
  guides(color = "none", fill = "none") + theme(
    plot.margin = unit(c(0, 0, 30, 0), "pt"),
    axis.title.y = element_blank()
  ) +
  panel4D_all[[1]]$gg +
  guides(color = "none", fill = "none") + theme(plot.margin = unit(c(0, 0, 30, 0), "pt")) +
  panel4D_all[[2]]$gg +
  guides(color = "none", fill = "none") + theme(plot.margin = unit(c(0, 0, 30, 0), "pt")) +
  (
    panel4C$gg +
    guides(color = "none", fill = "none") +
    theme(
      plot.margin = unit(c(0, 0, 60, 0), "pt"),
      axis.title.y = element_blank()
    )
  ) +
  plot_annotation(tag_levels = list(c("A", "B", "C", "D", "E", "F"))) +
  plot_layout(design = layout, heights = c(3, middle_height, bottom_height)) &
  theme(
    legend.position = "bottom"
  )

gg
ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig4/Figure_4.pdf"), width = 190, height = 280, unit = "mm", dpi = "retina")
ggsave(plot = gg, filename = paste0(OUTPUT_FOLDER, "/Fig4/Figure_4.svg"), width = 190, height = 280, unit = "mm", dpi = "retina")
