TEXT_SIZE <- 12 # 20 becomes 20
TITLE_SIZE <- 15
FONT <- "sans"
STARSIZE <- 5
N_TEXT_SIZE <- 4 # 5 becomes 14.23
PLOT_TAG_SIZE <- 22
LEGEND_TEXT_SIZE <- 15

phi <- sqrt(5) / 2 + 0.5

zt05_11 <- paste0("ZT", seq(5, 10.5, 0.5))
zt12_18 <- paste0("ZT", seq(12, 17.5, 0.5))
zt05_24 <- paste0("ZT", seq(5, 23.5, 0.5))
zt_all <- paste0("ZT", seq(4, 30, 0.5))
zts <- list(
  zt05_11 = zt05_11,
  zt12_18 = zt12_18,
  zt05_24 = zt05_24,
  zt_all = zt_all
)

NS_6x_spaced <- "magenta"
NS_6x_massed <- "#A5402D"
no_training <- "black"
paired <- "blue"
unpaired <- "red"
stm_20min <- "blue"
stm_1hr <- "#90be6d"
stm_3hr <- "#277da1"
NS_6X_spaced_cxm <- "#f53542"
orb2_6x_spaced <- "#2667ff"
orb2_20min <- "#add7f6"
zt05_11_sd <- "#61a119"

colors_panel1 <- c(paired, unpaired)
colors_panel2 <- c(stm_20min, stm_1hr, stm_3hr)
colors_panel3 <- c(NS_6x_spaced, NS_6X_spaced_cxm, orb2_6x_spaced, orb2_20min)
colors_panel4 <- c(NS_6x_massed, NS_6x_spaced, no_training)
colors_panel5 <- c(NS_6x_spaced, zt05_11_sd)



expansion_x_left <- 0.1
expansion_x_right <- 0.1
EXPANSION_Y_BOTTOM <- 0
EXPANSION_Y_TOP <- 0
LINEWIDTH <- .8

width <- dev.size("cm")[1] * 10

POINT_SIZE <- width*0.01
ERRORBAR_WIDTH <- .1
POINT_SIZE_MEAN <- width*0.02
print(POINT_SIZE)
print(POINT_SIZE_MEAN)


# POINT_SIZE <- 0.03
# ERRORBAR_WIDTH <- POINT_SIZE * 4
# POINT_SIZE_MEAN <- 0.05
LINEWIDTH_MEAN <- 1.2
VJUST <- 0
SUMMARY_PLOT_POINT_SIZE <- 2

OUTPUT_FOLDER <- "output/2024-10-26"
dir.create(OUTPUT_FOLDER, recursive = TRUE)
dir.create(file.path(OUTPUT_FOLDER, "Fig1"))
dir.create(file.path(OUTPUT_FOLDER, "Fig2"))
dir.create(file.path(OUTPUT_FOLDER, "Fig3"))
dir.create(file.path(OUTPUT_FOLDER, "Fig4"))
dir.create(file.path(OUTPUT_FOLDER, "Fig5"))


