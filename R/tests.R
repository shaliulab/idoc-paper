unpaired_t_test <- function(x, y, ...) {
  t.test(x = x, y = y, alternative="greater", paired = FALSE, ...)
}

paired_t_test <- function(x, y, ...) {
  t.test(x = x, y = y, alternative="greater", paired = TRUE, ...)
}

unpaired_wilcoxon_test <- function(x, y, ...) {
  wilcox.test(x = x, y = y, alternative="greater", paired = FALSE, ...)
}

paired_wilcoxon_test <- function(x, y, ...) {
  wilcox.test(x = x, y = y, alternative="greater", paired = TRUE, ...)
}
