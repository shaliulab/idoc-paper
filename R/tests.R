unpaired_t_test <- function(x, y, ...) {
  t.test(x = x, y = y, paired = FALSE, ...)
}

paired_t_test <- function(x, y, ...) {
  t.test(x = x, y = y, paired = TRUE, ...)
}
paired_wilcoxon_test <- function(x, y, ...) {
  wilcox.test(x = x, y = y, paired = TRUE, ...)
}
