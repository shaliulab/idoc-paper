unpaired_t_test <- function(x, y, ...) {
  t.test(x = x, y = y, alt = "two.sided", paired = FALSE, ...)
}

paired_t_test <- function(x, y, ...) {
  t.test(x = x, y = y, alt = "two.sided", paired = TRUE, ...)
}
paired_wilcoxon_test <- function(x, y, ...) {
  wilcox.test(x = x, y = y, alt = "two.sided", paired = TRUE, ...)
}
