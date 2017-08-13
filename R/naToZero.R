naToZero <- function(x) {
  ifelse(is.na(x), 0, x)
}

lglToZero <- function(x) {
  ifelse(x == TRUE, 1, 0)
}
