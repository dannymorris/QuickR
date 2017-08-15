naToZero <- function(x) {
  ifelse(is.na(x), 0, x)
}

lglToZero <- function(x) {
  ifelse(x == TRUE, 1, 0)
}

fillBlank <- function(x, pattern) {
  if (!is.character(x)) {
    stop('column(s) are not of class character')
  } else {
    ifelse(x %in% c('', ' '), NA, x)
  }
}
