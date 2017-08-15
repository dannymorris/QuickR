naToZero <- function(x) {
  ifelse(is.na(x), 0, x)
}

lglToZero <- function(x) {
  ifelse(x == TRUE, 1, 0)
}

fillBlank <- function(x) {
  if (is.character(x)) {
    ifelse(nchar(x) == 0, NA, x)
  }
}
