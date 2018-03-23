pct_round <- function(x, digits) {
  percent(round(x, digits = digits))
}

csv_sql <- function(x) {
  paste0("'", x, "'", collapse = ",")
}

na_to_zero <- function(x) {
  if (is.numeric(x)) {
    ifelse(is.na(x), 0, x)
  } else ifelse(is.na(x), '0', x)

}

fill_blank <- function(x, pattern) {
  if (!is.character(x)) {
    stop('column(s) are not of class character')
  } else {
    ifelse(x %in% c('', ' '), pattern, x)
  }
}
