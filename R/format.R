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

add_leading_zeros <- function(x, length_out) {
  if (nchar(x) < length_out) {
    x_chars <- nchar(x)
    zeros <- length_out - x_chars
    paste0(paste0(rep('0', zeros), collapse = ""), x, collapse = "")
  } else x
}

convert_symbol_to_na <- function(x, symbol) {
  ifelse(x %in% symbol, NA, x)
}

add_plus_minus_symbol <- function(x) {
  ifelse(x > 0, paste0('+', x), x)
}

convert_prop_to_percent <- function(x, digits, add_symbol = F) {
  library(scales)
  pct <- percent(round(x, digits = digits))

  if (add_symbol == F) {
    pct
  } else {
    add_plus_minus_symbol(pct)
  }
}

add_increment_to_values <- function(x, reference = "max", adjust=20) {
  mc <- match.call()
  mc$adjust <- NULL;  mc$reference <- NULL
  mc[[1]] <- as.name(reference)
  mc[[2]] <- x
  x + max(x)/adjust
}
