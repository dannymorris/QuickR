
floorDate <- function(x, format) {
  if (is.character(x) == FALSE) x <- as.character(x)
  if (format == 'yyyymm') {
    as.Date(paste0(substr(x, 1, 4), '-',
                   substr(x, 5, 6), '-',
                   '01'))
  }
}
