pctChange <- function(new, old) {
  if (old == 0) {
    stop("Cannot divide by zero")
  } else {
    (new - old)/old
  }
}
