cagr <- function(x) {
  (last(x) / first(x))^(1/(length(x) - 1)) - 1
}

pctChange <- function(new, old) {
  if (old == 0) {
    stop("Cannot divide by zero")
  } else {
    (new - old)/old
  }
}
