cagr <- function(x) {
  (last(x) / first(x))^(1/(length(x) - 1)) - 1
}
