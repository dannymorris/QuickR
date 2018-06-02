add_alpha <- function(color, alpha = 1) {
  if (missing(color)) stop("No color provided")
  apply(sapply(color, col2rgb)/ 255, 2,
        function(x) rgb(x[1], x[2], x[3], alpha = alpha))
}


