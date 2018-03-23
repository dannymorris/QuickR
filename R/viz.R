add_alpha <- function(color, alpha = 1) {
  if (missing(color)) stop("No color provided")
  apply(sapply(color, col2rgb)/ 255, 2,
        function(x) rgb(x[1], x[2], x[3], alpha = alpha))
}

chisq_plot <- function(x) {
  #if (!is.matrix(x)) stop("x is not a matrix")
  n <- nrow(x)
  p <- ncol(x)
  xbar <- apply(x, 2, mean)
  S <- var(x)
  S <- solve(S)
  index <- (1:n)/(n+1)
  xcent <- t(t(x) - xbar)
  di <- apply(xcent, 1, function(x,S) x %*% S %*% x,S)
  quant <- qchisq(index,p)
  plot(quant, sort(di), ylab = "Ordered distances",
       xlab = "Chi-square quantile", lwd=2,pch=1)
}

