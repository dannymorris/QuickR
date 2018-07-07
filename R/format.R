print_sign <- function(x) {
  ifelse(sign(x) == 1, paste0("+", x), x) %>%
    as.character()
}


as_percentage <- function(x, multiply = TRUE, factor = 100) {
  if (multiply == TRUE) {
    paste(x*100, "%")
  } else {
    paste(x, "%")
  }
}

convert <- function(x, what, to) {
  what_index <- x %in% what
  x[what_index] <- to
  x
}

add_leading_zeros <- function(x, output_length) {

  # empty vector to populate with transformed values
  result <- vector()

  # values to ignore
  ignored <- is.na(x)

  # pre-populate result with ignored values
  result[ignored] <- x[ignored]

  # function to paste zeros values
  paste_zeros <- function(x, output_length) {
    n_zeros <- output_length - nchar(x)
    paste0(paste0(rep('0', n_zeros), collapse = ""), x, collapse = "")
  }

  # loop through non-ignored values and add leading zero
  for (i in 1:length(x[!ignored])) {
    result[!ignored][i] <- paste_zeros(x = x[!ignored][i], output_length = output_length)
  }

  return(result)
}


add_increment_to_values <- function(x, reference = "max", adjust=20) {
  mc <- match.call()
  mc$adjust <- NULL;  mc$reference <- NULL
  mc[[1]] <- as.name(reference)
  mc[[2]] <- x
  x + max(x)/adjust
}

print_pca <- function(x, n_components = ncol(x$scores), var_explained = 1) {

  if (class(x) != "princomp")
    stop("class(x) must be princomp")

    # eigenvalues
  eigs <- x$sdev^2

  # variance explained
  explained <- eigs / sum(eigs)

  tibble::tibble(component = 1:length(eigs),
                 eigen_values = eigs,
                 explained,
                 total_explained = cumsum(explained)) %>%
    filter(component <= n_components) %>%
    filter(total_explained <= var_explained)
}

plot_pca <- function(x, n_components = 2, var_explained = 1, ...) {

  if (class(x) != "princomp")
    stop("class(x) must be princomp")

  comp_details <- print_pca(x)

  scores <- x$scores[, 1:n_components]

  comp_grid <- tibble(x = as.integer(seq(1, n_components - 1, 1)),
                      y = as.integer(seq(2, n_components, 1)))

  filter_details <- comp_details %>%
    filter(component %in% 1:n_components) %>%
    pull(explained) %>%
    round(3) %>%
    as_percentage()



  for (i in 1:nrow(comp_grid)) {
    with(comp_grid[i, ],
         plot(scores[, y] ~ scores[, x],
              xlab = "", ylab = "",
              #main = paste("X = PCA", x, ", Y = PCA", y),
              main = paste0("PCA ", x, " (x): ", filter_details[i],
                            "\nPCA ", y, " (y)"),
              cex.main = 0.9),
         title(paste0("PCA ", x, " (x): ", filter_details[i],
                      "\nPCA ", y, " (y)"), line = -1)
    )

  }

}

