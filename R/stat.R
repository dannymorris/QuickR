####################################################
## Unsupervised Feature Predicted Outlier Scores ##
####################################################

LPF_outlier_score <- function(input_matrix, returns = 'scores', scale = TRUE,
                              error_agg_function = 'sum') {
  #input_matrix <- iris[,-5]
  # prepare loop - N iterations
  n_cols <- ncol(input_matrix)
  # zero matrix - fill cols with errors[,i]
  error_matrix <- matrix(0, ncol = ncol(input_matrix),
                         nrow = nrow(input_matrix))
  # loop through each col and predict from the rest
  for (i in 1:n_cols) {
    linreg <- lm(input_matrix[,i] ~ ., data = input_matrix)
    error_matrix[,i] <- linreg$residuals
  }
  # aggregate residuals
  aggregated_errors <- apply(error_matrix^2, 1, error_agg_function)

  if (returns == 'scores') {
    if (scale == TRUE) {
      scaled_aggregated_errors <- as.numeric(scale(aggregated_errors))
      return(scaled_aggregated_errors)
    } else {
      return(aggregated_errors)
    }
  }

  if (return == "error_matrix") {
    return(error_matrix)
  }
}

exact_knn <- function(data, d=NULL, ids, k) {

  require(dplyr)

  if (missing(ids))
    stop('"ids" is missing. provide a string of row labels')

  if (missing(k)) {
    k <- 1
    warning('"k" is missing. defaults to 1')
  }

  if (!is.null(d)) {
    data <- NULL
    dmat <- as.matrix(d)
  }

  if (!is.null(data)) {
    if (!all(apply(data, 2, class) == 'numeric'))
      stop("all columns of the data set must be numeric")

    d <- dist(data)
    dmat <- as.matrix(d)
  }

  dimnames(dmat) <- list(ids, ids)
  pairs_key <- t(combn(colnames(dmat), 2))
  pairwise_d <- data.frame(col=colnames(dmat)[col(dmat)],
                           row=rownames(dmat)[row(dmat)],
                           distance=c(dmat))
  pairwise_d %>%
    tbl_df() %>%
    filter(col != row) %>%
    mutate_at(vars(1,2), as.character) %>%
    group_by(col) %>%
    arrange(col, distance) %>%
    mutate(knn = row_number()) %>%
    filter(knn %in% k)
}
