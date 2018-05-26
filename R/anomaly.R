ALSO <- function(input_matrix, returns = 'scores', scale = TRUE,
                              error_agg_function = 'sum') {

  input_matrix <- as.matrix(input_matrix)
  input_df <- data.frame(input_matrix)
  # prepare loop - N iterations
  n_cols <- ncol(input_matrix)
  # zero matrix - fill cols with errors[,i]
  error_matrix <- matrix(ncol = ncol(input_matrix),
                         nrow = nrow(input_matrix))

  rmse_mat <- matrix(0, ncol=ncol(input_matrix), nrow=1)

  k_folds <- createFolds(1:nrow(input_df), k=10)
  cv_error_mat <- matrix(ncol=1, nrow=nrow(input_df))

  # loop through each col and predict from the rest
  for (i in 1:n_cols) {
    #linreg <- lm(input_matrix[,i] ~ ., data = data.frame(input_matrix))
    # error_matrix[,i] <- linreg$residuals

    # each observation gets one out of sample prediction
    for (j in 1:length(k_folds)) {
      X <- as.matrix(input_df[-k_folds[[j]], -i])
      Y <- as.matrix(input_df[-k_folds[[j]], i])
      model_df <- data.frame(X,Y)
      linreg <- lm(Y ~ ., data=model_df)
      oos_predict <- predict(linreg, newdata = input_df[k_folds[[j]], -i])
      oos_error <- (oos_predict - input_df[k_folds[[j]], i])^2
      cv_error_mat[k_folds[[j]],] <- oos_error # get errors for jth variable
    }
    error_matrix[,i] <- cv_error_mat
    rmse_mat[,i] <- mean(cv_error_mat)
  }

  adj_rmse <- ifelse(rmse_mat > 1, 1, rmse_mat)
  weights <- 1-adj_rmse

  scores <- apply(error_matrix, 1, function(x) {
    sum(x*weights)
  })

  list(scores=scores,
       error_matrix=error_matrix,
       rmse_mat=rmse_mat,
       weights=weights)

#   if (returns == 'scores') {
#     if (scale == TRUE) {
#       scaled_aggregated_errors <- as.numeric(scale(aggregated_errors))
#       return(scaled_aggregated_errors)
#     } else {
#       return(aggregated_errors)
#     }
#   }
#
#   if (return == "error_matrix") {
#     return(error_matrix)
#   }
}

nearest_neighbors <- function(data, d=NULL, ids, k) {

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


