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
