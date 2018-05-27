
ALSO <- function(data, method='rf', cv=FALSE, folds=NULL) {

  require(randomForest)
  require(dplyr)
  require(caret)

  data <- data %>%
    mutate_if(is.character, as.factor) %>%
    data.frame()

  # prepare loop - N iterations
  n_cols <- ncol(data)
  # zero matrix - fill cols with errors[,i]
  error_matrix <- matrix(ncol = ncol(data),
                         nrow = nrow(data))

  rmse_mat <- matrix(0, ncol=ncol(data), nrow=1)

  if (method == 'lm' & cv == FALSE) {
    message("cross validation set to TRUE for method='lm'")
  }
  cv <- ifelse(method == 'rf', cv, TRUE)

  if (cv==TRUE) {
    cv_error_mat <- matrix(ncol=1, nrow=nrow(data))
    if(is.null(folds)) {
      message("k_folds not supplided. default to 5")
      k_folds <- createFolds(1:nrow(data), k=5)
    } else {
      k_folds <- createFolds(1:nrow(data), k=folds)
    }
  }

  # loop through each col and predict from the rest
  for (i in 1:n_cols) {

    # no CV (random forest only)
    if (cv==FALSE) {
      X <- as.matrix(data[, -i])
      Y <- data[, i]
      fit <- randomForest(x=X, y=Y)
      if (class(Y) == 'factor') {
        sq_error <- (as.numeric(fit$predicted) - as.numeric(Y))^2
      } else {
        sq_error <- (fit$predicted - Y)^2
      }
      error_matrix[,i] <- sq_error
      rmse_mat[,i] <- mean(sq_error)

    } else {

      # CV
      # each observation gets one out of sample prediction
      for (j in 1:length(k_folds)) {

        X_train <- as.matrix(data[-k_folds[[j]], -i])
        Y_train <- data[-k_folds[[j]], i]
        X_test <- as.matrix(data[k_folds[[j]], -i])
        Y_test <- data[k_folds[[j]], i]

        # random forest
        if (method == 'rf') {
          fit <- randomForest(x=X_train, y=Y_train)
          oos_predict <- predict(fit, newdata = X_test)
          if (class(Y) == 'factor') {
            oos_sq_error <- (as.numeric(oos_predict) - as.numeric(Y_test))^2
          } else {
            oos_sq_error <- (oos_predict - Y_test)^2
          }
        }

        # linear regression
        if (method == 'lm') {
          model_df <- data.frame(X_train, Y_train)
          fit <- lm(Y_train ~ ., data=model_df)
          oos_predict <- predict(fit, newdata = data.frame(X_test))
          oos_sq_error <- (oos_predict - Y_test)^2
        }

        cv_error_mat[k_folds[[j]],] <- oos_sq_error # get errors for jth variable
      }
      error_matrix[,i] <- cv_error_mat
      rmse_mat[,i] <- mean(cv_error_mat)
    }
  }

  bounded_rmse <- ifelse(rmse_mat > 1, 1, rmse_mat)
  feature_weights <- 1-bounded_rmse

  scores <- apply(error_matrix, 1, function(x) {
    sum(x*feature_weights)
  })

  list(scores=scores,
       error_matrix=error_matrix,
       rmse_mat=rmse_mat,
       weights=feature_weights)
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

df <- matrix(rnorm(1000), ncol=10) %>%
  tbl_df()


