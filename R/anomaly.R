
ALSO <- function(data, method, cv=FALSE, folds=NULL) {

  require(caret)

  # make data a data.frame
  data_df <- if (!('data.frame' %in% class(data))) {
    data.frame(data)
  } else data

  # characters to factors
  character_vars <- lapply(data_df, class) == "character"
  iris[, character_vars] <- lapply(iris[character_vars], as.factor)

  # prepare loop over all q variables
  n_cols <- ncol(data)

  # N x q error matrix
  error_matrix <- matrix(ncol = ncol(data),
                         nrow = nrow(data))

  # 1 x q rmse matrix
  rmse_mat <- matrix(ncol=ncol(data), nrow=1)

  # create variable-wise k-fold cv prediction error matrix
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

    # if no CV
    if (cv==FALSE) {
      # prepare model df
      X <- data[, -i]
      Y <- data[, i]
      model_df <- data.frame(X,Y)
      # prepare formula
      f <- as.formula(paste("Y~", paste(colnames(X), collapse="+")))
      # call method with formula and data args
      fit <- do.call(method, list(formula=f, data=model_df))
      # get prediction errors
      if (class(Y) == 'factor') {
        sq_error <- (as.numeric(fit$predicted) - as.numeric(Y))^2
      } else {
        sq_error <- (predict(fit) - Y)^2
      }
      # population error matrix and rmse matrix
      error_matrix[,i] <- sq_error
      rmse_mat[,i] <- mean(sq_error)

    } else {

      # CV
      # each observation gets out of sample prediction
      for (j in 1:length(k_folds)) {
        # prepare train and test splits
        X_train <- data[-k_folds[[j]], -i]
        Y_train <- data[-k_folds[[j]], i]
        X_test <- data[k_folds[[j]], -i]
        Y_test <- data[k_folds[[j]], i]
        # prepare model df
        model_df <- data.frame(X_train, Y_train)
        # prepare formula
        f <- as.formula(paste("Y_train~", paste(colnames(X_train), collapse="+")))
        # call method with formula and data arguments
        fit <- do.call(method, list(formula=f, data=model_df))
        # get out of sample predictions
        oos_predict <- predict(fit, newdata = X_test)
        if (class(Y_train) == 'factor') {
          oos_sq_error <- (as.numeric(oos_predict) - as.numeric(Y_test))^2
        } else {
          oos_sq_error <- (oos_predict - Y_test)^2
        }
        # populate cv prediction error
        cv_error_mat[k_folds[[j]],] <- oos_sq_error # get errors for jth variable

      }
      # population error matrix and rmse matrix
      error_matrix[,i] <- cv_error_mat
      rmse_mat[,i] <- mean(cv_error_mat)
    }
  }
  # adjusted rmse if > 1 then 1 else rmse
  bounded_rmse <- ifelse(rmse_mat > 1, 1, rmse_mat)
  # compute feature weights as 1-adjusted rmse
  feature_weights <- 1-bounded_rmse
  # compute outlier scores
  scores <- apply(error_matrix, 1, function(x) {
    sum(x*feature_weights, na.rm=T)
  })
  # output
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

