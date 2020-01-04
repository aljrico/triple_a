train_model <- function(train, test) {
  source("tuning_xgb.R")
  source("reduce_variables.R")
  library(xgboost)

  y <- train$target
  tri_val <- sample(seq_along(y), length(y) * 0.1)
  tr <- seq_along(y)[!(seq_along(y) %in% tri_val)]
  y_test <- test$target

  tr_te <- bind_rows(train, test)
  train_xgb <- xgb.DMatrix(tr_te %>% select(-target) %>% .[tr, ] %>% as.matrix(), label = y[tr])
  val_xgb <- xgb.DMatrix(tr_te %>% select(-target) %>% .[tri_val, ] %>% as.matrix(), label = y[tri_val])
  test_xgb <- xgb.DMatrix((tr_te %>% .[-(c(tr, tri_val))] %>% select(-target) %>% as.matrix()), label = y_test)

  ntrees <- 1e3
  p <- list(
    objective = "binary:logistic",
    booster = "gbtree",
    eval_metric = "auc",
    nthread = 4,
    eta = 0.03,
    max_depth = 6,
    min_child_weight = 30,
    gamma = 0,
    subsample = 0.5,
    colsample_bytree = 0.5,
    colsample_bylevel = 0.632,
    alpha = 0.01,
    lambda = 0.01,
    nrounds = 1e3
  )


  xgb_model <- xgb.train(p, train_xgb, ntrees, list(val = test_xgb), print_every_n = 10, early_stopping_rounds = 500)
  return(xgb_model)
}
