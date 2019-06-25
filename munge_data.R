list_of_feathers <- list.files(path = 'data_far', pattern="*")

complete_data <- lapply(paste0('data_far/', list_of_feathers), read_feather) %>% 
  reduce(rbind) %>% 
  data.table()


complete_data[, yr_return := (lead(price, 250) - price) / price, by = firm]
complete_data[, yr_time := lead(date, 250) - date, by = firm]
complete_data[, yr_avg := mean(yr_return, na.rm = TRUE), by = (date)]
complete_data[, target := ifelse(yr_return > yr_avg, 1, 0)]

split_size <- (tickers %>% length()) / 2

tr_tickers <- sample(tickers, split_size %>% floor())


tr_te <- complete_data %>% 
  mutate(date = ymd(date)) %>% 
  select(-yr_time, -yr_time, -yr_avg, -yr_return) %>% 
  data.table()

train <- tr_te %>%
  filter(year(date) == 2015 | year(date) == 2016) %>%
  select(-date, -firm) %>%
  data.table()

test <- tr_te %>%
  filter(year(date) == 2017 | year(date) == 2018) %>%
  select(-date, -firm) %>%
  data.table()

train <- tr_te %>%
  filter(year(date) < 2016) %>%
  filter(firm %in% tr_tickers) %>%
  select(-date, -firm) %>%
  data.table()

test <- tr_te %>%
  filter(year(date) == 2017) %>%
  filter(!(firm %in% tr_tickers)) %>%
  select(-date, -firm) %>%
  data.table()




# ML ----------------------------------------------------------------------
source('tuning_xgb.R')
source('reduce_variables.R')
library(xgboost)

y <- train$target
tri_val <- sample(seq_along(y), length(y)*0.1)
tr <- seq_along(y)[!(seq_along(y) %in% tri_val)]
y_test <- test$target

tr_te <- bind_rows(train,test) 
train_xgb <- xgb.DMatrix(tr_te %>% select(-target) %>% .[tr,] %>%  as.matrix(), label = y[tr])
val_xgb <- xgb.DMatrix(tr_te %>% select(-target) %>% .[tri_val,] %>%  as.matrix(), label = y[tri_val])
test_xgb <- xgb.DMatrix((tr_te %>% .[-(c(tr,tri_val))] %>% select(-target) %>%  as.matrix()), label = y_test)

tuning_scores <- train %>% sample_n(1e3) %>% tune_xgb(target_label = 'target', eval_metric = 'auc', fast = TRUE)

m <- which.max(tuning_scores$scores)
currentSubsampleRate <- tuning_scores[["subsample"]][[m]]
currentColsampleRate <- tuning_scores[["colsample_bytree"]][[m]]
lr <- tuning_scores[["lr"]][[m]]
mtd <- tuning_scores[["mtd"]][[m]]
mcw <- tuning_scores[["mcw"]][[m]]

ntrees <- 1e3
p <- list(objective = "binary:logistic",
          booster = "gbtree",
          eval_metric = "auc",
          nthread = 4,
          eta = lr/ntrees,
          max_depth = mtd,
          min_child_weight = 30,
          gamma = 0,
          subsample = currentSubsampleRate,
          colsample_bytree = currentColsampleRate,
          colsample_bylevel = 0.632,
          alpha = 0,
          lambda = 0,
          nrounds = ntrees)


xgb_model <- xgb.train(p, test_xgb, 1000, list(val = val_xgb), print_every_n = 10, early_stopping_rounds = 300)

xgb.importance(model = xgb_model) %>% as_tibble() %>% top_n(25, Gain) %>%
  ggplot(aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col() +
  coord_flip() +
  xlab('') +
  ylab('')

prediction <- xgb_model %>% predict(test_xgb)
err <- mean(as.numeric(prediction > 0.5) != test$target)
print(paste("test-error=", err))




# Predict -----------------------------------------------------------------

today_test <- complete_data %>% 
  mutate(date = ymd(date)) %>% 
  select(-yr_time, -yr_time, -yr_avg, -yr_return) %>% 
  group_by() %>% 
  filter(date == max(date)) %>% 
  data.table() 

today_xgb <- today_test %>% 
  select(-target, -date, -firm) %>% 
  as.matrix() %>% 
  xgb.DMatrix() 
  
today_test$pred <- xgb_model %>% predict(today_xgb)

today_test %>% 
  select(firm, pred) %>% 
  arrange(desc(pred))

