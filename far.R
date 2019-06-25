#####################
#RUN THE CODE BELOW #
#####################

library(zoo)
library(feather)
library(tidyverse)
library(lubridate)
require(data.table)

financial_path <- paste0(getwd(),"/data_far/data/")
prices_path    <- paste0(getwd(),"/data_far/prices/")
sector_path    <- paste0(getwd(),"/data_far/sector/")

avaiable.names <- function() {
  
  financial_files <- dir("data_far/data",pattern=".feather") %>% data.frame(.)
  price_files     <- dir("data_far/prices",pattern=".feather") %>% data.frame(.)
  merged <- merge(financial_files,price_files)
  colnames(merged) <- "files"
  merged$files <- as.character(merged$files)
  
  return(merged)
}
extend.name <- function(financial,price,to_date = "2019-01-01"){
  # financial$date <- as_date(financial$date)
  # price$date <- as_date(price$date)
  # 
  # from_date <- financial$date[1]
  # price$name <- NULL
  # 
  # dates <- data.frame(rep(seq(as.Date(from_date), as.Date(to_date), by = 'days')))
  # colnames(dates) <- "date"
  #
  # extended_df <- merge(x = dates, y = financial, by = "date", all.x = TRUE) %>%
  #                ds.addSector(.)    %>%
  #                ds.addFinRatios(.) %>% 
  #                merge(x = . , y = price , by = 'date', all.x = TRUE) %>% 
  #                na.locf.default(.) %>%
  #                ds.addBenchmarck(.) %>%
  #                ds.addPriceRatios(.) %>%
  #                ds.addPerformances(.,365) 
  # 
  # 
  #                 
  # extended_df$name_x <- NULL
  # extended_df$name_y <- NULL
  
  from_date <- financial$date[1]
  dates <- data.frame(rep(seq(as.Date(from_date), as.Date(to_date), by = 'days')))
  colnames(dates) <- "date"
  
  financial   <-  fixFinancial(financial) 
  price       <-  fixPrice(price) 
  
  extended_df <-  financialExtension(financial) %>%
    merge(x = dates, y = ., by = 'date', all.x = TRUE) %>%
    merge(x = . , y = price , by = 'date', all.x = TRUE) %>%
    na.locf.default(.) %>%
    priceExtension(.) 
  
  
  return(extended_df)
}
fixFinancial <- function(df){
  
  df$date <- as_date(df$date)
  
  return(df)
}
fixPrice <- function(df){
  
  df$date <- as_date(df$date)
  df$name <- NULL
  
  return(df)
}
financialExtension <- function(df.in){
  
  df.out <- ds.addSector(df.in)    %>%
    ds.addFinRatios(.)
  
  return(df.out)
}
priceExtension <- function(df.in){
  
  df.out <- ds.addBenchmarck(df.in) %>%
    ds.addPriceRatios(.) %>%
    ds.addPerformances(.,365)
  
  return(df.out)
}
ds.generator <- function(i_max=9999,names="null"){
  
  if (names != "null") {list.names <- names}
  else{list.names <- avaiable.names() %>% .$files}
  for (i in 1:length(list.names)){
    if (i > i_max){break}
    if (i == 1){
      financial <- read_feather(paste0(financial_path,list.names[i])) %>% mutate(date = ymd(date) + 60)
      price <- read_feather(paste0(prices_path,list.names[i]))
      df <- extend.name(financial,price)
    }
    else{
      financial <- read_feather(paste0(financial_path,list.names[i])) %>% mutate(date = ymd(date) + 60)
      price <- read_feather(paste0(prices_path,list.names[i]))
      df_tmp <- extend.name(financial,price)
      df <- rbind(df,df_tmp) 
    }
  }
  df$date <- as_date(df$date)
  spy <- read_feather(paste0(prices_path,"SPY.feather"))
  spy$date <- as_date(spy$date)
  keeps <- c("date", "Adj Close")
  spy <- spy[keeps]
  colnames(spy) <- c("date", "SPY")
  df_tmp <- merge(x = spy, y = df, by = "date", all.y = TRUE) %>% na.locf.default(.)
  return(df)
}

#####################################################################################
#####################################################################################


ds.addBenchmarck <- function(df){
  
  df$date <- as_date(df$date)
  spy <- read_feather(paste0(prices_path,"SPY.feather"))
  spy$date <- as_date(spy$date)
  
  keeps <- c("date", "Adj Close")
  spy <- spy[keeps]
  colnames(spy) <- c("date", "SPY")
  
  df_tmp <- merge(x = spy, y = df, by = "date", all.y = TRUE) %>% na.locf.default(.)
  df_tmp$date <- as_date(df_tmp$date)
  return(df_tmp)
}
ds.addSector <- function(data){
  
  sectors <- read_feather(paste0(sector_path,"tickers_industry.feather"))
  colnames(sectors) <- c("name","full name", "Sector","Industry")
  df_tmp <- merge(x=sectors, y=data, by = "name", all.y = TRUE)
  
  return(df_tmp)
}
ds.addFinRatios<-function(data){
  
  
  #Total debt to Total current assets
  debt_to_assets<-as.numeric(data$`Total debt`)/as.numeric(data$`Total current assets`)
  
  #Current ratio (liquitity ratio)
  current_ratio<-as.numeric(data$`Total current assets`)/as.numeric(data$`Total current liabilities`)
  
  #Qucik ratio ()
  quick_ratio<-as.numeric(data$`Cash and cash equivalents`)/as.numeric(data$`Total current liabilities`)
  
  #Debt to equity ratio
  debt_to_equity<-as.numeric(data$`Total liabilities`)/as.numeric(data$`Shareholders Equity`)
  
  #Dividend payout
  dividend<-as.numeric(data$`Dividend per Share`)/as.numeric(data$EPS)
  
  #Return on equity
  roe<-as.numeric(data$`Net Income`)/as.numeric(data$`Shareholders Equity`)
  
  #Return on assets
  roa<-as.numeric(data$`Net Income`)/as.numeric(data$`Total assets`)
  
  #Interest coverage
  interest_coverage<-as.numeric(data$EBIT)/as.numeric(data$`Interest Expense`)
  
  #Equity multiplier
  equity_multiplier<-as.numeric(data$`Total assets`)/as.numeric(data$`Shareholders Equity`)
  
  new<-as.data.table(cbind(data, debt_to_assets, current_ratio,quick_ratio, debt_to_equity,
                           dividend, roe, roa, interest_coverage, equity_multiplier ))
  return(new)
  
}
ds.addPriceRatios<-function(data){
  
  #PE
  PE<-as.numeric(data$'Adj Close')/as.numeric(data$EPS)
  
  #Price to Free Cash Flow
  price_to_free_cash_flow<-as.numeric(data$'Adj Close')/as.numeric(data$`Free Cash Flow`)
  
  new<-as.data.table(cbind(data, PE, price_to_free_cash_flow))
  return(new)
  
}
ds.addPerformances <- function(df,time_step){
  
  df$'Adj Close' <- as.double(df$'Adj Close')
  df$'SPY' <- as.double(df$'SPY')
  
  price_gain <- (lead(df$'Adj Close', time_step) - df$'Adj Close')/df$'Adj Close'
  SPY_gain   <- (lead(df$'SPY', time_step) - df$'SPY')/df$'SPY'
  
  df$target     <- ifelse(price_gain > SPY_gain, 1, 0)
  
  return(df)
}
fixFormat    <- function(df){
  
  
  df$name_x <-  NULL
  df$name_y <- NULL
  df$Sector <- as.factor(df$Sector)
  df$Industry <- as.factor(df$Industry)
  df[,7:length(df)] <- lapply(df[,7:length(df)], as.numeric)
  df$target <- as.factor(df$target)
  
  return(df)
  
  
}
#TEST DATASETS  HERE#
#####################

avaiable.names() # List all avaiable names

#############################################################################
# you can write down the names we have for testing and generate the dataset #
#############################################################################

#test_files <- c("AMZN.feather","AAPL.feather","MSFT.feather","FB.feather","DISCA.feather") 

n_sample <- 487

library(tictoc)
tic()
test_files <- sample_n(avaiable.names(), n_sample, replace = FALSE) %>% .$file

all_tickers <- avaiable.names() %>% .$files %>% str_remove_all('.feather')
test_files <- paste0(all_tickers[all_tickers %in% getTickers('sp500')], '.feather') %>% sample(n_sample)
ds         <- ds.generator(n_sample, test_files)
ds         <- fixFormat(ds)
toc()




# RF ----------------------------------------------------------------------

tickers <- test_files
split_size <- (tickers %>% length()) * 0.8

tr_tickers <- sample(tickers, split_size %>% floor()) %>% str_remove_all('.feather')


tr_te <- ds %>% 
  mutate(target = as.numeric(target) - 1) %>% 
  mutate(target = ifelse(is.na(target), 0, target)) %>% 
  rename(price = `Adj Close`) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(Sector = as.numeric(as.factor(Sector))) %>% 
  mutate(Industry = as.numeric(as.factor(Industry))) %>% 
  select(-`full name`, -`interest_coverage`, -`High`, -`Close`, -`Low`, -`Open`) %>% 
  mutate(target = as.numeric(target)) %>% 
  data.table()

train <- tr_te %>%
  filter(year(ymd(date)) < 2013) %>%
  filter(name %in% tr_tickers) %>%
  select(-date, -name) %>%
  data.table()

test <- tr_te %>%
  filter(year(date) %in% c(2014)) %>%
  filter(!(name %in% tr_tickers)) %>%
  select(-date, -name) %>%
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

# tuning_scores <- train %>% sample_n(1e4) %>% tune_xgb(target_label = 'target', eval_metric = 'auc', fast = TRUE)
# 
# m <- which.max(tuning_scores$scores)
# currentSubsampleRate <- tuning_scores[["subsample"]][[m]]
# currentColsampleRate <- tuning_scores[["colsample_bytree"]][[m]]
# lr <- tuning_scores[["lr"]][[m]]
# mtd <- tuning_scores[["mtd"]][[m]]
# mcw <- tuning_scores[["mcw"]][[m]]

ntrees <- 1e4
p <- list(objective = "binary:logistic",
          booster = "gbtree",
          eval_metric = "auc",
          nthread = 4,
          eta = 0.3,
          max_depth = 6,
          min_child_weight = 30,
          gamma = 5,
          subsample = 0.5,
          colsample_bytree = 0.5,
          colsample_bylevel = 0.632,
          alpha = 0.01,
          lambda = 0.01,
          nrounds = ntrees)


xgb_model <- xgb.train(p, test_xgb, ntrees, list(val = val_xgb), print_every_n = 10, early_stopping_rounds = 500)

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

today_test <- ds %>% 
  mutate(target = as.numeric(target) - 1) %>% 
  mutate(target = ifelse(is.na(target), 0, target)) %>% 
  rename(price = `Adj Close`) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(Sector = as.numeric(as.factor(Sector))) %>% 
  mutate(Industry = as.numeric(as.factor(Industry))) %>% 
  select(-`full name`, -`interest_coverage`, -`High`, -`Close`, -`Low`, -`Open`) %>% 
  mutate(target = as.numeric(target)) %>% 
  mutate(date = ymd(date)) %>% 
  group_by() %>% 
  filter(date == ymd('2016-01-01') | date == ymd('2016-01-02') | date == ymd('2016-01-03'))

today_xgb <- today_test %>% 
  select(-target, -date, -name) %>% 
  as.matrix() %>% 
  xgb.DMatrix() 

today_test$pred <- xgb_model %>% predict(today_xgb)

all_recommendations <- today_test %>% 
  select(name, pred) %>% 
  group_by(name) %>% 
  summarise(pred = mean(pred)) %>% 
  arrange(desc(pred)) %>% 
  top_n(40, pred)

our <- today_test %>% 
  select(name, pred) %>% 
  group_by(name) %>% 
  summarise(pred = mean(pred)) %>% 
  # filter(name %in% widemoat_2017)  %>% 
  arrange(desc(pred)) %>% 
  filter(pred > 0.5) %>% 
  top_n(10, pred)



# Performance -------------------------------------------------------------


get_hist_prices <- function(tickers, from = "2016-01-01", to = '2019-04-01' ){
  stock_prices <- new.env()
  getSymbols(tickers, env = stock_prices, src = "yahoo", from = from, to = to, auto.assign = TRUE)
  
  all_prices <- tibble()
  
  for(i in 1:length(tickers)){
    raw_df <- stock_prices[[tickers[[i]]]][,6] %>% data.frame()
    dates <- raw_df %>% row.names() %>% as.Date()
    prices <- raw_df[,1]
    
    new_df <- data.frame(price = as.numeric(prices), date = dates) %>%
      data.table()
    
    new_df$firm <- tickers[[i]]
    
    all_prices <- rbind(all_prices,new_df)
  }
  
  return(all_prices)
}


our_portfolio <- get_hist_prices(our$name) %>% 
  group_by(firm) %>% 
  mutate(price = (price - lag(price)) / price) %>% 
  group_by(date) %>% 
  summarise(r = mean(price)) %>% 
  data.table()


tickers <- widemoat_2017[!(widemoat_2017 %in% c('CBG', 'BRK.B', 'ESRX', 'MON', 'VF', 'MJN', 'JLL'))]

wm_portfolio <- get_hist_prices(tickers) %>% 
  group_by(firm) %>% 
  mutate(price = (price - lag(price)) / price) %>% 
  group_by(date) %>% 
  summarise(r = mean(price)) %>% 
  data.table()


tickers <- 'SPY'
sp_portfolio <- get_hist_prices(tickers) %>% 
  group_by(firm) %>% 
  mutate(price = (price - lag(price)) / price) %>% 
  group_by(date) %>% 
  summarise(r = mean(price)) %>% 
  data.table()
  

our_portfolio %>% 
  rename(our = r) %>% 
  cbind(
    wm_portfolio %>% 
      rename(wm = r)
  ) %>%
  cbind(
    sp_portfolio %>% 
      rename(sp = r)
  ) %>% 
  .[, -1] %>% 
  .[, -2] %>% 
  select(date, wm, our, sp) %>% 
  charts.PerformanceSummary()
  
