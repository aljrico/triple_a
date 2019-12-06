#####################
#RUN THE CODE BELOW #
#####################

library(zoo)
library(feather)
library(tidyverse)
require(data.table)
library(quantmod)
library(rvest)
library(progress)
library(lubridate)
library(BatchGetSymbols)
library(feather)

source("get_financials.R")
source("get_tickers.R")


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

build_trte <- function(full_data, split_size = 0.7, year_to_predict){
  
  tickers <- full_data$name %>% unique()
  
  tr_tickers <- sample(tickers, floor(length(tickers)*split_size))
  
  
  tr_te <- full_data %>% 
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
    filter(year(ymd(date)) < year_to_predict - 1) %>%
    filter(name %in% tr_tickers) %>%
    select(-date, -name) %>%
    data.table()
  
  test <- tr_te %>%
    filter(year(date) %in% c(year_to_predict - 1)) %>%
    filter(!(name %in% tr_tickers)) %>%
    select(-date, -name) %>%
    data.table()
  
  return(list(train = train, test = test))
}
train_model <- function(train, test){
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
  
  ntrees <- 1e3
  p <- list(objective = "binary:logistic",
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
            nrounds = 1e3)
  
  
  xgb_model <- xgb.train(p, train_xgb, ntrees, list(val = test_xgb), print_every_n = 10, early_stopping_rounds = 500)
  return(xgb_model)
}
predict_portfolio <- function(model, full_data, year_to_predict, n_firms = 20){
  
  days_to_predict <- paste0(year_to_predict, '-01-', c('01', '02', '03'))
  
  today_test <- full_data %>% 
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
    filter(date %in% ymd(days_to_predict))
  
  today_xgb <- today_test %>% 
    select(-target, -date, -name) %>% 
    as.matrix() %>% 
    xgb.DMatrix() 
  
  today_test$pred <- model %>% predict(today_xgb)
  
  all_recommendations <- today_test %>% 
    select(name, pred) %>% 
    group_by(name) %>% 
    summarise(pred = mean(pred)) %>% 
    arrange(desc(pred)) %>% 
    top_n(n_firms, pred)
  
  return(all_recommendations)
}

get_hist_prices <- function(tickers, from = "2015-01-01", to){
  stock_prices <- list()
  # getSymbols(tickers, env = stock_prices, src = "yahoo", from = from, to = to, auto.assign = TRUE)
  
  all_prices <- tibble()
  
  for(k in 1:length(tickers)){
    tryit <- try(
      getSymbols(tickers[[k]], src = "yahoo", from = "1950-01-01", auto.assign = FALSE)
    )
    
    if(inherits(tryit, "try-error")){
      cat(paste0('... ', tickers[[k]], ' : Not found. Skipping ... \n'))
      next
    }else {
      stock_prices[[tickers[[k]]]] <- getSymbols(tickers[[k]], src = "yahoo", from = from, to = to, auto.assign = FALSE)
      raw_df <- stock_prices[[tickers[[k]]]][,6] %>% data.frame()
      dates <- raw_df %>% row.names() %>% as.Date()
      prices <- raw_df[,1]
      
      new_df <- data.frame(price = as.numeric(prices), date = dates) %>%
        data.table()
      
      new_df$firm <- tickers[[k]]
      
      all_prices <- rbind(all_prices,new_df)
    }
  }
  
  return(all_prices)
}
build_porfolio <- function(portfolio, year_to_predict, n_portfolio){
  
  weight_formula <- function(rank, n_portfolio, weight_max = 0.1){
    
    find_step <- function(weight_max = 0.1, n_portfolio, n_ite = 1e3){
      step_min <- 0
      step_max <- 1
      A <- weight_max
      
      step <- step_min
      for(i in (1:n_ite)){
        s <- seq(from = A, length.out = n_portfolio, by = -step)
        res <- sum(s[s>0])
        
        if(res > 1){
          step_min <- step
          step <- (step + step_max) / 2
        }else{
          step_max <- step
          step <- (step + step_min) / 2
        }
        if(res == 1) break
      }
      return(step)
    }
    
    step <- find_step(weight_max = weight_max, n_portfolio = n_portfolio)
    
    weights <- seq(from = weight_max, length.out = n_portfolio, by = -step )
    return(weights[rank])
  }
  
  weight_list <- portfolio %>% 
    na.omit() %>% 
    mutate(n = 1:n()) %>% 
    filter(n <= n_portfolio) %>% 
    mutate(weight = weight_formula(rank = n, n_portfolio = n_portfolio)) %>% 
    select(name, weight) %>% 
    rename(firm = name) %>% 
    filter(weight > 0) %>% 
    na.omit()
  
  length_portfolio <- nrow(weight_list)
  
  portfolio %>% 
    na.omit() %>% 
    filter(year == year_to_predict) %>% 
    .[1:length_portfolio,] %>% 
    na.omit() %>% 
    .$name %>% 
    get_hist_prices(
      from = paste0(year_to_predict, '-01-01'), 
      to = paste0(year_to_predict + 1, '-01-01')
    ) %>% 
    group_by(firm) %>% 
    mutate(r = (price - lag(price)) / price) %>% 
    left_join(
      weight_list
    ) %>% 
    group_by(date) %>% 
    summarise(
      r_w = sum(r*weight, na.rm = TRUE),
      r_avg = mean(r, na.rm = TRUE)
      ) %>% 
    rename(r = r_w) %>% 
    select(date, r) %>% 
    as_tibble() %>% 
    na.omit() %>% 
    return()
}

n_sample <- 485

test_files <- sample_n(avaiable.names(), n_sample, replace = FALSE) %>% .$file

all_tickers <- avaiable.names() %>% .$files %>% str_remove_all('.feather')
test_files <- paste0(all_tickers[all_tickers %in% getTickers('sp500')], '.feather') %>% sample(n_sample)
ds         <- ds.generator(n_sample, test_files)
ds         <- fixFormat(ds)


years <- c(2015, 2016, 2017, 2018, 2019)

portfolio <- tibble(name = NA, pred = NA, year = NA)
our_portfolio <- tibble(date = NA, r = NA)
fund_max_size <- 35

for(i in seq_along(years)){
  tr_te <- build_trte(full_data = ds, split_size = 0.7, year_to_predict = years[[i]])
  xgb_model <- train_model(tr_te$train, tr_te$test)

  portfolio <- rbind(
    portfolio, 
    predict_portfolio(model = xgb_model, full_data = ds, year_to_predict = years[[i]], n_firms = fund_max_size) %>% 
      mutate(year = years[[i]])
    ) %>% 
    na.omit()
  
  our_portfolio <- 
    rbind(
      our_portfolio,
      build_porfolio(portfolio, year_to_predict = years[[i]], n_portfolio = fund_max_size)
    )
}

tickers <- 'SPY'
sp_portfolio <- get_hist_prices(tickers, to = '2019-07-01') %>% 
  group_by(firm) %>% 
  mutate(price = (price - lag(price)) / price) %>% 
  group_by(date) %>% 
  summarise(r = mean(price)) %>% 
  data.table()


compute_performance <- function(y){
  current_capital <- 100
  hist_capital <- c()
  for(i in seq_along(y)){
    hist_capital[[i]] <- current_capital
    previous_capital <- current_capital
    current_capital <- previous_capital * (1 + y[[i]])
  }
  return(hist_capital)
}

our_portfolio %>% 
  na.omit() %>% 
  rename(our = r) %>% 
  mutate(date = as.Date(date)) %>% 
  left_join(
    sp_portfolio %>%
      rename(sp = r) %>%
      mutate(date = as.Date(date)) %>%
      as_tibble() %>% 
      na.omit()
    ) %>% 
  select(date, our, sp) %>% 
  mutate(our = ifelse(is.na(our), 0, our)) %>% 
  mutate(sp = ifelse(is.na(sp), 0, sp)) %>% 
  mutate(our_ret = our %>% compute_performance(),
         sp_ret = sp %>% compute_performance()) %>% 
  select(-our, -sp) %>% 
  melt(id.vars = c('date')) %>% 
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_line()
  
