complete_data <- list.files(path = 'older_data', pattern = '*.feather') %>% 
  lapply(paste0('older_data', .), read_feather) %>% 
  reduce(rbind) %>% 
  data.table()


tickers <- complete_data$name %>% 
  unique() %>% 
  sample(100)

# Prices
stock_prices <- new.env()
getSymbols(tickers, env = stock_prices, src = "yahoo", from = "2008-01-01", auto.assign = TRUE)

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

complete_data <- complete_data %>% 
  mutate(date = ymd(date)) %>% 
  rename(firm = name) %>% 
  right_join(
    all_prices %>% 
      mutate(date = ymd(date))
    ) %>% 
  distinct() %>% 
  na.locf() %>% 
  data.table()
