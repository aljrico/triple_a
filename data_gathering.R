add_sp500 <- function(df, prices_path){
  sp500 <- read_feather(paste0(prices_path,"SPY.feather"))
  
  sp500 <- sp500 %>% rename(sp500 = `Adj Close`)
  sp500 <- sp500 %>% select(date, sp500)
  
  setDT(df)
  setDT(sp500)
  
  sp500[, date := ymd(as.Date(date))]
  df[, date := ymd(as.Date(date))]
  
  df[sp500, on = 'date', sp500 := i.sp500]

  df %>% 
    na.locf() %>% 
    return()
}

add_prices <- function(financial_df, price_df, extend_time = TRUE){
  
  price_df <- price_df %>% rename(price = `Adj Close`)
  setDT(financial_df)
  setDT(price_df)
  
  financial_df[, date := ymd(date)]
  price_df[, date := ymd(as.Date(date))]
  
  
  
  dates <- seq(
    from = price_df$date %>% min(),
    to = price_df$date %>% max(),
    by = 'days'
  )
  
  dates <- data.frame(date = dates) %>% as.data.table()
  
  if(extend_time) financial_df <- merge(financial_df, dates, by = 'date', all.y = TRUE)
  financial_df[price_df, on = "date", price := i.price]
  
  financial_df %>% 
    na.locf() %>% 
    return()
}

add_sector <- function(financial_df, sector_df){
  
  sector_df <- sector_df %>% 
    rename(sector = Sector,
           name = Symbol) %>% 
    select(sector, name)
  
  setDT(financial_df)
  setDT(sector_df)
  
  setkey(financial_df, 'name')
  setkey(sector_df, 'name')
  
  df <- sector_df[financial_df]
  return(df)
}

add_ratios <- function(financial_df){
  setDT(financial_df)
  
  financial_df[, debt_to_assets := as.numeric(`Total debt`) / as.numeric(`Total current assets`)]
  financial_df[, liquidity_ratio := as.numeric(`Total current assets`) / as.numeric(`Total current liabilities`)]
  financial_df[, quick_ratio := as.numeric(`Cash and cash equivalents`) / as.numeric(`Total current liabilities`)]
  financial_df[, debt_to_equity := as.numeric(`Total liabilities`) / as.numeric(`Shareholders Equity`)]
  financial_df[, dividend_payout := as.numeric(`Dividend per Share`) / as.numeric(`EPS`)]
  financial_df[, roe := as.numeric(`Net Income`) / as.numeric(`Shareholders Equity`)]
  financial_df[, roa := as.numeric(`Net Income`) / as.numeric(`Total assets`)]
  financial_df[, interest_coverage := as.numeric(`EBIT`) / as.numeric(`Interest Expense`)]
  financial_df[, equity_multiplier := as.numeric(`Total assets`) / as.numeric(`Shareholders Equity`)]
  
  financial_df[, per := as.numeric(price) / as.numeric(EPS)]
  
  return(financial_df)
}

ds_generator <- function(firm_names, extend_time = TRUE){
  
  financial_path <- paste0(getwd(),"/data_far/data/")
  prices_path    <- paste0(getwd(),"/data_far/prices/")
  sector_path    <- paste0(getwd(),"/data_far/sector/")
  
  df_list <- list()
  
  displace_financial_time <- function(df){
    setDT(df)
    df[, date := ymd(date) + 60]
    return(df)
  }
  
  for(i in seq_along(firm_names)){
    specific_financial_path <- paste0(financial_path, firm_names[[i]])
    specific_prices_path <- paste0(prices_path, firm_names[[i]])
    
    financial_data <- read_feather(specific_financial_path) %>% displace_financial_time()
    prices_data    <- read_feather(specific_prices_path)
    sector_data    <- read_feather(paste0(sector_path,"tickers_industry.feather"))
    
    df_list[[i]] <- 
      financial_data %>% 
      add_sector(sector_df = sector_data) %>% 
      add_prices(price_df = prices_data, extend_time = extend_time) %>% 
      add_ratios()
  }
  
  complete_data <- rbindlist(df_list)
  
  complete_data %>% 
    add_sp500(prices_path = prices_path)
  return(complete_data)
}


available_companies <- function() {
  
  financial_files <- dir("data_far/data",pattern=".feather")
  price_files     <- dir("data_far/prices",pattern=".feather")
  intersect(financial_files,price_files) %>% 
    str_remove_all('.feather')
}