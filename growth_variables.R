growth_variable <- function(df, firm, variable, max_year = 2019) {
  df[[variable]] <- as.numeric(df[[variable]])

  filtered_data <- 
    df %>%
    filter(name == firm) %>%
    arrange(date) %>%
    select(date, {{ variable }}) %>%
    group_by(year = year(date)) %>%
    rename(v = {{ variable }}) %>%
    summarise(v = sum(v)) %>%
    filter(year < max_year)
  
  if(nrow(filtered_data) < 1){
    variable_growth <- NA
  }else{
    
    l <- lm(data = filtered_data, formula = v ~ year)
    
    s <- summary(l)
    r_squared <- s$r.squared
    slope <- l$coefficients[[2]]
    
    variable_growth <- slope * (r_squared^2)
  }

  data.frame(firm = firm, year = max_year, variable_growth = variable_growth, variable = variable)
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

get_yr_returns <- function(firm, max_year) {
  
  prices <- get_hist_prices(firm, from = "1950-01-01", to = "2030-01-01")
  
      last_price <- prices %>%
        filter(year(date) == max_year) %>%
        arrange(desc(date)) %>%
        .$price
      
      prev_price <- prices %>%
        filter(year(date) < max_year) %>%
        arrange(desc(date)) %>%
        .$price
      
      if(length(last_price)*length(prev_price) < 1) return(NA)
      
      last_price <- last_price[[1]]
      prev_price <- prev_price[[1]]
      
      r <- last_price / prev_price - 1
      
  return(r)
}


get_all_growth_variable <- function(financial_data, firms, variables, max_years) {
  df_list <- list()
  all_options <- expand.grid(firms = firms, variables = variables, max_years = max_years)
  pb <- progress_bar$new(total = nrow(all_options))

  for (i in 1:nrow(all_options)) {
    
    firm <- all_options[i, 'firms'] %>% as.character()
    variable <- all_options[i, 'variables'] %>% as.character()
    max_year <- all_options[i, 'max_years']
    
    yr_ret <- get_yr_returns(firm = firm, max_year = max_year)
    
    df_list[[i]] <- financial_data %>%
      growth_variable(firm = firm, variable = variable, max_year = max_year) %>%
      mutate(return = yr_ret) %>%
      data.table()
    pb$tick()
  }

  final_df <- rbindlist(df_list)
  return(final_df)
}

firms_list <- compressed_ds$name %>% unique() # %>% sample(10)
variables <- c('Dividend per Share', 'per', 'debt_to_equity')
max_years <- 2019:2019

data_to_explore <- 
  get_all_growth_variable(financial_data = compressed_ds, 
                          firms = firms_list, 
                          variables = variables, 
                          max_years = max_years) %>% 
  na.omit()

data_to_explore %>% 
  filter(variable_growth^2 < 1) %>% 
  ggplot(aes(x = variable_growth, y = return)) +
  geom_point() +
  facet_wrap(.~variable, scales = 'free')
