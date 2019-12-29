recommend_companies <- function(model, tr_te, n_firms = 20, date_to_evaluate = '2019-01-01'){
  
  today_eval <- 
    tr_te %>% 
    .$eval %>% 
    filter(ymd(date) %in% ymd(date_to_evaluate))
  
  today_xgb <- 
    today_eval %>% 
    select(-name, -date, -target) %>% 
    mutate_if(is.character, as.numeric) %>% 
    as.matrix() %>% 
    xgb.DMatrix()
  
  today_eval$pred <- model %>% predict(today_xgb)
  
  all_recommendations <- 
    today_eval %>% 
    select(name, pred) %>% 
    group_by(name) %>% 
    summarise(pred = mean(pred)) %>% 
    arrange(desc(pred)) %>% 
    top_n(n_firms, pred) %>% 
    mutate(date_of_purchase = date_to_evaluate)
  
  return(all_recommendations)
}

get_sp500_performance <- function(df){
  df %>% 
    select(sp500, date) %>% 
    na.locf() %>% 
    mutate(sp500_return = (lead(sp500, 1) / sp500) - 1)
}


get_portfolio_performance <- function(df, companies_recommended){
  complete_dataset %>% 
    select(date, name, price) %>% 
    right_join(
      companies_recommended
    ) %>% 
    filter(date >= date_of_purchase) %>% 
    select(-pred, -date_of_purchase) %>% 
    group_by(name) %>% 
    mutate(daily_return = (lead(price, 1) / price) - 1) %>% 
    select(-price) %>% 
    group_by(date) %>% 
    summarise(portfolio_return = mean(daily_return))
  # pivot_wider(id_cols = c('date'), names_from = 'name', values_from = 'daily_return', names_sep = '_')
  
}


get_combined_returns <- function(complete_dataset, companies_recommended){
  
  complete_dataset %>% 
    get_portfolio_performance(companies_recommended = companies_recommended) %>% 
    inner_join(
      complete_dataset %>% 
        get_sp500_performance() %>% 
        select(date, sp500_return) %>% 
        filter(date >= date_to_evaluate),
      by = 'date'
    ) %>% 
    filter(date <= ymd(date_to_evaluate) + 365) %>% 
    group_by(date) %>% 
    summarise_all(mean)
}