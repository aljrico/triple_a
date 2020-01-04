define_target <- function(df, time_step = 365, discrete = TRUE) {
  df[, price := as.numeric(price)]
  df[, sp500 := as.numeric(sp500)]

  df <-
    df %>%
    group_by(name) %>%
    mutate(price_change = (lead(price, time_step) / price) - 1) %>%
    mutate(sp500_change = (lead(sp500, time_step) / sp500) - 1)

  if (discrete) {
    df[["target"]] <- ifelse(df[["price_change"]] > df[["sp500_change"]], 1, 0)
  } else {
    df[["target"]] <- df[["price_change"]]
  }

  df <-
    df %>%
    select(-c("price_change", "sp500_change")) %>%
    data.table()

  return(df)
}

build_trte <- function(full_data, split_size = 0.7, year_to_predict) {
  tickers <- full_data$name %>% unique()

  tr_tickers <- sample(tickers, floor(length(tickers) * split_size))


  tr_te <- full_data %>%
    mutate(target = ifelse(is.na(target), 0, target)) %>%
    mutate(date = ymd(date)) %>%
    mutate(sector = as.numeric(as.character(sector))) %>%
    mutate(target = as.numeric(target)) %>%
    select(-name_x, -name_y, -sp500) %>%
    data.table()

  train <- tr_te %>%
    filter(year(ymd(date)) < year_to_predict - 1) %>%
    filter(name %in% tr_tickers) %>%
    select(-date, -name) %>%
    mutate_if(is.character, as.numeric) %>%
    data.table()

  test <- tr_te %>%
    filter(year(date) %in% c(year_to_predict - 1)) %>%
    filter(!(name %in% tr_tickers)) %>%
    select(-date, -name) %>%
    mutate_if(is.character, as.numeric) %>%
    data.table()

  eval <- tr_te %>%
    filter(year(date) >= year_to_predict) %>%
    data.table()

  return(list(train = train, test = test, eval = eval))
}
