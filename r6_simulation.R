investment_fund <- R6::R6Class(
  classname = "investment_fund",
  public = list(
    portfolio = NULL,
    returns = NULL,
    add_companies = function(date_of_decision, strategy, financial_data) {
      picked_companies <- do.call(strategy, list(financial_data = financial_data, date_of_decision = date_of_decision))
      new_log <- data.frame(date = date_of_decision, name = picked_companies)
      self$portfolio <- dplyr::bind_rows(self$portfolio, new_log)
    },
    compute_return = function(date_of_decision, price_data) {
      self$returns <-
        price_data %>%
        select(date, name, price) %>%
        right_join(self$portfolio) %>%
        group_by(name) %>%
        mutate(tomorrows_price = lead(price)) %>%
        mutate(return = tomorrows_price / price - 1) %>%
        group_by(date) %>%
        summarise(return = mean(return, na.rm = TRUE))
    },
    maintain_portfolio = function(date_of_decision) {
      new_log <- self$portfolio %>%
        dplyr::filter(date == date_of_decision - 1) %>%
        dplyr::mutate(date = date_of_decision)

      self$portfolio <- dplyr::bind_rows(self$portfolio, new_log)
    }
  )
)

lowest_per <- function(financial_data, date_of_decision) {
  date_of_decision <- lubridate::ymd(date_of_decision)
  financial_data %>%
    filter(date == date_of_decision) %>%
    top_n(-350, per) %>%
    .$name
}

highest_per <- function(financial_data, date_of_decision) {
  date_of_decision <- lubridate::ymd(date_of_decision)
  financial_data %>%
    filter(date == date_of_decision) %>%
    top_n(350, per) %>%
    .$name
}

random_pick <- function(financial_data, date_of_decision) {
  financial_data %>% 
    filter(date == date_of_decision) %>% 
    .$name %>% 
    unique()
}

magic_formula <- function(financial_data, date_of_decision) {
  financial_data <- financial_data[sector != "Finance"]
  financial_data <- financial_data[sector != "Public Utilities"]
  financial_data <- financial_data[date == date_of_decision]
  financial_data[, market_capitalization := as.numeric(`Weighted Average Shs Out`) * price]
  financial_data <- financial_data[market_capitalization >= 50e6]
  financial_data[, enterprise_value := market_capitalization + as.numeric(`Total current liabilities`) + as.numeric(`Total non-current liabilities`) - as.numeric(`Cash and cash equivalents`)]
  financial_data[, earnings_yield := as.numeric(EBIT) / enterprise_value]
  financial_data[, roc := as.numeric(EBIT) / (as.numeric(`Total assets`) + as.numeric(`Capital Expenditure`))]

  financial_data %>%
    mutate(magic_formula = sqrt(roc * earnings_yield * roe * roa)) %>%
    top_n(350, magic_formula) %>%
    .$name
}


fund_random <- investment_fund$new()
fund_magic <- investment_fund$new()
fund_lp <- investment_fund$new()
fund_hp <- investment_fund$new()

first_date <- ymd("2011-01-01")
last_date <- ymd("2019-01-01")
all_dates <- ymd(seq.Date(from = first_date, to = last_date, by = "day"))
days_since_investment <- Inf

for (d in all_dates) {
  d <- as.POSIXct.Date(d) %>%
    as.Date() %>%
    ymd()

  
  if (days_since_investment > 28) {
    days_since_investment <- 0
    
    fund_random$add_companies(
      date_of_decision = d,
      strategy = "random_pick",
      financial_data = whole_dataset
    )

    fund_magic$add_companies(
      date_of_decision = d,
      strategy = "magic_formula",
      financial_data = whole_dataset
    )
    
    fund_lp$add_companies(
      date_of_decision = d,
      strategy = "lowest_per",
      financial_data = whole_dataset
    )
    
    fund_hp$add_companies(
      date_of_decision = d,
      strategy = "highest_per",
      financial_data = whole_dataset
    )
  } else {
    days_since_investment <- days_since_investment + 1
    fund_random$maintain_portfolio(date_of_decision = d)
    fund_magic$maintain_portfolio(date_of_decision = d)
    fund_lp$maintain_portfolio(date_of_decision = d)
    fund_hp$maintain_portfolio(date_of_decision = d)
  }
}

fund_random$compute_return(price_data = whole_dataset)
fund_magic$compute_return(price_data = whole_dataset)
fund_lp$compute_return(price_data = whole_dataset)
fund_hp$compute_return(price_data = whole_dataset)

random_results <- fund_random$returns %>% rename(random_pick = return)
magic_results  <- fund_magic$returns  %>% rename(magic_formula = return)
lp_results  <- fund_lp$returns  %>% rename(lowest_per = return)
hp_results  <- fund_hp$returns  %>% rename(highest_per = return)

merge(random_results, magic_results) %>%
  merge(lp_results) %>% 
  merge(hp_results) %>% 
  mutate(date = ymd(date)) %>%
  pivot_longer(cols = -c(date)) %>%
  group_by(name) %>%
  mutate(value_evolution = cumprod(1 + value)) %>%
  ggplot(aes(x = date, y = value_evolution, colour = name)) +
  geom_line()
