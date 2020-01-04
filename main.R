library(magrittr)
library(xml2)
library(rvest)
library(tidyverse)
library(feather)
library(data.table)
library(lubridate)
library(zoo)
library(financialr)

source("get_financials.R")
source("get_tickers.R")
source("data_gathering.R")
source("data_processing.R")
source("machine_learning.R")
source("strategy.R")

financial_path <- paste0(getwd(), "/data_far/data/")
prices_path <- paste0(getwd(), "/data_far/prices/")
sector_path <- paste0(getwd(), "/data_far/sector/")


n <- 100
sp500_list <- getTickers("sp500")
subject_companies <- intersect(available_companies(), sp500_list) # %>% sample(n)
subject_companies <- subject_companies %>% paste0(., ".feather")


rs <- sample(200)
companies_set_1 <- subject_companies[rs]
companies_set_2 <- subject_companies[-rs]

whole_dataset <- ds_generator(subject_companies,   extend_time = TRUE)
complete_dataset1 <- ds_generator(companies_set_1, extend_time = TRUE)
complete_dataset2 <- ds_generator(companies_set_2, extend_time = TRUE)

dates <- seq.Date(from = as.Date("2017-01-01"), to = as.Date("2019-01-01"), by = "month")
returns_list <- list()

for (i in seq_along(dates)) {
  date_to_evaluate <- dates[[i]]

  tr_te <-
    complete_dataset1 %>%
    define_target(time_step = 365, discrete = TRUE) %>%
    build_trte(year_to_predict = year(date_to_evaluate), split_size = 0.5)

  tr_te2 <-
    complete_dataset2 %>%
    define_target(time_step = 365, discrete = TRUE) %>%
    build_trte(year_to_predict = year(date_to_evaluate), split_size = 0.5)

  model <- train_model(train = tr_te$train, test = tr_te$test)
  companies_recommended <- recommend_companies(model = model, tr_te = tr_te2, n_firms = 10, date_to_evaluate = date_to_evaluate)
  returns_list[[i]] <- get_combined_returns(complete_dataset = complete_dataset2, companies_recommended = companies_recommended)
}


returns_list %>%
  bind_rows() %>%
  group_by(date) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(portfolio_evolution = 100 * cumprod(portfolio_return + 1)) %>%
  mutate(sp500_evolution = 100 * cumprod(sp500_return + 1)) %>%
  select(date, portfolio_evolution, sp500_evolution) %>%
  pivot_longer(cols = -c("date")) %>%
  ggplot(aes(x = date, y = value, colour = name)) +
  geom_line(size = 1)

returns_list %>%
  bind_rows() %>%
  filter(date == "2018-09-09")
group_by(date) %>%
  summarise_all(mean, na.rm = TRUE)




# Update Data -------------------------------------------------------------

library(progress)
all_symbols <- get_symbols_list()

pb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                  total = length(all_symbols), clear = FALSE, width= 60)

for(i in seq_along(all_symbols)){
  pb$tick()
  
  this_symbol <- all_symbols[[i]]

  balance_sheet <- get_balance_sheet_statement(this_symbol)
  cash_flow <- get_cash_flow_statement(this_symbol)
  income <- get_income_statement(this_symbol)
  
  if(is.null(balance_sheet) | is.null(cash_flow) | is.null(income)) next
  if(nrow(balance_sheet) * nrow(cash_flow) * nrow(income) == 0) next
  
  balance_sheet %>% 
    merge(cash_flow) %>% 
    merge(income) %>% 
    write_feather(glue::glue('rfinance_data/{this_symbol}'))
}

