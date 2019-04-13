library(finreportr)
CompanyInfo("AAPL")
AnnualReports("AAPL")
(GetIncome('AAPL', 2010))
GetBalanceSheet('AAPL', 2017)


lapply(tickers %>% sample(10), CompanyInfo)
