stocks = read.csv("nasdaq_returns.csv")
str(stocks)
nrow(table(stocks$stock_symbol))
