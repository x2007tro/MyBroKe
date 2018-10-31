##
# Performance
##
output$cum_ret <- renderPlot({
  
  dataset_full <- ReadDataFromSS(db_obj, "100_020_AccountReconciliationHistory")
  dataset <- dataset_full %>% 
    dplyr::filter(Security.Type == "NetLiquidation" & Currency == "CAD") %>%
    dplyr::select(dplyr::one_of(c("Market.Date", "CAD.Balance")))
  
  prcs <- xts::xts(dataset$CAD.Balance, dataset$Market.Date)
  rets <- PerformanceAnalytics::Return.calculate(prcs, method="discrete")[-1,]
  PerformanceAnalytics::chart.CumReturns(rets, main = "Porfolio Cumulative Return")
})
