#
# Market ETFs performance
#
mkt_etfs <- UtilGetMarketReturn(watchlist)
output$equity_mkt <- renderPlot({
  UtilPlotMarketReturn(mkt_etfs, "Equity", input$eq_perf_period)
})

output$tbond_mkt <- renderPlot({
  UtilPlotMarketReturn(mkt_etfs, "Tbond", input$tb_perf_period)
})

output$cbond_mkt <- renderPlot({
  UtilPlotMarketReturn(mkt_etfs, "Cbond", input$cb_perf_period)
})