#
# handling watchlist request
#
observeEvent(input$ticker_search_submit, {
  tik <- isolate(input$ticker_search)
  output$prev_day_quote <- DT::renderDataTable({
    prc <- UtilGetStockLastestPrice(tik)
    opt <- as.data.frame(t(c(format(index(prc)[1], "%Y-%m-%d"), round(prc[1,],2))))
    colnames(opt) <- c("Date", colnames(prc))
    
    DT::datatable(opt, options = list(dom = "t"))
  })
  
  output$hist_return <- renderPlot({
    hist_ret <- UtilGetStockHistReturn(tik)
    UtilPlotMarketReturn(hist_ret, input$ticker_search, "1Y")
  })
})