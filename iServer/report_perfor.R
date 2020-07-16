##
# Performance
##
perfor_data <- reactive({
  withProgress(message = 'Getting portfolio performance data ...', {
    dataset <- UtilGetPortfPerfor()
  })
})

output$perfor_table <- DT::renderDataTable({
  rets <- perfor_data()$table
  DT::datatable(
    rets, 
    options = list(
      pageLength = 10,
      orderClasses = FALSE,
      searching = TRUE,
      paging = FALSE
    )
  ) %>%
    DT::formatPercentage('Return', 2)
})


output$perfor_graph <- renderPlot({
  rets <- perfor_data()$graph
  PerformanceAnalytics::chart.CumReturns(rets, main = "Porfolio Cumulative Return")
})
