##
# Performance
##
autoUpdate <- reactiveTimer(perfor_refresh_time)

perfor_data <- reactive({
  autoUpdate()
  withProgress(message = 'Getting portfolio performance data ...', {
    dataset <- UtilGetPortfPerfor()
  })
})

output$last_update_time_perfor_table <- renderText({
  update_datetime <- perfor_data()$update_datetime
  paste0("Last updated: ", format(update_datetime, "%Y-%m-%d %H:%M:%S"))
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

output$perfor_graph_ytd <- renderPlot({
  rets <- perfor_data()$graph$ytd
  ggplot(rets, aes(x = MarketDate, y = Return, color = Regime)) +
    geom_point() +
    geom_label(label = paste0(round(rets$Return*100,1), "%"), nudge_x = 0.0, nudge_y = 0.02) +
    ggthemes::theme_wsj() +
    scale_color_manual(name = "Regime", values = user_colors) +
    ggtitle("Cumulative Return", subtitle = paste0("YTD since ", format(rets$MarketDate[1], "%b %Y"))) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
    scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
    theme(legend.position = "none")
})

output$perfor_graph_yfn <- renderPlot({
  rets <- perfor_data()$graph$yfn
  ggplot(rets, aes(x = MarketDate, y = Return, color = Regime)) +
    geom_point() +
    geom_label(label = paste0(round(rets$Return*100,1), "%"), nudge_x = 0.0, nudge_y = 0.02) +
    ggthemes::theme_wsj() +
    scale_color_manual(name = "Regime", values = user_colors) +
    ggtitle("Cumulative Return", subtitle = paste0("Year from now since ", format(rets$MarketDate[1], "%b %Y"))) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
    scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
    theme(legend.position = "none")
})

output$perfor_graph_sinc <- renderPlot({
  rets <- perfor_data()$graph$sinc
  ggplot(rets, aes(x = MarketDate, y = Return, color = Regime)) +
    geom_point() +
    geom_label(label = paste0(round(rets$Return*100,1), "%"), nudge_x = 0.0, nudge_y = 0.02) +
    ggthemes::theme_wsj() +
    scale_color_manual(name = "Regime", values = user_colors) +
    ggtitle("Cumulative Return", subtitle = paste0("Since inception ", format(rets$MarketDate[1], "%b %Y"))) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 month") + 
    scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
    theme(legend.position = "none")
})
