#
# Auto refresh tables
#
autoUpdate <- reactiveTimer(refresh_time)

sql_tbls <- reactive({
  autoUpdate()
  res <- list(
    trade_hist = ReadDataFromSS(db_obj, "MyBroKe_TradeHistory"),
    trade_msg = ReadDataFromSS(db_obj, "MyBroKe_TradeMessage"),
    err_log = ReadDataFromSS(db_obj, "100_510_ErrorLog"),
    rprofit = ReadDataFromSS(db_obj, "100_710_RealizedProfitHistory")
  )
})

#
# handling past trades
#
output$past_trades <- DT::renderDataTable({
  trades <- sql_tbls()$trade_hist
  DT::datatable(
    trades, 
    options = list(
      pageLength = 10,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    )
  )
})

#
# realized profit
#
output$real_profit <- DT::renderDataTable({
  rpft <- sql_tbls()$rprofit
  DT::datatable(
    rpft,
    options = list(
      pageLength = 10,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    )
  ) %>% 
    DT::formatStyle(
      "Security.Type",
      fontWeight = "bold",
      color = "gray",
      backgroundColor = DT::styleEqual(
        unique(rpft$`Security Type`),
        brewed_colors[1:length(unique(rpft$`Security.Type`))]
      )
    ) %>% 
    DT::formatStyle(
      "Currency",
      fontWeight = "bold",
      color = "gray",
      backgroundColor = DT::styleEqual(
        unique(rpft$Currency),
        brewed_colors[1:length(unique(rpft$Currency))]
      )
    ) %>% 
    DT::formatCurrency(c("Realized Profit"), currency = "$", digits = 2) %>% 
    DT::formatStyle(
      c("Realized Profit"),
      fontWeight = "bold",
      #color = "white",
      color = DT::styleInterval(
        0,
        c("#fa8072","#9acd32")    # salmon and yellowgreen
      )
    )
})

#
# Err log
#
output$err_log <- DT::renderDataTable({
  log <- sql_tbls()$err_log
  DT::datatable(
    log,
    options = list(
      pageLength = 10,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    )
  ) %>% 
    DT::formatStyle(
    "Type",
    fontWeight = "bold",
    color = "gray",
    backgroundColor = DT::styleEqual(
      unique(log$Type),
      brewed_colors[1:length(unique(log$Type))]
    )
  )
})

#
# handling past messages
#
output$past_messages <- DT::renderDataTable({
  messages <- sql_tbls()$trade_msg
  DT::datatable(
    messages, 
    options = list(
      pageLength = 10,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    )
  )
})
