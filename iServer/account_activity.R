#
# handling past trades
#
output$past_trades <- DT::renderDataTable({
  trades <- ReadDataFromSS(db_obj, "MyBroKe_TradeHistory")
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
# handling past messages
#
output$past_messages <- DT::renderDataTable({
  messages <- ReadDataFromSS(db_obj, "MyBroKe_TradeMessage")
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

#
# Err log
#
output$err_log <- DT::renderDataTable({
  log <- ReadDataFromSS(db_obj, "100_510_ErrorLog")
  DT::datatable(
    log,
    options = list(
      pageLength = 10,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    )
  )
})
