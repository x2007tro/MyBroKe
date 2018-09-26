#
# handling past trades
#
output$past_trades <- DT::renderDataTable({
  port_info()    # To auto update history
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
  port_info()    # To auto update history
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
