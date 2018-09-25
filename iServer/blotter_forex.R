#
# handling required currency
#
observeEvent({ 
  input$tgt_curr
}, {
  tgt_c <- input$tgt_curr
  if(tgt_c == "USD"){
    updateTextInput(session, "req_curr", value = "CAD")
  } else {
    updateTextInput(session, "req_curr", value = "USD")
  }
})

#
# handling required value
#
observeEvent({ 
  input$tgt_curr
  input$tgt_val
}, {
  tgt_c <- input$tgt_curr
  tgt_v <- input$tgt_val
  port_info <- UtilGetPortfolio()
  acct_info <- port_info()$acctInfo
  exch_rate <- acct_info[rownames(acct_info) == "ExchangeRate","Value"]
  
  if(tgt_c == "USD"){
    updateTextInput(session, "req_val", value = round(tgt_v * exch_rate,0))
  } else {
    updateTextInput(session, "req_val", value = round(tgt_v / exch_rate,0))
  }
})

#
# handling forex trade
#
observeEvent(input$trade_forex, {
  blotter <- data.frame(TargetCurrency = input$tgt_curr,
                        TargetValue = input$tgt_val,
                        SecurityType = "Forex",
                        TradeSwitch = input$forex_trade_transmit,
                        stringsAsFactors = FALSE)
  res <- UtilTradeForexWithIB(blotter)
  updateTextInput(session, "forex_trade_msg", value = res)
})