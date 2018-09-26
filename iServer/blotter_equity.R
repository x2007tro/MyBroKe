#
# Handling dynamic trade items 
#
observeEvent(input$blotter_size_selector,{
  #
  # Clear current value
  #
  print(blotter_size_tracker)
  lapply(1:blotter_size_tracker, function(i){
    output[[paste0('trade_item',i)]] <- renderUI({
      tags$div()
    })
  })
  
  #
  # Update new value
  # 
  blotter_size_tracker <<- as.numeric(input$blotter_size_selector)
  lapply(1:blotter_size_tracker, function(i){
    output[[paste0('trade_item',i)]] <- renderUI({
      list(
        br(),
        tags$div(class = "blotter_fields", textInput(paste0('ticker',i), NULL, value = "", width = blotter_field_default_width, placeholder = "AAPL")),
        tags$div(class = "blotter_fields", selectInput(paste0('currency',i), NULL, choices = c("CAD","USD"), width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", selectInput(paste0('side',i), NULL, choices = c("Buy", "Sell"), width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", numericInput(paste0('shares',i), NULL, value = 0, min = 0, max = 1000,  width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", selectInput(paste0('type',i), NULL, choices = c("Lmt", "Mkt"), width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", numericInput(paste0('limit_price',i), NULL, value = 1, min = 0, max = 1000,  width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", textInput(paste0('trade_value',i), NULL, value = "0", width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", checkboxInput(paste0('transmit',i), NULL, value = FALSE, width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", actionButton(class = "btn-primary", paste0('trade',i), "Trade", width = blotter_field_default_width))
      )
    })
  })
})

#
# Automatically calculate trade_value
#
lapply(1:max_blotter_size, function(i){
  observeEvent({ 
    input[[paste0('shares',i)]]
    input[[paste0('limit_price',i)]]
  }, {
    updateTextInput(session, paste0('trade_value',i), value=input[[paste0('shares',i)]]*input[[paste0('limit_price',i)]])
  })
})

#
# Cancel all trades
#
observeEvent(input$cancel_all_trades, {
  UtilCancelAllTrades()
  # Update active orders
  output$current_active_trades <- renderText({
    res <- paste(active_trade_ids, " ,")
    res <- substr(res, 1, nchar(res)-2)
  })
})

#
# Handling trade order submit
#
lapply(1:max_blotter_size, function(i){
  observeEvent(input[[paste0("trade",i)]],{
    
    blotter <- data.frame(LocalTicker = input[[paste0('ticker',i)]],
                          Action = input[[paste0('side',i)]],
                          Quantity = input[[paste0('shares',i)]],
                          OrderType = input[[paste0('type',i)]],
                          LimitPrice = input[[paste0('limit_price',i)]],
                          SecurityType = "Stk",
                          Currency = input[[paste0('currency',i)]],
                          TradeSwitch = input[[paste0('transmit',i)]],
                          stringsAsFactors = FALSE)
    
    res <- UtilTradeEquityWithIB(blotter)
    msg <- res$msg_rec
    trd <- res$trade_rec
    
    ## 
    # Write message to db
    WriteDataToSS(db_obj, trd, "MyBroKe_TradeHistory", apd = TRUE)
    WriteDataToSS(db_obj, msg, "MyBroKe_TradeMessage", apd = TRUE)
    
    
    ifelse(message_count_trader %% max_message_count == 0, 
           msg_id <- max_message_count,
           msg_id <- message_count_trader %% max_message_count)
    output[[paste0('message', msg_id)]] <- renderText({
      msg$Msg
    })
    message_count_trader <<- message_count_trader + 1
    
    # Update active orders
    output$current_active_trades <- renderText({
      res <- paste0(active_trade_ids, collapse = ", ")
      res <- paste0(" ", res)
    })
  })
})