#
# Handling dynamic trade items 
#
observeEvent(input$opt_blotter_size_selector,{
  #
  # Clear current value
  #
  print(opt_blotter_size_tracker)
  lapply(1:opt_blotter_size_tracker, function(i){
    output[[paste0('opt_trade_item',i)]] <- renderUI({
      tags$div()
    })
  })
  
  #
  # Update new value
  # 
  opt_blotter_size_tracker <<- as.numeric(input$opt_blotter_size_selector)
  print(opt_blotter_size_tracker)
  lapply(1:opt_blotter_size_tracker, function(i){
    output[[paste0('opt_trade_item',i)]] <- renderUI({
      list(
        tags$div(class = "blotter_fields", textInput(paste0('opt_ticker',i), "Symbol", value = "", width = blotter_field_default_width, placeholder = "AAPL")),
        tags$div(class = "blotter_fields", selectInput(paste0('opt_right',i), "Right", choices = c("C", "P"), width = blotter_field_default_width)),
        tags$div(class = "blotter_fields_wide", selectInput(paste0('opt_expiry',i), "Expiry", choices = "", width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", selectInput(paste0('opt_strike',i), "Strike", choices = "", width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", selectInput(paste0('opt_currency',i), "Currency", choices = c("CAD","USD"), width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", selectInput(paste0('opt_side',i), "Side", choices = c("Buy", "Sell"), width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", numericInput(paste0('opt_shares',i), "Shares", value = 0, min = 0, max = 1000,  width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", selectInput(paste0('opt_type',i), "Type", choices = c("Lmt", "Mkt"), width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", numericInput(paste0('opt_limit_price',i), "Limit Price", value = 1, min = 0, max = 1000,  width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", numericInput(paste0('opt_multiplier',i), "Multiplier", value = 100, min = 100, max = 100,  width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", textInput(paste0('opt_trade_value',i), "Trade Value", value = "0", width = blotter_field_default_width)),
        tags$div(class = "blotter_fields", checkboxInput(paste0('opt_transmit',i), "Transmit", value = FALSE, width = blotter_field_default_width)),
        tags$div(class = "blotter_fields_wide", actionButton(class = "btn-primary", paste0('opt_reqc',i), "Request", width = blotter_field_default_width)),
        tags$div(class = "blotter_fields_wide", actionButton(class = "btn-primary", paste0('opt_trade',i), "Trade", width = blotter_field_default_width))
      )
    })
  })
})

#
# Automatically calculate trade_value
#
lapply(1:opt_max_blotter_size, function(i){
  observeEvent({ 
    input[[paste0('opt_shares',i)]]
    input[[paste0('opt_limit_price',i)]]
  }, {
    updateTextInput(session, paste0('opt_trade_value',i), value=input[[paste0('opt_shares',i)]]*input[[paste0('opt_limit_price',i)]]*100)
  })
})

#
# Cancel all trades
#
observeEvent(input$opt_cancel_all_trades, {
  UtilCancelAllTrades()
  # Update active orders
  output$opt_current_active_trades <- renderText({
    res <- paste(active_trade_ids, " ,")
    res <- substr(res, 1, nchar(res)-2)
  })
})

#
# Handling equity contract request
#
lapply(1:opt_max_blotter_size, function(i){
  observeEvent(input[[paste0("opt_reqc",i)]],{
    
    withProgress(message = 'Retrieving contract details ...', {
      res <- UtilGetContractDetails(sym = input[[paste0('opt_ticker',i)]], sec_type = "option")
    })
    
    # Render contract details
    output$opt_cd <- DT::renderDataTable({
      DT::datatable(
        res, 
        options = list(
          pageLength = 20,
          orderClasses = TRUE,
          searching = TRUE,
          paging = TRUE
        ) 
      ) %>%
        DT::formatStyle(
          c("ContractRight"),
          fontWeight = "bold",
          #color = "white",
          color = DT::styleEqual(
            unique(res$ContractRight),
            brewed_colors[1:length(unique(res$ContractRight))]
          )
        ) %>% 
        DT::formatStyle(
          c("Expiry"),
          fontWeight = "bold",
          #color = "white",
          color = DT::styleEqual(
            unique(res$Expiry),
            brewed_colors[1:length(unique(res$Expiry))]
          )
        ) %>% 
        DT::formatStyle(
          c("Strike"),
          fontWeight = "bold",
          #color = "white",
          backgroundColor = DT::styleEqual(
            unique(res$Strike),
            brewed_colors[1:length(unique(res$Strike))]
          )
        ) %>% 
        DT::formatStyle(
          c("Currency"),
          fontWeight = "bold",
          #color = "white",
          color = DT::styleEqual(
            unique(res$Currency),
            brewed_colors[1:length(unique(res$Currency))]
          )
        )
    })
    
    # update eq trade blotter
    if(ncol(res) != 1){
      updateSelectInput(session, paste0('opt_expiry',i), choices = res$Expiry)
      updateSelectInput(session, paste0('opt_strike',i), choices = res$Strike)
      updateSelectInput(session, paste0('opt_currency',i), choices = res$Currency) 
    }
    
  })
})
  
#
# Handling trade order submit
# This needs to be updated for sure
#
lapply(1:opt_max_blotter_size, function(i){
  observeEvent(input[[paste0("opt_trade",i)]],{
    
    blotter <- data.frame(LocalTicker = input[[paste0('opt_ticker',i)]],
                          Right = input[[paste0('opt_right',i)]],
                          Expiry = input[[paste0('opt_expiry',i)]],
                          Strike = input[[paste0('opt_strike',i)]],
                          Exchange = "",
                          Action = input[[paste0('opt_side',i)]],
                          Quantity = input[[paste0('opt_shares',i)]],
                          OrderType = input[[paste0('opt_type',i)]],
                          LimitPrice = input[[paste0('opt_limit_price',i)]],
                          SecurityType = "OPT",
                          Currency = input[[paste0('opt_currency',i)]],
                          TradeSwitch = input[[paste0('opt_transmit',i)]],
                          stringsAsFactors = FALSE)
    
    withProgress(message = 'Trading in progress ...', {
      res <- UtilTradeWithIB(blotter)
      msg <- res$msg_rec
      trd <- res$trade_rec
    })
    
    ## 
    # Write message to db
    WriteDataToSS(db_obj, trd, "MyBroKe_TradeHistory", apd = TRUE)
    WriteDataToSS(db_obj, msg, "MyBroKe_TradeMessage", apd = TRUE)
    
    ifelse(opt_message_count_trader %% opt_max_message_count == 0, 
           msg_id <- opt_max_message_count,
           msg_id <- opt_message_count_trader %% opt_max_message_count)
    output[[paste0('opt_message', msg_id)]] <- renderText({
      msg$Msg
    })
    opt_message_count_trader <<- opt_message_count_trader + 1
    
    # Update active orders
    output$opt_current_active_trades <- renderText({
      res <- paste0(active_trade_ids, collapse = ", ")
      res <- paste0(" ", res)
    })
  })
})