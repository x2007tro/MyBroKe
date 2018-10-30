#
# Update section portfolio
#
autoUpdate <- reactiveTimer(refresh_time)

port_info <- reactive({
  autoUpdate()
  port_info <- UtilGetPortfolio()
})

output$last_update_time_nonforex <- renderText({
  update_datetime <- port_info()$update_datetime
  paste0("Last updated: ", format(update_datetime, "%Y-%m-%d %H:%M:%S"))
})

output$last_update_time_forex <- renderText({
  update_datetime <- port_info()$update_datetime
  paste0("Last updated: ", format(update_datetime, "%Y-%m-%d %H:%M:%S"))
})

output$last_update_time_cashbal <- renderText({
  update_datetime <- port_info()$update_datetime
  paste0("Last updated: ", format(update_datetime, "%Y-%m-%d %H:%M:%S"))
})


output$portfolio_holding_nonforex <- DT::renderDataTable({
  tbl2dis <- port_info()$holdings_nonforex
  
  DT::datatable(
    tbl2dis, 
    options = list(
      pageLength = 20,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    ) 
  ) %>% 
    DT::formatStyle(
      'SecurityType',
      fontWeight = "bold",
      color = "gray",
      backgroundColor = DT::styleEqual(
        unique(tbl2dis$SecurityType),
        RColorBrewer::brewer.pal(n = 12, name = "Set3")[1:length(unique(tbl2dis$SecurityType))]
      )
    ) %>%
    DT::formatCurrency("Position", currency = "", digits = 0) %>% 
    DT::formatCurrency(c("Cost", "MktPrc", "MktVal", "UnrealizedPNL"), currency = "$", digits = 0) %>% 
    DT::formatPercentage("UnrealizedPNLPrc", 2) %>% 
    DT::formatStyle(
      c("UnrealizedPNL", "UnrealizedPNLPrc"),
      fontWeight = "bold",
      #color = "white",
      color = DT::styleInterval(
        0,
        c("#fa8072","#9acd32")    # salmon and yellowgreen
      )
    )
})

output$portfolio_holding_forex <- DT::renderDataTable({
  tbl2dis <- port_info()$holdings_forex
  DT::datatable(
    tbl2dis, 
    options = list(
      pageLength = 20,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    ) 
  ) %>% 
    DT::formatStyle(
      'SecurityType',
      fontWeight = "bold",
      color = "gray",
      backgroundColor = DT::styleEqual(
        unique(tbl2dis$SecurityType),
        RColorBrewer::brewer.pal(n = 12, name = "Set3")[1:length(unique(tbl2dis$SecurityType))]
      )
    ) %>%
    DT::formatCurrency("Position", currency = "", digits = 0) %>% 
    DT::formatCurrency(c("Cost", "MktPrc"), currency = "", digits = 5) %>% 
    DT::formatCurrency(c("MktVal", "UnrealizedPNL"), currency = "$", digits = 0) %>% 
    DT::formatPercentage("UnrealizedPNLPrc", 2) %>% 
    DT::formatStyle(
      c("UnrealizedPNL", "UnrealizedPNLPrc"),
      fontWeight = "bold",
      #color = "white",
      color = DT::styleInterval(
        0,
        c("#fa8072","#9acd32")    # salmon and yellowgreen
      )
    )
})

output$portfolio_cash_balance <- DT::renderDataTable({
  tbl2dis <- port_info()$cash_balance
  DT::datatable(
    tbl2dis, 
    options = list(
      pageLength = 20,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE
    ) 
  ) %>% 
    DT::formatStyle(
      'Currency',
      fontWeight = "bold",
      color = "gray",
      backgroundColor = DT::styleEqual(
        unique(tbl2dis$Currency),
        brewed_colors[1:length(unique(tbl2dis$Currency))]
      )
    ) %>%
    DT::formatCurrency(c("Balance", "CAD Balance"), currency = "$", digits = 0)
})

#
# Handle add to trade list
#
observeEvent(input$add_trade_list_submit, {
  
  selected_rows <- input$portfolio_holding_nonforex_rows_selected
  if(!is.null(selected_rows)){
    for(i in 1:length(selected_rows)){
      my_row <- selected_rows[i]
      
      ##
      # Retrieve values
      holdings <- port_info()$holdings_nonforex
      ticker <- holdings[my_row,"LocalTicker"]
      right <- holdings[my_row,"Right"]
      expiry <- holdings[my_row,"Expiry"]
      strike <- holdings[my_row,"Strike"]
      currency <- holdings[my_row,"Currency"]
      exchange <- holdings[my_row,"Exchange"]
      security_type <- holdings[my_row,"SecurityType"]
      position <- holdings[my_row,"Position"]
      prc <- holdings[my_row,"MktPrc"]
      
      
      if(security_type == "STK"){
        # increase blotter size
        eq_blotter_size_tracker <<- eq_blotter_size_tracker + 1
        
        output[[paste0('eq_trade_item', eq_blotter_size_tracker)]] <- renderUI({
          list(
            tags$div(class = "blotter_fields", textInput(paste0('eq_ticker',eq_blotter_size_tracker), "Symbol", value = ticker, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", selectInput(paste0('eq_exch',eq_blotter_size_tracker), "Exchange", choices = exchange, selected = exchange, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('eq_currency',eq_blotter_size_tracker), "Currency", choices = tradable_curr, selected = currency, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('eq_side',eq_blotter_size_tracker), "Side", choices = c("Buy", "Sell"), selected = "Sell", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('eq_shares',eq_blotter_size_tracker), "Shares", value = position, min = 0, max = 1000,  width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('eq_type',eq_blotter_size_tracker), "Type", choices = c("Lmt", "Mkt"), width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('eq_limit_price',eq_blotter_size_tracker), "Limit Price", value = prc, min = 0, max = 1000, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", textInput(paste0('eq_trade_value',eq_blotter_size_tracker), "Trade Value", value = "0", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", checkboxInput(paste0('eq_transmit',eq_blotter_size_tracker), "Transmit", value = FALSE, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", actionButton(class = "btn-primary", paste0('eq_reqc',eq_blotter_size_tracker), "Request", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", actionButton(class = "btn-primary", paste0('eq_trade',eq_blotter_size_tracker), "Trade", width = blotter_field_default_width))
          )
        })
      } else if (security_type == "OPT"){
        # increase blotter size
        opt_blotter_size_tracker <<- opt_blotter_size_tracker + 1
        
        output[[paste0('opt_trade_item',opt_blotter_size_tracker)]] <- renderUI({
          list(
            tags$div(class = "blotter_fields", textInput(paste0('opt_ticker',i), "Symbol", value = ticker, width = blotter_field_default_width, placeholder = "AAPL")),
            tags$div(class = "blotter_fields", selectInput(paste0('opt_right',i), "Right", choices = c("C", "P"), selected = right, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", selectInput(paste0('opt_expiry',i), "Expiry", choices = expiry, selected = expiry, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('opt_strike',i), "Strike", choices = strike, selected = strike, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('opt_currency',i), "Currency", choices = c("CAD","USD"), selected = currency, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('opt_side',i), "Side", choices = c("Buy", "Sell"), selected = "Sell", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('opt_shares',i), "Shares", value = position, min = 0, max = 1000,  width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('opt_type',i), "Type", choices = c("Lmt", "Mkt"), width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('opt_limit_price',i), "Limit Price", value = prc, min = 0, max = 1000,  width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('opt_multiplier',i), "Multiplier", value = 100, min = 100, max = 100,  width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", textInput(paste0('opt_trade_value',i), "Trade Value", value = "0", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", checkboxInput(paste0('opt_transmit',i), "Transmit", value = FALSE, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", actionButton(class = "btn-primary", paste0('opt_reqc',i), "Request", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", actionButton(class = "btn-primary", paste0('opt_trade',i), "Trade", width = blotter_field_default_width))
          )
        })
      } else if (security_type == "FUT"){
        # increase blotter size
        fut_blotter_size_tracker <<- fut_blotter_size_tracker + 1
        
        output[[paste0('fut_trade_item',fut_blotter_size_tracker)]] <- renderUI({
          list(
            tags$div(class = "blotter_fields", textInput(paste0('fut_ticker',i), "Symbol", value = ticker, width = blotter_field_default_width, placeholder = "AAPL")),
            tags$div(class = "blotter_fields_wide", selectInput(paste0('fut_expiry',i), "Expiry", choices = expiry, selected = expiry, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('fut_currency',i), "Currency", choices = currency, selected = currency, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('fut_side',i), "Side", choices = c("Buy", "Sell"), selected = "Sell", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('fut_shares',i), "Shares", value = position, min = 0, max = 1000,  width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", selectInput(paste0('fut_type',i), "Type", choices = c("Lmt", "Mkt"), width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('fut_limit_price',i), "Limit Price", value = prc, min = 0, max = 1000,  width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput(paste0('fut_multiplier',i), "Multiplier", value = 100, min = 100, max = 100,  width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", textInput(paste0('fut_trade_value',i), "Trade Value", value = "0", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", checkboxInput(paste0('fut_transmit',i), "Transmit", value = FALSE, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", actionButton(class = "btn-primary", paste0('fut_reqc',i), "Request", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", actionButton(class = "btn-primary", paste0('fut_trade',i), "Trade", width = blotter_field_default_width))
          )
        })
      } else {
        print("Invalid Trade!")
      }
      
    }
  } 
})