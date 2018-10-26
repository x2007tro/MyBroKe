#
# Update section portfolio
#
autoUpdate <- reactiveTimer(refresh_time)

port_info <- reactive({
  autoUpdate()
  port_info <- UtilGetPortfolio()
})

output$last_update_time <- renderText({
  update_datetime <- port_info()$update_datetime
  paste0("Last updated: ", format(update_datetime, "%Y-%m-%d %H:%M:%S"))
})

output$portfolio_dt <- DT::renderDataTable({
  tbl2dis <- port_info()$portfolio
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

observe({
  updateSelectInput(session, "add_trade_list", choices = port_info()$holdings)
})

#
# Handle add to trade list
#
observeEvent(input$add_trade_list_submit, {
  eq_blotter_size_tracker <<- eq_blotter_size_tracker + 1
  
  holdings <- port_info()$portfolio
  trade_item <- holdings[holdings$Ticker == input$add_trade_list,]
  ticker_w_crncy <- trade_item[1,"Ticker"]
  pos <- regexpr("-", ticker_w_crncy)[1]
  ticker <- substr(ticker_w_crncy, 1, pos-1)
  currency <- substr(ticker_w_crncy, pos+1, nchar(ticker_w_crncy))
  security_type <- trade_item[1,"SecurityType"]
  position <- trade_item[1,"Position"]
  
  output[[paste0('eq_trade_item', eq_blotter_size_tracker)]] <- renderUI({
    list(
      br(),
      tags$div(class = "blotter_fields", textInput(paste0('eq_ticker',eq_blotter_size_tracker), NULL, value = ticker, width = blotter_field_default_width)),
      tags$div(class = "blotter_fields_wide", selectInput(paste0('eq_exch',eq_blotter_size_tracker), NULL, choices = c("TSE", "NASDAQ", "NYSE"), width = blotter_field_default_width)),
      tags$div(class = "blotter_fields", selectInput(paste0('eq_currency',eq_blotter_size_tracker), NULL, choices = c("CAD","USD"), selected = currency, width = blotter_field_default_width)),
      tags$div(class = "blotter_fields", selectInput(paste0('eq_side',eq_blotter_size_tracker), NULL, choices = c("Buy", "Sell"), selected = "Sell", width = blotter_field_default_width)),
      tags$div(class = "blotter_fields", numericInput(paste0('eq_shares',eq_blotter_size_tracker), NULL, value = position, min = 0, max = 1000,  width = blotter_field_default_width)),
      tags$div(class = "blotter_fields", selectInput(paste0('eq_type',eq_blotter_size_tracker), NULL, choices = c("Lmt", "Mkt"), width = blotter_field_default_width)),
      tags$div(class = "blotter_fields", numericInput(paste0('eq_limit_price',eq_blotter_size_tracker), NULL, value = 1, min = 0, max = 1000, width = blotter_field_default_width)),
      tags$div(class = "blotter_fields", textInput(paste0('eq_trade_value',eq_blotter_size_tracker), NULL, value = "0", width = blotter_field_default_width)),
      tags$div(class = "blotter_fields", checkboxInput(paste0('eq_transmit',eq_blotter_size_tracker), NULL, value = FALSE, width = blotter_field_default_width)),
      tags$div(class = "blotter_fields_wide", actionButton(class = "btn-primary", paste0('eq_reqc',eq_blotter_size_tracker), "Request", width = blotter_field_default_width)),
      tags$div(class = "blotter_fields_wide", actionButton(class = "btn-primary", paste0('eq_trade',eq_blotter_size_tracker), "Trade", width = blotter_field_default_width))
      #br()
    )
  })
})