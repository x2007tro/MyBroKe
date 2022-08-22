##
# Performance
##
autoUpdate <- reactiveTimer(perfor_refresh_time)

##
# Get data from server
perfor_data <- reactive({
  autoUpdate()
  withProgress(message = 'Getting portfolio performance data ...', {
    dataset <- UtilGetPortfPerfor(c(acct_number1, acct_number2))
  })
})

perfor_data_oa <- reactive({
  autoUpdate()
  withProgress(message = 'Getting other account performance data ...', {
    dataset2 <- UtilGetPortfPerfor_OA()
  })
})

##
# Update timestamp
# output$last_update_time_perfor_table1 <- renderText({
#   update_datetime <- perfor_data()[[1]]$update_datetime
#   paste0("Last updated: ", format(update_datetime, "%Y-%m-%d %H:%M:%S"))
# })

output$last_update_time_perfor_table2 <- renderText({
  update_datetime <- perfor_data()[[1]]$update_datetime
  paste0("Last updated: ", format(update_datetime, "%Y-%m-%d %H:%M:%S"))
})

##
# Update mwrr
output$mwrr_graph_acct1 <- renderPlot({
  rets <- perfor_data()[[1]]$table
  rets <- rets %>% 
    dplyr::mutate(Regime = ifelse(MWRR > 0, 'positve', 'negtive')) %>% 
    dplyr::mutate(`Time.Frame` = factor(`Time.Frame`, levels = unique(`Time.Frame`)))
  
  ggplot(rets, aes(x = Time.Frame, y = MWRR, fill = Regime)) +
    geom_bar(position="stack", stat="identity") +
    geom_label(aes(label = scales::percent(MWRR, digits = 2))) +
    ggthemes::theme_wsj() +
    ggtitle("Performance Snapshot", subtitle = acct_alias1) +
    theme(legend.position = "none", 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    coord_flip()
})

output$mwrr_graph_acct2 <- renderPlot({
  rets <- perfor_data()[[2]]$table
  rets <- rets %>% 
    dplyr::mutate(Regime = ifelse(MWRR > 0, 'positve', 'negtive')) %>% 
    dplyr::mutate(`Time.Frame` = factor(`Time.Frame`, levels = unique(`Time.Frame`)))
  
  ggplot(rets, aes(x = Time.Frame, y = MWRR, fill = Regime)) +
    geom_bar(position="stack", stat="identity") +
    geom_label(aes(label = scales::percent(MWRR, digits = 2))) +
    ggthemes::theme_wsj() +
    ggtitle("Performance Snapshot", subtitle = acct_alias2) +
    theme(legend.position = "none", 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    coord_flip()
})

output$mwrr_graph_all <- renderPlot({
  rets1 <-perfor_data()[[1]]$table %>% 
    dplyr::filter(`Time.Frame` == 'Since Inception') %>% 
    dplyr::mutate(`Account.Alias` = acct_alias1) %>% 
    dplyr::select(`Account.Alias`, MWRR)
  
  rets2 <-perfor_data()[[2]]$table %>% 
    dplyr::filter(`Time.Frame` == 'Since Inception') %>% 
    dplyr::mutate(`Account.Alias` = acct_alias2) %>% 
    dplyr::select(`Account.Alias`, MWRR)
  
  rets3 <- perfor_data_oa() %>% 
    dplyr::select(`Account.Alias`, `MWRR`)
  
  rets <- rbind.data.frame(rets1, rets2, rets3)
  
  rets <- rets %>% 
    dplyr::mutate(Regime = ifelse(MWRR > 0, 'positve', 'negtive')) %>% 
    dplyr::mutate(`Account.Alias` = factor(`Account.Alias`, levels = unique(`Account.Alias`)))
  
  ggplot(rets, aes(x = Account.Alias, y = MWRR, fill = Regime)) +
    geom_bar(position="stack", stat="identity") +
    geom_label(aes(label = scales::percent(MWRR, digits = 2))) +
    ggthemes::theme_wsj() +
    #scale_fill_manual(name = "Cashflow", values = user_colors) +
    ggtitle("Performance Snapshot", subtitle = "Since Inception") +
    theme(legend.position = "none", 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    coord_flip()
})

##
# Update twrr
output$twrr_graph_acct1 <- renderPlot({
  rets <- perfor_data()[[1]]$table
  rets <- rets %>% 
    dplyr::mutate(Regime = ifelse(TWRR > 0, 'positve', 'negtive')) %>% 
    dplyr::mutate(`Time.Frame` = factor(`Time.Frame`, levels = unique(`Time.Frame`)))
  
  ggplot(rets, aes(x = Time.Frame, y = TWRR, fill = Regime)) +
    geom_bar(position="stack", stat="identity") +
    geom_label(aes(label = scales::percent(TWRR, digits = 2))) +
    ggthemes::theme_wsj() +
    ggtitle("Performance Snapshot", subtitle = acct_alias1) +
    theme(legend.position = "none", 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    coord_flip()
})

output$twrr_graph_acct2 <- renderPlot({
  rets <- perfor_data()[[2]]$table
  rets <- rets %>% 
    dplyr::mutate(Regime = ifelse(TWRR > 0, 'positve', 'negtive')) %>% 
    dplyr::mutate(`Time.Frame` = factor(`Time.Frame`, levels = unique(`Time.Frame`)))
  
  ggplot(rets, aes(x = Time.Frame, y = TWRR, fill = Regime)) +
    geom_bar(position="stack", stat="identity") +
    geom_label(aes(label = scales::percent(TWRR, digits = 2))) +
    ggthemes::theme_wsj() +
    ggtitle("Performance Snapshot", subtitle = acct_alias2) +
    theme(legend.position = "none", 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    coord_flip()
})

output$twrr_graph_all <- renderPlot({
  rets1 <-perfor_data()[[1]]$table %>% 
    dplyr::filter(`Time.Frame` == 'Since Inception') %>% 
    dplyr::mutate(`Account.Alias` = acct_alias1) %>% 
    dplyr::select(`Account.Alias`, TWRR)
  
  rets2 <-perfor_data()[[2]]$table %>% 
    dplyr::filter(`Time.Frame` == 'Since Inception') %>% 
    dplyr::mutate(`Account.Alias` = acct_alias2) %>% 
    dplyr::select(`Account.Alias`, TWRR)
  
  rets3 <- perfor_data_oa() %>% 
    dplyr::select(`Account.Alias`, `TWRR`)
  
  rets <- rbind.data.frame(rets1, rets2, rets3)
  
  rets <- rets %>% 
    dplyr::mutate(Regime = ifelse(TWRR > 0, 'positve', 'negtive')) %>% 
    dplyr::mutate(`Account.Alias` = factor(`Account.Alias`, levels = unique(`Account.Alias`)))
  
  ggplot(rets, aes(x = Account.Alias, y = TWRR, fill = Regime)) +
    geom_bar(position="stack", stat="identity") +
    geom_label(aes(label = scales::percent(TWRR, digits = 2))) +
    ggthemes::theme_wsj() +
    #scale_fill_manual(name = "Cashflow", values = user_colors) +
    ggtitle("Performance Snapshot", subtitle = "Since Inception") +
    theme(legend.position = "none", 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    coord_flip()
})

##
# Update return tables
output$perfor_table_acct1 <- DT::renderDataTable({
  rets <- perfor_data()[[1]]$table %>% 
    dplyr::select(-`Account`)
  DT::datatable(
    rets, 
    options = list(
      pageLength = 10,
      orderClasses = FALSE,
      searching = TRUE,
      paging = FALSE
    ),
    rownames = FALSE
  ) %>%
    DT::formatPercentage('TWRR', 2) %>% 
    DT::formatPercentage('MWRR', 2)
})

output$perfor_table_acct2 <- DT::renderDataTable({
  rets <- perfor_data()[[2]]$table %>% 
    dplyr::select(-`Account`)
  DT::datatable(
    rets, 
    options = list(
      pageLength = 10,
      orderClasses = FALSE,
      searching = TRUE,
      paging = FALSE
    ),
    rownames = FALSE
  ) %>%
    DT::formatPercentage('TWRR', 2) %>% 
    DT::formatPercentage('MWRR', 2)
})

output$perfor_table_oa <- DT::renderDataTable({
  rets <- perfor_data_oa()
  DT::datatable(
    rets, 
    options = list(
      pageLength = 10,
      orderClasses = FALSE,
      searching = TRUE,
      paging = FALSE
    ),
    rownames = FALSE
  ) %>%
    DT::formatPercentage('TWRR', 2) %>% 
    DT::formatPercentage('MWRR', 2) %>% 
    DT::formatRound('Account.Value', digits = 0) %>% 
    DT::formatRound('Profit', digits = 0)
  
})

observeEvent(input$ptoa_input_confirm, {
  
  withProgress(message = 'Adding current account value to DB ...', {
    # Inactivate last account end value
    my_sql <- paste0("UPDATE `MyBroKe_FundTransferHistoryOA` SET `Active`=0 WHERE `Account Number` = '", other_broker_account_numbers[input$ptoa_input_account],"' and `Method` = 'Account Value' and `Period` = 'End'")
    GetQueryResFromSS(db_obj, my_sql)
    
    # Reinsert new account end value
    new_rec <- data.frame(
      datadate = input$ptoa_input_mktdate,
      `Account Number` = other_broker_account_numbers[input$ptoa_input_account],
      Method = 'Account Value',
      Period = 'End',
      Amount = input$ptoa_input_endav,
      Active = 1,
      EntryDatetime = Sys.time(),
      stringsAsFactors = F,
      check.names = F
    )
    WriteDataToSS(db_obj, new_rec, 'MyBroKe_FundTransferHistoryOA', apd = T)
    
    # wait for refresh
  })
  
})

##
# Update historical tables
output$perfor_graph_ytd <- renderPlot({
  rets <- perfor_data()[[1]]$graph$ytd
  ggplot(rets, aes(x = MarketDate, y = Value, fill = Cashflow)) +
    geom_bar(position="stack", stat="identity") +
    ggthemes::theme_wsj() +
    #scale_fill_manual(name = "Cashflow", values = user_colors) +
    ggtitle("Account Value", subtitle = paste0("YTD since ", format(rets$MarketDate[1], "%b %Y"))) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
    #scale_y_continuous(labels = function(x) scales::label_comma(x)) +
    theme(legend.position = "bottom")
})

output$perfor_graph_yfn <- renderPlot({
  rets <- perfor_data()[[1]]$graph$yfn
  ggplot(rets, aes(x = MarketDate, y = Value, fill = Cashflow)) +
    geom_bar(position="stack", stat="identity") +
    ggthemes::theme_wsj() +
    #scale_fill_manual(name = "Cashflow", values = user_colors) +
    ggtitle("Account Value", subtitle = paste0("Year from now since ", format(rets$MarketDate[1], "%b %Y"))) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
    #scale_y_continuous(labels = function(x) scales::label_comma(x)) +
    theme(legend.position = "bottom")
})

output$perfor_graph_sinc <- renderPlot({
  rets <- perfor_data()[[1]]$graph$sinc
  ggplot(rets, aes(x = MarketDate, y = Value, fill = Cashflow)) +
    geom_bar(position="stack", stat="identity") +
    ggthemes::theme_wsj() +
    #scale_fill_manual(name = "Cashflow", values = user_colors) +
    ggtitle("Account Value", subtitle = paste0("Since inception ", format(rets$MarketDate[1], "%b %Y"))) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 month") + 
    #scale_y_continuous(labels = function(x) scales::label_comma(x)) +
    theme(legend.position = "bottom")
})
