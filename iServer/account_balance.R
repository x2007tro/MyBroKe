#
# handling account info
#
output$acc_bal_acct1 <- DT::renderDataTable({
  tbl2dis <- portf1_info()$ts_acc_recon
  DT::datatable(
    tbl2dis, 
    options = list(
      pageLength = 10,
      orderClasses = FALSE,
      searching = TRUE,
      paging = FALSE
    ) 
  ) %>% 
    DT::formatCurrency(c("Balance", "CAD Balance"), currency = "$", digits = 0) %>% 
    DT::formatCurrency(c("Exchange Rate"), currency = "", digits = 5) %>% 
    DT::formatStyle(
      "Security Type",
      fontWeight = "bold",
      color = "gray",
      backgroundColor = DT::styleEqual(
        unique(tbl2dis$`Security Type`),
        brewed_colors[1:length(unique(tbl2dis$`Security Type`))]
      )
    )
})

output$acc_bal_acct2 <- DT::renderDataTable({
  tbl2dis <- portf2_info()$ts_acc_recon
  DT::datatable(
    tbl2dis, 
    options = list(
      pageLength = 10,
      orderClasses = FALSE,
      searching = TRUE,
      paging = FALSE
    ) 
  ) %>% 
    DT::formatCurrency(c("Balance", "CAD Balance"), currency = "$", digits = 0) %>% 
    DT::formatCurrency(c("Exchange Rate"), currency = "", digits = 5) %>% 
    DT::formatStyle(
      "Security Type",
      fontWeight = "bold",
      color = "gray",
      backgroundColor = DT::styleEqual(
        unique(tbl2dis$`Security Type`),
        brewed_colors[1:length(unique(tbl2dis$`Security Type`))]
      )
    )
})

output$acc_bal_aggr <- DT::renderDataTable({
  acct1 <- portf1_info()$ts_acc_recon
  acct2 <- portf2_info()$ts_acc_recon
  aggr <- acct1 %>% 
    rbind.data.frame(acct2) %>% 
    dplyr::group_by(`Market Date`, `Security Type`, `Currency`, `Trade mode`, `Application Status`) %>% 
    dplyr::summarise(`CAD Balance` = sum(`CAD Balance`)) %>% 
    dplyr::arrange(`CAD Balance`)
  
  tbl2dis <- aggr
  
  DT::datatable(
    tbl2dis, 
    options = list(
      pageLength = 10,
      orderClasses = FALSE,
      searching = TRUE,
      paging = FALSE
    ) 
  ) %>% 
    DT::formatCurrency(c("CAD Balance"), currency = "$", digits = 0) %>% 
    #DT::formatCurrency(c("Exchange Rate"), currency = "", digits = 5) %>% 
    DT::formatStyle(
      "Security Type",
      fontWeight = "bold",
      color = "gray",
      backgroundColor = DT::styleEqual(
        unique(tbl2dis$`Security Type`),
        brewed_colors[1:length(unique(tbl2dis$`Security Type`))]
      )
    )
})