#
# handling account info
#
output$account_snapshot <- DT::renderDataTable({
  acct_info <- port_info()$acctInfo
  DT::datatable(
    acct_info, 
    options = list(
      pageLength = 10,
      orderClasses = FALSE,
      searching = TRUE,
      paging = FALSE
    )
  )
})