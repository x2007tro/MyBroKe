##
# Balance tab
balance_tp <- tabPanel(
  "Balance",
  
  tabsetPanel(
    
    tabPanel(
      acct_alias1,
      
      fluidRow(
        column(
          12,
          tags$div(DT::dataTableOutput("acc_bal_acct1"))
        )
      )
    ), # end of panel 1
    
    tabPanel(
      acct_alias2,
      
      fluidRow(
        column(
          12,
          tags$div(DT::dataTableOutput("acc_bal_acct2"))
        )
      )
    ), # end of panel 1

    tabPanel(
      'Aggregate',
      
      fluidRow(
        column(
          12,
          tags$div(DT::dataTableOutput("acc_bal_aggr"))
        )
      )
    ) # end of panel 1
    
  ) # end of tabset panel
              
  
)