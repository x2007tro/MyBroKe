##
# Account tab
account_tp <- tabPanel(
  "Account",
  
  tabsetPanel(
    
    tabPanel(
      "Balance Reconciliation",
      
      fluidRow(
        column(
          12,
          tags$div(DT::dataTableOutput("account_recon"))
        )
      )
    ), # end of panel 1
    
    tabPanel(
      "Raw Metrics",
      
      fluidRow(
        column(
          12,
          tags$div(DT::dataTableOutput("raw_metrics"))
        )
      )
    ) # end of panel 1

  ) # end of tabset panel
              
  
)