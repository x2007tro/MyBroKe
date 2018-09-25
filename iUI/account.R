##
# Account tab
account_tp <- tabPanel(
  "Balance",
  
  tabsetPanel(
    
    tabPanel(
      "Snapshot",
      
      fluidRow(
        column(
          12,
          tags$div(DT::dataTableOutput("account_snapshot"))
        )
      )
    )
    # end of panel 1
  ) # end of tabset panel
              
  
)