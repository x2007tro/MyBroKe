##
# Performance
##
perfor_tp <- tabPanel(
  "Performance",
  
  tabsetPanel(
    # Equity curve
    
    tabPanel(
      "Return",
      tags$br(),
      plotOutput("cum_ret")
    )
    # End
  )
  
)