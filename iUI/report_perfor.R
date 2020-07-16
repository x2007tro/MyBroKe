##
# Performance
##
perfor_tp <- tabPanel(
  "Performance",
  
  tabsetPanel(
    
    # return table
    tabPanel(
      "Table",
      tags$br(),
      DT::dataTableOutput("perfor_table")
    ),
    
    # return curve
    tabPanel(
      "Graph",
      tags$br(),
      plotOutput("perfor_graph")
    )
    # End
  )
  
)