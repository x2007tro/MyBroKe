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
      
      tabsetPanel(
        tabPanel(
          "YTD",
          tags$br(),
          plotOutput("perfor_graph_ytd")
        ),
        tabPanel(
          "One Year From Now",
          tags$br(),
          plotOutput("perfor_graph_yfn")
        ),
        tabPanel(
          "Since Inception",
          tags$br(),
          plotOutput("perfor_graph_sinc")
        )
      )
    
    )
    # End
  )
  
)