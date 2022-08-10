##
# Performance
##
perfor_tp <- tabPanel(
  "Performance",
  
  tabsetPanel(
    
    # return table
    tabPanel(
      "Cumulative Return",
      
      fluidRow(
        column(
          6,
          tags$br(),
          tags$div(
            shypka.ddiv(tags$h5(
              style = "padding:4px",
              textOutput("last_update_time_perfor_table")
            ), color = "#ffe4e1"),    #misty rose
            DT::dataTableOutput("perfor_table")
          )
        )
      )
    ),
    
    # return curve
    tabPanel(
      "Account Value History",
      
      fluidRow(
        column(
          12,
          tags$h3('YTD Performance'),
          plotOutput("perfor_graph_ytd")
        )
      ),
      
      fluidRow(
        column(
          12,
          tags$h3('Past 1 Year Performance'),
          plotOutput("perfor_graph_yfn")
        )
      ),
      
      fluidRow(
        column(
          12,
          tags$h3('Max Performance'),
          plotOutput("perfor_graph_sinc")
        )
      )
    ) 
    # End
  )
)