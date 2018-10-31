##
# Trade hist tabpanel
trade_hist_tp <- tabPanel(
  "Account Activity",
  
  tabsetPanel(
    tabPanel(
      "Past Trades",
      tags$br(),
      fluidRow(
        column(
          12,
          DT::dataTableOutput("past_trades")
        )
      )
    ),
    tabPanel(
      "Past Messages",
      tags$br(),
      fluidRow(
        column(
          12,
          DT::dataTableOutput("past_messages")     
        )
      )
    )
    # tabPanel(
    #   "Error Log",
    #   tags$br(),
    #   fluidRow(
    #     column(
    #       12,
    #       DT::dataTableOutput("err_log")     
    #     )
    #   )
    # )
  )
  
)