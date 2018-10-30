##
# Portfolio tabpanel
##
portfolio_tp <- tabPanel(
  "Holding",
  
  tabsetPanel(
    tabPanel(
      "Securities ex. Forex",
      
      fluidRow(
        column(
          12,
          tags$div(
            #class = "macro_block",
            
            shypka.ddiv(tags$h5(
              style = "padding:4px",
              textOutput("last_update_time_nonforex")
            ), color = "#ffe4e1"),    #misty rose
            
            shypka.ddiv(
              DT::dataTableOutput("portfolio_holding_nonforex")
            ),
            
            shypka.ddiv(
              actionButton(class = "btn-primary","add_trade_list_submit", "Add to blotter", width = "150px")
            )
          )
        )
      )  
    ),  # end of nonforex security
    
    tabPanel(
      "Forex",
      
      fluidRow(
        column(
          12,
          shypka.ddiv(tags$h5(
            style = "padding:4px",
            textOutput("last_update_time_forex")
          ), color = "#ffe4e1"),    #misty rose
          
          shypka.ddiv(
            DT::dataTableOutput("portfolio_holding_forex")
          )
        )
      )
    ), # end of forex
    
    tabPanel(
      "Cash",
      
      fluidRow(
        column(
          12,
          shypka.ddiv(tags$h5(
            style = "padding:4px",
            textOutput("last_update_time_cashbal")
          ), color = "#ffe4e1"),    #misty rose
          
          shypka.ddiv(
            DT::dataTableOutput("portfolio_cash_balance")
          )
        )
      )
    ) # end of cash balance
    
  )
  
)