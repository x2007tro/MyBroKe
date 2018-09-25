##
# Portfolio tabpanel
##
portfolio_tp <- tabPanel(
  "Porfolio",
  
  tabsetPanel(
    tabPanel(
      "Holdings",
      
      fluidRow(
        column(
          12,
          tags$div(
            #class = "macro_block",
            
            shypka.ddiv(tags$h5(
              style = "padding:4px",
              textOutput("last_update_time")
            ), color = "#ffe4e1"),    #misty rose
            
            shypka.ddiv(
              DT::dataTableOutput("portfolio_dt")
            ),
            
            shypka.ddiv(
              selectInput("add_trade_list", NULL, choices = c("CAD"), width = "150px")
            ),
            
            shypka.ddiv(
              actionButton(class = "btn-primary","add_trade_list_submit", "Add to blotter", width = "150px")
            )
          )
        )
      )
      
    )
  )
  
)