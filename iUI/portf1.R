##
# Portfolio tabpanel
##
portf1_tp <- tabPanel(
  acct_alias1,
  
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
              textOutput("portf1_last_update_time_nonforex")
            ), color = "#ffe4e1"),    #misty rose
            
            shypka.ddiv(
              DT::dataTableOutput("portf1_holding_nonforex")
            ),
            
            shypka.ddiv(
              actionButton(class = "btn-primary","portf1_add_trade_list_submit", "Add to blotter", width = "150px")
            )
          )
        )
      )  
    ),  # end of nonforex security
    
    tabPanel(
      "Exposure",
      
      fluidRow(
        column(
          12,
          shypka.ddiv(tags$h5(
            style = "padding:4px",
            textOutput("portf1_last_update_time_sector")
          ), color = "#ffe4e1")    #misty rose
        )
      ),
      
      fluidRow(
        column(
          4,
          
          fluidRow(
            column(
              12,
              tags$h5(class = 'wgt_title', "Weight by Instrument"),
              shypka.ddiv(
                DT::dataTableOutput("portf1_instrument")
              )
            )
          ),
          
          fluidRow(
            column(
              12,
              tags$h5(class = 'wgt_title', "Weight by Asset Category"),
              shypka.ddiv(
                DT::dataTableOutput("portf1_assetcat")
              )
            )
          ),
          
          fluidRow(
            column(
              12,
              tags$h5(class = 'wgt_title', "Weight by Asset Class"),
              shypka.ddiv(
                DT::dataTableOutput("portf1_assetcla")
              )
            )
          )
          
        ),
        
        column(
          4,
          
          fluidRow(
            column(
              12,
              tags$h5(class = 'wgt_title', "Weight by Investment Style"),
              shypka.ddiv(
                DT::dataTableOutput("portf1_style")
              )
            )
          ),
          
          fluidRow(
            column(
              12,
              tags$h5(class = 'wgt_title', "Weight by Sector (Equities Only)"),
              shypka.ddiv(
                DT::dataTableOutput("portf1_sector")
              )
            )
          )
          
        ),
        
        column(
          4,
          fluidRow(
            column(
              12,
              tags$h5(class = 'wgt_title', "Weight by Country (Equities Only)"),
              shypka.ddiv(
                DT::dataTableOutput("portf1_country")
              )
            )
          )
          
        )
      )
    
    ), # end of sector
    
    tabPanel(
      "Forex",
      
      fluidRow(
        column(
          12,
          shypka.ddiv(tags$h5(
            style = "padding:4px",
            textOutput("portf1_last_update_time_forex")
          ), color = "#ffe4e1"),    #misty rose
          
          shypka.ddiv(
            DT::dataTableOutput("portf1_holding_forex")
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
            textOutput("portf1_last_update_time_cashbal")
          ), color = "#ffe4e1"),    #misty rose
          
          shypka.ddiv(
            DT::dataTableOutput("portf1_cash_balance")
          )
        )
      )
    ) # end of cash balance
    
  )
  
)