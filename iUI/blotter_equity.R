##
# Equity blotter tabPanel
equity_blotter_tp <- tabPanel(
  "Equity",
  
  tabsetPanel(
    ##
    # 1st panel
    tabPanel(
      "Blotter",
      
      tags$br(),
      
      fluidRow(
        column(
          12,
          tags$div(
            style="display:block",
            
            lapply(1:eq_max_blotter_size, function(i){
              fluidRow(
                column(
                  12,
                  tags$div(
                    style="display:block", uiOutput(paste0('eq_trade_item', i), inline = FALSE))
                ))
            })
          )            
        )
      ), # end of equity div
      
      tags$div(style = "border:1px gray solid"),
      
      ##
      # cancel order div
      fluidRow(
        # Cancel order column
        column(
          12, 
          tags$div(
            fluidRow(column(
              12,
              shypka.ddiv(tags$h4(tags$b("Active Orders"))),   # salmon
              tags$div(textOutput("eq_current_active_trades")),
              tags$br(),
              tags$div(
                id = "cancel_all_trades", 
                actionButton(class = "btn-primary", "eq_cancel_all_trades", "Cancel All", width = blotter_field_default_width)
              )
            ))
          )
        )
      ), # end of cancel order div
      
      tags$br(),
      tags$div(style = "border:1px gray solid"),
      tags$br(),
      
      ##
      # Message div
      fluidRow(
        column(
          12, 
          tags$div(
            tags$h4(tags$b("Message")),
            lapply(1:eq_max_message_count, function(i){
              tags$div(style="display:block", textOutput(paste0('eq_message', i), inline = FALSE))
            })
          )
        )
      )
    ),  # end of blotter panel
    
    ##
    # Contract Details tabPanel
    tabPanel(
      "Contract Details",
      DT::dataTableOutput("eq_cd")
    ),
    
    tabPanel(
      "Config",
      
      fluidRow(
        column(
          12,
          tags$br(),
          shypka.ddiv(
            selectInput("eq_blotter_size_selector", tags$b("Slot Size"), choices = 1:10, selected = 1, width = blotter_field_default_width)
          )
        )
      )
      
    )
  )
)