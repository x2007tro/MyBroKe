##
# blotter tabPanel
blotter_tp <- tabPanel(
  "Blotter",
  
  tabsetPanel(
    tabPanel(
      "Equity",
      
      tags$br(),
      
      fluidRow(
        column(
          12,
          tags$div(
            fluidRow(
              column(
                12,
                tags$div(
                  style="display:block",
                  tags$div(class = "blotter_fields", "Ticker"),
                  tags$div(class = "blotter_fields", "Currency"),
                  tags$div(class = "blotter_fields", "Side"),
                  tags$div(class = "blotter_fields", "Shares"),
                  tags$div(class = "blotter_fields", "Type"),
                  tags$div(class = "blotter_fields", "Limit Price"),
                  tags$div(class = "blotter_fields", "Trade Value"),
                  tags$div(class = "blotter_fields", "Transmit"),
                  tags$div(class = "blotter_fields", "Trade"),
                  br(),
                  
                  lapply(1:max_blotter_size, function(i){
                    fluidRow(
                      column(
                        12,
                        tags$div(
                          style="display:block", uiOutput(paste0('trade_item', i), inline = FALSE))
                      ))
                  })
                )            
              )
            ),
            fluidRow(
              column(
                12,
                shypka.ddiv(
                  selectInput("blotter_size_selector", NULL, choices = 1:10, selected = 5, width = blotter_field_default_width)
                )
              )
            )
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
              shypka.ddiv(tags$h4(tags$b("Active Orders: "))),   # salmon
              tags$div(textOutput("current_active_trades")),
              tags$br(),
              tags$div(
                id = "cancel_all_trades", 
                actionButton(class = "btn-primary", "cancel_all_trades", "Cancel All", width = blotter_field_default_width)
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
            lapply(1:max_message_count, function(i){
              tags$div(style="display:block", textOutput(paste0('message', i), inline = FALSE))
            })
          )
        )
      )
    ),  # end of blotter panel
    
    tabPanel(
      "Forex",
      tags$br(),
      tags$div(
        style="display:block",
        tags$div(class = "blotter_fields", "Target Currency"),
        tags$div(class = "blotter_fields", "Target Value"),
        tags$div(class = "blotter_fields", "Transmit"),
        tags$div(class = "blotter_fields", "Required Currency"),
        tags$div(class = "blotter_fields", "Required Value"),
        br(), br(),
        fluidRow(
          column(
            12,
            tags$div(class = "blotter_fields", selectInput("tgt_curr", NULL, choices = c("USD", "CAD"), width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", numericInput("tgt_val", NULL, 100, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", checkboxInput('forex_trade_transmit', NULL, value = FALSE, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", textInput("req_curr", NULL, value = "CAD", width = blotter_field_default_width)),
            tags$div(class = "blotter_fields", textInput("req_val", NULL, value = "0", width = blotter_field_default_width)),
            tags$div(
              class = "blotter_fields", 
              actionButton(class="btn-primary", "trade_forex", "Trade", width = blotter_field_default_width)
            )
        ))
               
      ),
      
      tags$br(),
      tags$div(style = "border:1px gray solid"),
      
      ##
      # Message div
      fluidRow(
        column(
          12,
          tags$div(
            tags$h4(tags$b("Message")),
            tags$br(),
            tags$div(style="display:block", textOutput("forex_trade_msg", inline = FALSE))
          )
        )
      )
    ) # end of panel 2
  )
)