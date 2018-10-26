##
# Forex blotter tabPanel
forex_blotter_tp <- tabPanel(
  "Forex",
  
  ##
  # Main panel
  tabsetPanel(
    tabPanel(
      "Blotter",
      tags$br(),
      tags$div(
        style="display:block",
        tags$div(class = "blotter_fields_wide", "Target Currency"),
        tags$div(class = "blotter_fields_wide", "Target Value"),
        tags$div(class = "blotter_fields", "Transmit"),
        tags$div(class = "blotter_fields_wide", "Required Currency"),
        tags$div(class = "blotter_fields_wide", "Required Value"),
        br(), br(),
        fluidRow(
          column(
            12,
            tags$div(class = "blotter_fields_wide", selectInput("tgt_curr", NULL, choices = c("USD", "CAD"), width = blotter_field_default_width_wide)),
            tags$div(class = "blotter_fields_wide", numericInput("tgt_val", NULL, 100, width = blotter_field_default_width_wide)),
            tags$div(class = "blotter_fields", checkboxInput('forex_trade_transmit', NULL, value = FALSE, width = blotter_field_default_width)),
            tags$div(class = "blotter_fields_wide", textInput("req_curr", NULL, value = "CAD", width = blotter_field_default_width_wide)),
            tags$div(class = "blotter_fields_wide", textInput("req_val", NULL, value = "0", width = blotter_field_default_width_wide)),
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
    )
  )
)