##
# Performance
##
perfor_tp <- tabPanel(
  "Performance",
  
  tabsetPanel(
    
    tabPanel(
      "Return",
      
      fluidRow(
        column(
          12,
          tags$br(),
          tags$h3("MWRR"),
          tags$div(
            
            fluidRow(
              column(
                4,
                plotOutput("mwrr_graph_acct1")
              ),
              column(
                4,
                plotOutput("mwrr_graph_acct2")
              ),
              column(
                4,
                plotOutput('mwrr_graph_all')
              )
            )
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          tags$br(),
          tags$h3("TWRR"),
          tags$div(
            
            fluidRow(
              column(
                4,
                plotOutput("twrr_graph_acct1")
              ),
              column(
                4,
                plotOutput("twrr_graph_acct2")
              ),
              column(
                4,
                plotOutput('twrr_graph_all')
              )
            )
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          tags$br(),
          tags$div(
            # shypka.ddiv(tags$h5(
            #   style = "padding:4px",
            #   textOutput("last_update_time_perfor_table2")
            # ), color = "#ffe4e1"),
            
            fluidRow(
              column(
                4,
                tags$h3(acct_alias1),
                DT::dataTableOutput("perfor_table_acct1")
              ),
              column(
                4,
                tags$h3(acct_alias2),
                DT::dataTableOutput("perfor_table_acct2")
              ),
              column(
                4,
                tags$h3('Other Accounts'),
                DT::dataTableOutput('perfor_table_oa'),
                tags$hr(),
                tags$div(
                  style = 'border:2px;border-color:coral;',
                  fluidRow(
                    column(
                      3,
                      tags$div(class = "perfor_table_oa_input", selectInput(paste0('ptoa_input_account'), "Account", choices = other_broker_account_alias, selected = other_broker_account_alias[1], width = perfor_table_oa_input_width))
                    ),
                    column(
                      3,
                      tags$div(class = "perfor_table_oa_input", dateInput(paste0('ptoa_input_mktdate'), "Market Date", value = Sys.Date(), width = perfor_table_oa_input_width))
                    ),
                    column(
                      3,
                      tags$div(class = "perfor_table_oa_input", numericInput(paste0('ptoa_input_endav'), "Account Value", value = 0, width = perfor_table_oa_input_width))
                    ),
                    column(
                      3,
                      tags$div(class = "perfor_table_oa_input", style = "padding-top:20px", actionButton(class = "btn-primary", "ptoa_input_confirm", "Update", width = button_field_default_width))
                    )
                  )
                )
                
              )
            )
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