##
# Watchlist tabpanal
watchlist_tp <- tabPanel(
  "Watchlist",
  
  tabsetPanel(
    tabPanel(
      "Watchlist",
      
      fluidRow(
        
        # Watchlist Column
        column(
          12, 
          tags$div(
            fluidRow(
              column(
                12,
                tags$h4(tags$b("Watchlist"), style="float:left"),
                tags$div(style="float:right; padding:0px, margin:0px, height:100%",
                         actionButton("ticker_search_submit", "Get quote", width = blotter_field_default_width)
                ),
                tags$div(style="float:right; padding:0px, margin:0px, height:100%",
                         textInput("ticker_search", NULL, value = "AAPL-USD", width = blotter_field_default_width)
                )
            )),
            fluidRow(
              column(
                12,
                #tags$h5("Latest information"),
                tags$div(style="padding:0px, margin:0px, height:100%",
                         dataTableOutput("prev_day_quote")), 
                #tags$h5("Historical Performance (1 Year)"),
                tags$div(style="padding:0px, margin:0px, height:100%",
                         plotOutput("hist_return"))
            ))
          )     
        ) # End   
        
      ) # end of fluidrow
    ) # end of tabpanel
  ) # end of tabsetpanel
)