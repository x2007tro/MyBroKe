##
# Development tab
dev_tp <- tabPanel(
  "Development",
  
  tabsetPanel(
   
    tabPanel(
      "Development",
      
      fluidRow(
        column(
          12,
          tags$h5("Future features under development:"),
          tags$ul(
            tags$li("Add configuration page"),
            tags$li("Better looking theme"),
            tags$li("Add news search function"),
            tags$li("Clean up and reorganize UI"),
            tags$li("Add error handling in Server"),
            tags$li("Parameterize more items"),
            tags$li("Track all active orders in a global variable and fix IB trade class input order ID.")
          ),
          br(), br(),
          tags$h5("Features developed"),
          tags$ul(
            tags$li("Add economic indicators"),
            tags$li("Automatically calculate trade values"),
            tags$li("Add historical trade/messages")
          )
        )
      )
      
    ) # end of panel
    
  ) # end of tabsetpanel
  
)