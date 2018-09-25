##
# Source all ui files
##
ui_files <- c("portfolio", "blotter", "watchlist", "trade_hist",
              "market_trend", "market_news", "ei",
              "account", "dev", "conf")
lapply(ui_files, function(f){
  source(paste0("./iUI/", f, ".R"), local = FALSE)
})

##
# Shiny ui
##
mainUI <- fluidPage(theme = shinythemes::shinytheme("simplex"),
  
  # css style
  tags$head(
    includeCSS("stt_style.css")
  ),
  
  navbarPage(
    "MyBroKe",
    
    ##
    # Main panel
    tabPanel(
      "Main",

      navlistPanel(
        widths = c(2,10),
        
        "Trade",
        portfolio_tp,
        blotter_tp,
        watchlist_tp,
        trade_hist_tp,

        "Market",
        market_trend_tp,
        market_news_tp,
        ei_tp,

        "Account",
        account_tp
      )
    ),

    ##
    # Development panel
    tabPanel(
      "Development",
      dev_tp
    ),

    ##
    # Configuration panel
    tabPanel(
      "Configuration",
      conf_tp
    )
    
  ) # End of navbarpage
)
