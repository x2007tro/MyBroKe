##
# Source all ui files
##
ui_files <- c("portfolio", "account", "watchlist", "trade_hist", "download",
              "blotter_forex", "blotter_equity", "blotter_option", "blotter_future",
              "market_trend", "market_news", "ei", "report_perfor",
              "dev", "conf")
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
        
        "Account",
        portfolio_tp,
        account_tp,
        watchlist_tp,
        trade_hist_tp,
        download_tp,
        
        "Trade",
        forex_blotter_tp,
        equity_blotter_tp,
        option_blotter_tp,
        future_blotter_tp,

        "Market",
        market_trend_tp,
        market_news_tp,
        ei_tp,
        
        "Report",
        perfor_tp
        
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
