#
# Shiny server
#
mainServer <- function(input, output, session) {

  ##
  # portfolio initialization
  ##
  source("./iServer/portfolio.R", local = TRUE)
  
  ##
  # blotter
  ##
  source("./iServer/blotter_equity.R", local = TRUE)
  
  ##
  # forex
  ##
  source("./iServer/blotter_forex.R", local = TRUE)
  
  ##
  # Option
  ##
  source("./iServer/blotter_option.R", local = TRUE)
  
  ##
  # future
  ##
  source("./iServer/blotter_future.R", local = TRUE)
  
  ##
  # Watchlist
  ##
  source("./iServer/watchlist.R", local = TRUE)
  
  ##
  # market trend
  ##
  source("./iServer/market_trend.R", local = TRUE)
  
  ##
  # market news (not developed yet)
  ##
  # source("./iServer/market_news.R", local = TRUE)
  
  ##
  # economic indicators
  ##
  source("./iServer/ei.R", local = TRUE)
  
  ##
  # account
  ##
  source("./iServer/account.R", local = TRUE)
  
  ##
  # trade history
  ##
  source("./iServer/trade_hist.R", local = TRUE)
  
  ##
  # conf
  ##
  source("./iServer/conf.R", local = TRUE)
  
}
