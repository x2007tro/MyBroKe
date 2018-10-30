##
# Utility functions for shiny trading portal
##

#
# Get portfolio data
# 
UtilGetPortfolio <- function(){
  ts_tmp <- IBTradingSession$new(11, platform, acct)
  ts_tmp$
    TSSetTransmit(FALSE)$                       #Prevert trade from actually happening
    TSUpdateAccountDetail()$
    TSCloseTradingSession()
  
  return(list(update_datetime = Sys.time(),
              holdings_nonforex = ts_tmp$ts_port_holdings_nonforex %>% dplyr::mutate(UnrealizedPNLPrc = MktPrc/Cost - 1),
              holdings_forex = ts_tmp$ts_port_holdings_forex %>% dplyr::mutate(UnrealizedPNLPrc = MktPrc/Cost - 1),
              port_into = ts_tmp$ts_port_info,
              cash_balance = ts_tmp$ts_cash_balance,
              ts_acc_recon = ts_tmp$ts_acc_recon %>% dplyr::select(-MarketDatetime)))
}

#
# Find current holding
#
UtilFindCurrentHolding <- function(ticker, curr, sec_type){
  port <- UtilGetPortfolio()$holdings_nonforex
  if(nrow(port) == 0){
    pos <- 0
  } else {
    holding <- port %>% dplyr::filter(LocalTicker == ticker & Currency == curr & SecurityType == sec_type)
    
    if(nrow(holding) == 0){
      pos <- 0
    } else {
      pos <- holding[,"Position"]
    }
  }
  return(pos)
}

#
# Find current holding
#
UtilFindCashBalance <- function(currency){
  port <- UtilGetPortfolio()$cash_balance
  if(nrow(port) == 0){
    pos <- 0
  } else {
    cash <- port %>% dplyr::filter(Currency == currency)
    
    if(nrow(cash) == 0){
      pos <- 0
    } else {
      pos <- cash[,"Balance"]
    }
  }
  return(pos)
}

#
# Retrieve contract details
#
UtilGetContractDetails <- function(sym, cur = "", sec_type){
  ts_tmp <- IBTradingSession$new(10, platform, acct)
  res <- ts_tmp$TSGetContractDetails(sym, cur, sec_type)
  ts_tmp$TSCloseTradingSession()
  return(res)
}

#
# Trade equity functions
#
UtilTradeWithIB <- function(blotter){
  for(i in 1:nrow(blotter)){
    ticker <- blotter[i,"LocalTicker"]
    curr <- blotter[i,"Currency"]
    sec_type <- blotter[i,"SecurityType"]
    side <- blotter[i,"Action"]
    trade_shares <- blotter[i,"Quantity"]
    transmit <- blotter[i,"TradeSwitch"]
    
    if(sec_type == "FOREX"){
      transmit <- blotter[i,"TradeSwitch"]
      tgt_curr <- blotter[,"LocalTicker"]
      tgt_value <- blotter[,"Quantity"]
      curr_balance <- UtilFindCashBalance(tgt_curr)
      
      expected_balance <- curr_balance + tgt_value
      
      # ts_static <<- TradingSession(22, platform, acct)
      ts_static$
        TSSetTransmit(transmit)$
        TSSetPrelimTradeList(blotter[i,])$
        TSGenFnlTradeList()
      
      ts_static$
        TSExecuteAllTrades()
      
      curr_trd_id <- ts_static$ts_trade_ids[length(ts_static$ts_trade_ids)]
      actual_balance <- UtilFindCashBalance(tgt_curr)
      
      if(actual_balance >= expected_balance) {
        res <- "Successful"
        flag <- 1
      } else {
        res <- "Failed"
        flag <- 0
      }
    } else {
      
      #
      # Check the current position
      #
      curr_holding <- UtilFindCurrentHolding(ticker, curr, sec_type)
      if(side == "Buy"){
        expected_after_holding <- curr_holding + trade_shares
      } else {
        expected_after_holding <- curr_holding - trade_shares
      }
      
      #
      # Trade
      #
      # ts_static <<- TradingSession(22, platform, acct)
      ts_static$
        TSSetTransmit(transmit)$
        TSSetPrelimTradeList(blotter[i,])
      
        ts_static$TSGenFnlTradeList()$
        TSExecuteAllTrades()
      
      curr_trd_id <- ts_static$ts_trade_ids[length(ts_static$ts_trade_ids)]
      print(curr_trd_id)
      err_msg <- ts_static$ts_last_trade_message[length(ts_static$ts_trade_ids)]
      
      #
      # Run a loop to check if the trade is sucessful
      #
      flag <- 0
      while(i <= trade_time_limit){
        actual_after_holding <- UtilFindCurrentHolding(ticker, curr, sec_type)
        ifelse(actual_after_holding == expected_after_holding, flag <- 1, flag <- 0)
        
        if(flag == 1){
          break
        } else {
          i <- i + 1
          Sys.sleep(1)
        }
      }
    }
    
    #
    # Output results
    #
    trade_res <- blotter
    trade_date <- format(Sys.Date(), "%Y-%m-%d")
    trade_time <- format(Sys.time(), "%H:%M:%S")
    if(flag == 1){
      trade_res$Date <- trade_date
      trade_res$Time <- trade_time
      trade_res$Result <- "Success"
      trade_res$TradeID <- curr_trd_id
      trade_res$TradeMode <- acct
      trade_res$ApplicationStatus <- app_sta
      trade_res <- trade_res[,c(ncol(trade_res),1:(ncol(trade_res)-1))]
      
      msg <- data.frame(Date = trade_date,
                        Time = trade_time,
                        TradeMode = acct,
                        ApplicationStatus = app_sta,
                        Msg = paste0(sec_type, " trade (",curr_trd_id, ") ", ticker, " - ", currency," is successfully traded (", side, ") at ",
                                     trade_date, " ", trade_time),
                        stringsAsFactors = FALSE)
    } else {
      if(curr_trd_id != -1){
        active_trade_ids <<- c(active_trade_ids, curr_trd_id)   # Update background active trades
      }
      trade_res$Date <- trade_date
      trade_res$Time <- trade_time
      trade_res$Result <- "Failed"
      trade_res$TradeID <- curr_trd_id
      trade_res$TradeMode <- acct
      trade_res$ApplicationStatus <- app_sta
      trade_res <- trade_res[,c(ncol(trade_res),1:(ncol(trade_res)-1))]
      
      msg <- data.frame(Date = trade_date,
                        Time = trade_time,
                        TradeMode = acct,
                        ApplicationStatus = app_sta,
                        Msg = paste0(sec_type, " trade (",curr_trd_id, ") ", ticker, " - ", currency," is successfully traded (", side, ") at ",
                                    trade_date, " ", trade_time),
                        stringsAsFactors = FALSE)
    }

  }
  return(list(trade_rec = trade_res, msg_rec = msg))
}

#
# Trade forex functions
#
UtilTradeForexWithIB <- function(blotter){

  for(i in 1:nrow(blotter)){
    curr_us_balance <- UtilFindCurrentHolding("USD")
    curr_ca_balance <- UtilFindCurrentHolding("CAD")
    
    transmit <- blotter[i,"TradeSwitch"]
    tgt_curr <- blotter[,"LocalTicker"]
    tgt_value <- blotter[,"Quantity"]
    curr_balance <- UtilFindCurrentHolding(tgt_curr)
    
    expected_balance <- curr_balance + tgt_value
    
    # ts_static <<- TradingSession(22, platform, acct)
    ts_static$
      TSSetTransmit(transmit)$
      TSSetPrelimTradeList(blotter[i,])$
      TSGenFnlTradeList()$
      TSExecuteAllTrades()
    
    actual_balance <- UtilFindCurrentHolding(tgt_curr)
    
    if(actual_balance >= expected_balance) {
      res <- "Successful"
    } else {
      res <- "Failed"
    }
  }
  return(res)
}

#
# Cancel all trades
#
UtilCancelAllTrades <- function(){
  # Cancel all trades
  ts_static$TSCancelAllTrades()
  active_trade_ids <<- c()
  
  # Re-open ts Static
  # TSCloseTradingSession(ts_static)
  # ts_static <<- TradingSession(22, platform, acct)
}

#
# Download etf historical price and calculate return
#
UtilGetMarketReturn <- function(watchlist){
  #
  # Setup
  #
  start.date <- as.Date("2013-01-01") 
  end.date <- Sys.Date()
  ei.etf.keys <- paste("$", watchlist$LocalTicker, sep="")
  
  #
  # Retrieve quotes
  #
  fshd1 <- FinancialSecurityHistoricalData(id = 1, hist_startdate = start.date,
                                           hist_enddate = end.date)
  fshd1 <- FSHDSetWatchlist(fshd1, watchlist)
  fshd1 <- FSHDObtainAllHistPrcs(fshd1)
  ei.etf <- fshd1$FSHD_hist_cumret
  colnames(ei.etf) <- paste0(watchlist$Comments, " (", watchlist$LocalTicker, ")")
  
  return(ei.etf)
}

UtilGetStockHistReturn <- function(ticker_w_crncy){
  #
  # Setup
  #
  start.date <- as.Date("2013-01-01")
  end.date <- Sys.Date()
  
  pos <- regexpr("-", ticker_w_crncy)[1]
  if(pos == -1){
    ticker <- ticker_w_crncy
    currency <- "USD"
  } else {
    ticker <- substr(ticker_w_crncy, 1, pos-1)
    currency <- substr(ticker_w_crncy, pos+1, nchar(ticker_w_crncy))
  }
  
  watchlist <- data.frame(LocalTicker = ticker,
                          Currency = currency,
                          SecurityType = 'STK',
                          Comments = 'None',
                          stringsAsFactors = FALSE)
  
  #
  # Retrieve quotes
  #
  fshd1 <- FinancialSecurityHistoricalData(id = 1, hist_startdate = start.date,
                                           hist_enddate = end.date)
  fshd1 <- FSHDSetWatchlist(fshd1, watchlist)
  fshd1 <- FSHDObtainAllHistPrcs(fshd1)
  ei.etf <- fshd1$FSHD_hist_cumret
  colnames(ei.etf) <- ticker_w_crncy
  
  return(ei.etf)
}

#
# Plot etf return data
#
UtilPlotMarketReturn <- function(master_plot_data, market, period){
  
  if(market == "Equity"){
    plot_data_prelim <- master_plot_data[,1:3]
  } else if (market == "Tbond"){
    plot_data_prelim <- master_plot_data[,4:7]
  } else if (market == "Cbond"){
    plot_data_prelim <- master_plot_data[,8:10]
  } else {
    # do nothing
    plot_data_prelim <- master_plot_data
  }
  
  # Filter based on period
  if(period == "5D"){
    offset <- 5
  } else if(period == "1M"){
    offset <- 252/12
  } else if(period == "3M"){
    offset <- 252/12 * 3
  } else if(period == "6M"){
    offset <- 252/12 * 6
  } else if(period == "1Y"){
    offset <- 252
  } else if(period == "3Y"){
    offset <- 252*3
  } else if(period == "5Y"){
    offset <- 252*5
  } else if(period == "YTD"){
    offset <- 252
  } else {
    offset <- 0
  }
  
  base <- plot_data_prelim[nrow(plot_data_prelim)-offset-1,]
  fac <- 1/as.vector((1+base))
  last_rec <- nrow(plot_data_prelim)
  plot_data_prelim <- plot_data_prelim[(last_rec-offset):last_rec,]
  plot_data_prelim_mtx <- (1+plot_data_prelim) %*% diag(fac, nrow = length(fac)) - 1
  
  # Transform data to dataframe
  plot_data_prelim_df <- data.frame(Period = index(plot_data_prelim),
                                    Value = plot_data_prelim_mtx,
                                    stringsAsFactors = FALSE)
  colnames(plot_data_prelim_df) <- c("Period",colnames(plot_data_prelim))
  plot_data_final <- tidyr::gather(plot_data_prelim_df, Security, CumRet, -Period)
  
  YearMonthDay <- function(x) format(x, "%Y-%m-%d")
  my_plot <- ggplot(plot_data_final, aes(x = Period, y = CumRet, color = Security)) +
    geom_point() + 
    geom_line() +
    #scale_x_date(date_breaks = "1 day", labels = YearMonthDay) +
    ggtitle(paste0("Cumulative Return for ", market, " Market")) +
    labs(caption = paste0("Plot produced on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))) +
    theme_pka()
  
  return(my_plot)
}

#
# Get lastest quote
#
UtilGetStockLastestPrice <- function(ticker_w_crncy){
  
  pos <- regexpr("-", ticker_w_crncy)[1]
  if(pos == -1){
    ticker <- ticker_w_crncy
  } else {
    ticker <- substr(ticker_w_crncy, 1, pos-1)
    currency <- substr(ticker_w_crncy, pos+1, nchar(ticker_w_crncy))
    if(currency == "CAD"){
      ticker <- paste0(ticker,".TO")
    } 
  }
  
  lprc_prelim <- getSymbols(ticker, auto.assign = FALSE)
  lprc_final <- lprc_prelim[nrow(lprc_prelim),1:5]
  colnames(lprc_final) <- c("Open", "High", "Low", "Close", "Volume")
  
  return(lprc_final)
}

#
# Economic Indicators functions
#
UtilGetEconIndicators <- function(ei_fred, ei_quandl){
  
  #
  # Setup run parameters
  #
  end.date <- Sys.Date() - days(day(Sys.Date())) + 1
  mth.seq <- rev(seq(from = end.date, length = 12, by="-1 month")) 
  start.date <- mth.seq[1]
  
  ###########################################################################
  ############################# Monthly Data ################################
  ###########################################################################
  ei1 <- EconomicIndicators(id = 1, fred_items = ei_fred, quandl_items = ei_quandl,
                            hist_startdate = start.date, hist_enddate = end.date)
  # 
  # Download economic items
  ei1 <- EIDownloadAllFredItems(ei1)
  ei1 <- EIDownloadAllQuandlItems(ei1)
  
  # 
  # Data aggregation for monthly data
  #
  ei.mthly <- merge.xts(ei1$EI_fred_data,
                        ei1$EI_quandl_data,
                        all= TRUE)
  
  #
  # Format monthly data
  #
  res <- lapply(1:length(mth.seq), function(j, mth.seq){
    
    bom <- mth.seq[j]
    yr <- year(bom)
    mh <- month(bom)
    
    if(mh == 12){
      eom <- as.Date(paste(yr+1, "-", "01", "-01", sep="")) - 1
    } else {
      eom <- as.Date(paste(yr, "-", mh+1, "-01", sep="")) - 1
    }
    prd <- paste(bom, eom, sep="/")
    
    sub.data <- ei.mthly[prd]
    month.mean <- colMeans(sub.data, na.rm = TRUE)
    month.mean <- sapply(month.mean, function(x){ format(round(x, 2), nsmall=2, big.mark=",") })
    res <- as.data.frame(month.mean)
    colnames(res) <- names(res)
    
    return(res)
  }, mth.seq)
  pd.mthly.output <- dplyr::bind_cols(res)
  pd.mthly.output[is.na(pd.mthly.output)] <- ""
  colnames(pd.mthly.output) <- format(mth.seq, "%b %Y")
  pd.mthly.output$Key <- c(names(ei_fred), names(ei_quandl))
  
  #
  # Merge Data
  #
  gei_data <- merge.data.frame(gei_lookup, pd.mthly.output, by="Key")[,-(1:4)]
  lei_data <- merge.data.frame(lei_lookup, pd.mthly.output, by="Key")[,-(1:4)]
  coi_data <- merge.data.frame(coi_lookup, pd.mthly.output, by="Key")[,-(1:4)]
  lai_data <- merge.data.frame(lai_lookup, pd.mthly.output, by="Key")[,-(1:4)]
  
  #
  # Return data
  #
  return(list(gei_dt = gei_data,
              lei_dt = lei_data,
              coi_dt = coi_data,
              lai_dt = lai_data))
  
}
#
# Manual open & close connection
#
OpenCloseConn <- function(dirc = c("open", "close")){
  d <- match.arg(dirc)
  if(d == "open"){
    if(!ts_static$TSIsConnected()){
      ts_static <<- IBTradingSession$new(22, platform, acct)
    }
  } else {
    if(ts_static$TSIsConnected()){
      ts_static$TSCloseTradingSession()
    }
  }
}
