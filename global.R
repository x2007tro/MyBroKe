#
# IB Trading specific variables
#
platform <- "IBG"     # Options: TWS, IBG
acct <- "Paper"    # Options: Live, Paper
app_sta <- "Test"
trade_time_limit <- 10
active_trade_ids <- c()
brewed_colors <- rep(RColorBrewer::brewer.pal(n = 9, name = "Set3"), 100)

#
# Common parameters for server and ui
#
blotter_field_default_width <- "150px"
blotter_field_default_width_wide <- "250px"

# equity blotter
eq_max_blotter_size <- 10
eq_max_message_count <- 3
eq_blotter_size_tracker <- 1
eq_message_count_trader <- 1

# option blotter
opt_max_blotter_size <- 10
opt_max_message_count <- 3
opt_blotter_size_tracker <- 1
opt_message_count_trader <- 1

# option blotter
fut_max_blotter_size <- 10
fut_max_message_count <- 3
fut_blotter_size_tracker <- 1
fut_message_count_trader <- 1

# other
econ_indi_panel_default_width <- 12
econ_indi_tab_names <- c("gei_dt", "lei_dt", "coi_dt", "lai_dt")
refresh_time <- 60000
ei_refresh_time <- 24 * 60 * 60 * 1000

#
# connection for database
#
db_obj <- list(
  srv = "192.168.2.120,3773",
  dbn = "WebappAdmin",
  id = "kmin",
  pwd = "yuheng"
)

##
# Obtain tradable currencies from DB
tradable_curr <- unique(ReadDataFromSS(db_obj, "MyBroKe_CashBalanceMap")$Currency)

#
# Load API keys
#
api_tbl <- ReadDataFromSS(db_obj, "MyAPI")
fred_api_key <- api_tbl[api_tbl$APIName == "fred","APIKey"]
quandl_key <- api_tbl[api_tbl$APIName == "quandl","APIKey"]

#
# Load economic indicators from DB
#
watchlist <- ReadDataFromSS(db_obj, "MyBroKe_Watchlist")
gei_lookup <- ReadDataFromSS(db_obj, "MyBroKe_GeneralEI")
lei_lookup <- ReadDataFromSS(db_obj, "MyBroKe_LeadingEI")
coi_lookup <- ReadDataFromSS(db_obj, "MyBroKe_CoincidentEI")
lai_lookup <- ReadDataFromSS(db_obj, "MyBroKe_LaggingEI")

##
# Make fredr and quandl dictionaries
master_lookup <- dplyr::bind_rows(list(gei_lookup, lei_lookup, coi_lookup, lai_lookup))
ei_fred <- master_lookup[master_lookup$APISource == "fred","Item"]
names(ei_fred) <- master_lookup[master_lookup$APISource == "fred","Key"]

ei_quandl <- master_lookup[master_lookup$APISource == "quandl","Item"]
names(ei_quandl) <- master_lookup[master_lookup$APISource == "quandl","Key"]

#
# Load portfolio status function
#
if(R.Version()$os == "linux-gnu"){
  github_folder <- "/home/kmin/Projects/"
} else {
  github_folder <- "c:/Github/"
}

source(paste0(github_folder,"IBTWSTradingSession/IB_TWS_TradingSession.R"))
source(paste0(github_folder,"FinancialSecurityHistoricalData/FinancialSecurityHistoricalData.R"))
source(paste0(github_folder,"EconomicIndicators/EconomicIndicators.R"))
source(paste0(github_folder,"Utils/R/shypka.R"))
source(paste0(github_folder,"Utils/R/ggpthemepka.R"))

#
# Buy/Sell trading session
#
ts_static <- IBTradingSession$new(22, platform, acct)