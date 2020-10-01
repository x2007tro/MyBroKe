##
# Get ETF constituents from BBG and return to DB


##
# Connection
##

##
# Connect to SQL server using connection string
##
ConnSqlServer <- function(db_obj){
  conn <- DBI::dbConnect(drv = odbc::odbc(),
                         Driver = "ODBC Driver 17 for SQL Server",
                         Server = db_obj$srv,
                         Database = db_obj$dbn,
                         UID = db_obj$id,
                         PWD = db_obj$pwd)
  return(conn)
}

##
# Connect to MariaDB using connection string
##
ConnMySql <- function(db_obj){
  conn <- DBI::dbConnect(drv = RMariaDB::MariaDB(),
                         user = db_obj$id,
                         password = db_obj$pwd,
                         dbname = db_obj$dbn,
                         host = db_obj$srv,
                         port = db_obj$prt)
  return(conn)
}

##
# Read a table from sel server db
##
ReadDataFromSS <- function(db_obj, tbl_name){
  conn <- ConnMySql(db_obj)
  df <- DBI::dbReadTable(conn, tbl_name)
  DBI::dbDisconnect(conn)  
  return(df)
}

##
# Write a table to sql server db
##
WriteDataToSS <- function(db_obj, data, tbl_name, apd = FALSE){
  conn <- ConnMySql(db_obj)
  df <- DBI::dbWriteTable(conn, name = tbl_name, value = data,
                          append = apd, overwrite = !apd, row.names = FALSE)
  DBI::dbDisconnect(conn) 				  
  return(df)
}

##
# List all tables and queries
##
ListTblsFromSS <- function(db_obj){
  conn <- ConnMySql(db_obj)
  dfs_tn <- DBI::dbListTables(conn, scheme = "dbo")
  DBI::dbDisconnect(conn)
  return(dfs_tn)
}

##
# Send query to db
##
GetQueryResFromSS <- function(db_obj, qry_str){
  conn <- ConnMySql(db_obj)
  qry_conn <- DBI::dbSendQuery(conn, qry_str)
  res <- DBI::dbFetch(qry_conn)
  DBI::dbClearResult(qry_conn)
  DBI::dbDisconnect(conn)
  return(res)
}

# parameters
db_obj <- list(
  srv = "10.0.0.200",
  prt = 3307,
  dbn = "WebappAdmin",
  id = "dspeast2",
  pwd = "yuheng"
)

tmp <- ReadDataFromSS(db_obj, "MyBroKe_PortfolioHoldings")
max_mktdat <- max(tmp$`Market.Date`)
portf_holding <- tmp %>% 
  dplyr::filter(`Market.Date` == max_mktdat & `Security.Type` != "OPT")

# start bloomberg and retrieve
conn <- Rblpapi::blpConnect()
tmp <- lapply(1:nrow(portf_holding), function(i){
  
  sym <- portf_holding$Symbol[i]
  mktdat <- portf_holding$Market.Date
  cur <- portf_holding$Currency[i]
  
  if(cur == "USD"){
    curr <- "US"
  } else (
    curr <- "CA"
  )
  memb <- Rblpapi::bds(paste0(sym, " ", curr, " Equity"), "INDX_MWEIGHT")[,1]
  memb_df <- data.frame(
    `Market Date` = mktdat,
    Symbol = sym,
    Constituent = memb,
    stringsAsFactors = FALSE
  )
  
  return(memb_df)
})
member <- dplyr::bind_rows(tmp)

# Enter into DB
sql_str <- paste0("DELETE FROM MyBroKe_Consituents WHERE `Market Date` = '", max_mktdat, "'")
print(sql_str)
GetQueryResFromSS(db_obj, sql_str)

WriteDataToSS(db_obj, member, "MyBroKe_Consituents", apd = TRUE)
