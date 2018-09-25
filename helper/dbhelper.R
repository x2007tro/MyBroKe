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
# Read a table from sel server db
##
ReadDataFromSS <- function(db_obj, tbl_name){
  conn <- ConnSqlServer(db_obj)
  df <- DBI::dbReadTable(conn, tbl_name)
  DBI::dbDisconnect(conn)  
  return(df)
}


ReadDataFromSS_sql <- function(db_obj, tbl_name){
  conn <- ConnSqlServerSharedData(db_obj)
  df <- DBI::dbReadTable(conn, tbl_name)
  DBI::dbDisconnect(conn)  
  return(df)
}
##
# Write a table to sql server db
##
WriteDataToSS <- function(db_obj, data, tbl_name, apd = FALSE){
  conn <- ConnSqlServer(db_obj)
  df <- DBI::dbWriteTable(conn, name = tbl_name, value = data,
                          append = apd, overwrite = !apd, row.names = FALSE)
  DBI::dbDisconnect(conn) 				  
  return(df)
}

##
# List all tables and queries
##
ListTblsFromSS <- function(db_obj){
  conn <- ConnSqlServer(db_obj)
  dfs_tn <- DBI::dbListTables(conn, scheme = "dbo")
  DBI::dbDisconnect(conn)
  return(dfs_tn)
}