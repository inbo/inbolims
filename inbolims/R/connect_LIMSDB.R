



###############################################################

#' Title
#'
#' @param server name of the LIMS server
#' @param database name of the LIMS database
#' @param uid application username (that as writing rights on the db, so this is not you username)
#' @param pwd applciation username password
#'
#' @return db connection
#' @export
#'
#' @examples
#' \dontrun{
#' lims_db_connect(uid = "me", pwd = "123456") #should not work
#' }
limsdb_connect <- function(server = "inbo-sql07-prd.inbo.be", 
                         database = "D0015_00_Lims", uid, pwd){
  con <- DBI::dbConnect(odbc::odbc(), 
                        Driver = "SQL Server", 
                        Server = server, 
                        Database = database, 
                        uid = unname(uid),
                        pwd = unname(pwd))
  con
}



#' Connect to Data warehouse
#'
#' @param server naam van de server van het LIMS datawarehouse. Staat default correct
#' @param database naam van de database van het LIMS datawaerhouse. Staat default correct
#'
#' @return database connection object
#' @export
#' @examples
#' \dontrun{
#' conn <- lims_connect()
#' sql <- "select top(10) * from factResult"
#' dfResults <- dbGetQuery(conn, sql)
#' str(dfResults)
#' }
limsdwh_connect <- function(server = "inbo-sql08-prd.inbo.be", 
                         database = "W0003_00_Lims"){
  con <- DBI::dbConnect(odbc::odbc(), 
                        Driver = "SQL Server", 
                        Server = server, 
                        Database = database, 
                        Trusted_Connection = "True")
  con
}