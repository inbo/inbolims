
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
