########################################

#' Lees de data credentials van de LIMS hoofddatabank
#'
#' Lees de credentials in voor de database vanuit een bestand. De eerste regel bevat de data source, de tweede regel de username, en de derde regel het passwoord
#' @param file bestandsnaam waaruit de credentials geschreven worden
#'
#' @return list with 3 elements, the data source, user-id and paswoord of the LIMS Main DB
#' @export
#'
#' @examples
#' read_db_credentials(system.file("extdata", "dbcredentials.txt", package = "inbolims"))
read_db_credentials <- function(file = "dbcredentials.txt")
{
  creds <- readLines(file)
  list(dsn = creds[1],
       uid = creds[2], 
       pwd = creds[3])
}

#' Write db credentials
#'
#' @param file bestandsnaam waar je de database credentials wil bewaren
#' @param dsn de naam van de gegevensbron (zie ODBC sources in windows)
#' @param uid de gebruikersnaam voor de db
#' @param pwd het passwoord voor de db voor de gebruikersnaam

#'
#' @return writes a text file with the database credentials
#' @export
#'
#' @examples
#' \dontrun{
#' write_db_credentials(file = "dbcredentials.txt", dsn = "MyDataSource", uid = "Me", pwd = "1234")
#' }

##############################################################

write_db_credentials <- 
  function(file = "dbcredentials.txt", 
           dsn = NULL, 
           uid = "User001", 
           pwd = "123456") {
    writeLines(paste(dsn, uid, pwd, "\n", sep = "\n"), con = file)
  }


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

lims_db_connect <- function(server = "inbo-sql07-prd.inbo.be", 
                         database = "D0015_00_Lims", uid, pwd){
  con <- DBI::dbConnect(odbc::odbc(), 
                        Driver = "SQL Server", 
                        Server = server, 
                        Database = database, 
                        uid = uid,
                        pwd = pwd)
  con
}
