
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

write_db_credentials <- 
  function(file = "dbcredentials.txt", 
           dsn = NULL, 
           uid = "User001", 
           pwd = "123456") {
    writeLines(paste(dsn, uid, pwd, "\n", sep = "\n"), con = file)
  }
