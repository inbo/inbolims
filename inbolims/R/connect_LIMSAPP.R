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

write_db_credentials <- 
  function(file = "dbcredentials.txt", 
           dsn = NULL, 
           uid = "User001", 
           pwd = "123456") {
    writeLines(paste(dsn, uid, pwd, "\n", sep = "\n"), con = file)
  }



###############################################################

#' Ophalen commando-argumenten
#'
#' @param args de argumenten die meekomen bij het oproepen van het script (meestal commandArgs)
#' @param min_args minimum aantal argumenten om te checken of het een effectieve call is, of een test (bij een test werk je interactief en zijn er veel minder commandoargumenten)
#' @param first_arg de positie van het eerste argument (dit zal 5 of 6 zijn afh. van je R). Het eerste argument is het opgeroepen R script, daarna volgt normaal gezien de data-source, de user-name, het passwoord, en de call_id, die de extra benodigde argumenten voor het script ophaalt in de lims databank (tabel C_RSCRIPT_ARGS)
#' @param cred_file de bestandsnaam (al dan niet met padverwijzing) van het 3 regels lange tekstbestand, waar de data-source, user-name en paswoord kunnen opgehaald worden in die volgorde. Dit is enkel relevant in testmodus, dus als er minder dan min_args argumenten zijn
#' @param call_id het nummer waarmee de argumenten uit de lims databank kunnen opgehaald worden (tabel C_RSCRIPT_ARGS)
#'
#' @return character vector waarbij de elementen met de naam dsn (data-source), uid (gebruikersnaam), pwd (paswoord), call_id (call identificatie) teruggegeven worden
#' @export
#'
#' @examples
#' \dontrun{
#' cred_file <- system.file("extdata", "dbcredentials.txt", package = "inbolims")
#' args <- prepare_session(call_id = 5, cred_file = cred_file)
#' }

prepare_session <- function(args = commandArgs(), min_args = 5, first_arg = min_args, 
                            cred_file = "dbcredentials.txt", call_id = NULL){
  is_test <- ifelse(length(args) < min_args, TRUE, FALSE)
  if (is_test) {
    creds <- try(inbolims::read_db_credentials(cred_file))
    if (class(creds)[1] == "try-error") {
      stop("databasse info niet gevonden, zorg dat cred_file verwijst naar een bestaand bestand")
    }
    if (is.null(call_id)) {
      stop("call_id moet een waarde hebben")
    }
    argvec <- c(dsn = creds$dsn, uid = creds$uid, 
                pwd = creds$pwd, call_id = call_id) 
  } else {
    argvec <- c(dsn = args[min_args + 1], uid = args[min_args + 2], 
                pwd = args[min_args + 3], call_id = args[min_args + 4])
  }
  argvec
}


############################################################

#' Lees argumenten uit de database
#'
#' @param conn connection object to the LIMS database
#' @param call_id numeric identifier of the function you want to get the arguments from
#'
#' @return named list with arguments
#' @export
#'
#' @examples
#' \dontrun{
#' conn = lims_db_connect(uid = "ikke", pwd = "123456")
#' read_db_arguments(conn, call_id = 10)
#' }
read_db_arguments <- function(conn, call_id){
  q = paste0("select CALL_ID, CALL_FUN, ARG_NAME, VALUE from C_RSCRIPT_ARGS where CALL_ID = ", call_id)
  rv <- DBI::dbGetQuery(conn, q)
  
}


##############################################################

# #Proberen te verwijderen
# readCreds <- function(file = "dbcredentials.txt", additional_vars = NULL){
#   cn <- read.table(file, stringsAsFactors = FALSE)
#   arglist <- c("", "", "", "", "", cn[1,1], cn[2,1], cn[3,1])
#   if (!is.null(additional_vars)) {
#     for (i in 1:length(additional_vars)) {
#       arglist[8 + i] <- additional_vars[i]
#     }
#   }
#   arglist
# }



##############################################################

