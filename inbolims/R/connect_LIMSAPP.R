
#' Manueel zetten van de commando argumenten
#'
#' @param argvalues vector met de commando-argumenten (allemaal tussen quotes) zoals je zou verwachten als je het script laat runnen via de command line. Het 6e argument is doorgaans het eerste relevante argument en bevat normaliter de naam van het script, de andere argumenten kunnen variëren, maar vaak zijn dsn, uid en pwd het 7e, 8e en 9e argument, daarna gevolgd door scriptspecifieke parameters
#' @param n_min minimum aantal argumenten die nodig zijn voor gekozen wordt om argvalues in te vullen zoals in argvalues gespecifieerd.
#'
#' @return een lijst met de echte commandArgs() als die bestaan (meer dan n_min argumenten in commandArgs) of de zelf gecreëerde indien de commandArgs() slechts n_min argumenten bevat
#' @export
#'
#' @examples
#' check_and_set_commandArgs(argvalues = c("","","","","","", par1 = "foobar", aantal = 5), 
#'                           n_min = 3)
check_and_set_commandArgs <- function(argvalues = NULL, n_min = 3) {
  tmp <- commandArgs()
  if (length(tmp) < n_min) {
    if (is.null(argvalues)) stop("commandoargumenten moeten meegegeven worden, omdat ze niet bestaan")
    print("argumenten uit argvalues gehaald")
    args <- argvalues
  } else {
    print("argumenten uit commando-aanroep gehaald")
    args <- tmp
  }
  args
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


