#' Connect to Data warehouse.
#'
#' De connectie kan enkel gebeuren via windows authentificatie.
#' Er wordt gebruik gemaakt van DBI::dbConnect en odbc::odbc().
#' @param deployment "prd" betekent uit de productieserver halen,
#' "uat" is enkel voor testen met het datawarehouse op de uat server.
#' De toegangsrechten moeten voor uat
#' via referentiebeheer in windows gedefinieerd zijn
#' @param use_rodbc TRUE to use RODBC for the db connection, FALSE for DBI
#' @param uidpwd connectiesting addendun in de vorm "uid=user001;pwd=geheim;"
#' @return database connection object
#' @export
#' @importFrom RODBC odbcDriverConnect
#' @examples
#' \dontrun{
#' conn <- lims_connect()
#' sql <- "select top(10) * from factResult"
#' df_results <- dbGetQuery(conn, sql)
#' str(df_results)
#' }
lims_connect <- function(deployment = "prd", use_rodbc = FALSE, uidpwd = "") {
  if (deployment != "uat") {
    if (use_rodbc) {
      con <- try(RODBC::odbcDriverConnect(
        connection = "Driver={ODBC Driver 13 for SQL Server};Server=inbo-sql08-prd.inbo.be;Database=W0003_00_Lims;Trusted_Connection=yes;Port=1433;" #nolint
      ), silent = TRUE)
      if (inherits(con, "try-error") || inherits(con, "integer")) {
        cstr <- paste0(
          "Driver={ODBC Driver 13 for SQL Server};Server=inbo-sql08-prd.inbouat.be;Database=W0003_00_Lims;Port=1435;",#nolint
          uidpwd
        )
        con <- RODBC::odbcDriverConnect(
          connection = cstr
        )
      }
      if (!inherits(con, "RODBC")) print("Connectie niet gelukt. Ben je op het INBO netwerk of via VPN verbonden? Contacteer de database administrator")#nolint
    } else {
      con <- DBI::dbConnect(odbc::odbc(),
        Driver = "SQL Server",
        Server = "inbo-sql08-prd.inbo.be",
        port = 1433, # toegevoegd voor vpn, weghalen indien dit problemen geeft
        Database = "W0003_00_Lims",
        Trusted_Connection = "True"
      )
      if (!inherits(con, "Microsoft SQL Server")) {
        print("Connectie niet gelukt. Ben je op het INBO netwerk of via VPN verbonden? Contacteer de database administrator") #nolint
      }
    }
  } else {
    # DBI lijkt niet graag te werken met referentiebeheer
    # Referentiebeheer zoeken in windows
    # toevoegen inbo-sql06-uat.inbouat.be  gebruiker INBOUAT\pieter_verschelde

    con <- try(
      RODBC::odbcDriverConnect(
        connection = "Driver={ODBC Driver 13 for SQL Server};Server=inbo-sql06-uat.inbouat.be,1435;Database=W0003_00_Lims;Trusted_Connection=yes;"#nolint
      ),
      silent = TRUE
    )

    if (inherits(con, "try-error") || inherits(con, "integer")) {
      cstr <- paste0(
        "Driver={ODBC Driver 13 for SQL Server};Server=inbo-sql06-uat.inbouat.be,1435;Database=W0003_00_Lims;",#nolint
        uidpwd
      )
      con <- RODBC::odbcDriverConnect(
        connection = cstr
      )
    }
    if (!inherits(con, "RODBC")) print("Connectie niet gelukt. Ben je op het INBO netwerk of via VPN verbonden? Contacteer de database administrator") #nolint
  }
  con
}
