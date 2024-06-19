#' Haal rapportdata uit LIMS DWH
#'
#' @param connection DBI connection object (see odbc::dbConnect())
#' @param project charactervector met projectnamen
#' @param sql_template indien "default" wordt de standaardquery uitgevoerd,
#' indien "all" worden alle voorziene velden geïmporteerd,
#' bij "minimal" worden enkel de essentiële velden geïmporteerd
#' @param show_query indien TRUE toon de query op het scherm
#' net voordat deze uitgevoerd wordt,
#' je kan deze eventueel kopiëren en aanpassen en doorgeven aan custom_sql_query
#' @param custom_fields charactervector die controleert
#' welke veldnamen je in de output ziet.
#' De velden ORIGINAL_SAMPLE, ANALYSIS, COMPONENT en ENTRY
#' worden altijd standaard meegeleverd.
#' Via de functie get_available_report_fields kan je alle namen
#' van de velden zien.
#' @param custom_where_clause custom SQL where clause dat geplakt wordt aan
#'  "where PROJECT in ('project_x')",
#'  dus een where clause dat je specificeerd als
#'  " ResultNumeric > 0 OR ResultNumeric < 100,
#'  zal vertaald worden naar de sql where clause:
#'  "where PROJECT in ('project_x')
#'  AND (ResultNumeric > 0 OR ResultNumeric < 100)"
#' @param custom_sql_query geldige Sql Query bruikbaar voor de connectie.
#' Indien dit veld actief is, wordt sql_template,
#' custom_fields en custom_where_clause genegeerd
#' @param deployment default "prd", "uat" indien op de ontwikkelomgeving
#' gewerkt wordt.
#' Voorlopig is enkel "prd" actief ondersteund
#' @return data.frame met minstens de velden ORIGINAL_SAMPLE,
#' ANALYSIS, COMPONENT en ENTRY
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- lims_connect()
#' reportdata <- read_lims_data(conn,
#'   project = c("I-19W001-01"),
#'   sql_template = "default"
#' )
#' }
read_lims_data <- function(connection,
                           project,
                           sql_template = "default",
                           show_query = FALSE,
                           custom_fields = NULL,
                           custom_where_clause = NULL,
                           custom_sql_query = NULL,
                           deployment = "prd") {
  # if custom query defined, just execute the query and exit
  if (!is.null(custom_sql_query)) {
    rv <- DBI::dbGetQuery(connection, custom_sql_query)
    return(rv)
  }

  template_information <- get_report_config_info(template = sql_template) %>%
    select(.data$Type, .data$Veldnaam, .data$Afkorting, .data$Kolom,
      template = contains(sql_template)
    ) %>%
    filter(!is.na(.data$template), .data$template > 0) %>%
    arrange(.data$template)

  sql_query <- parse_sql_report_query(template_information, project)
  if (show_query) {
    cat(sql_query)
  }

  rv <- DBI::dbGetQuery(connection, sql_query)
}
