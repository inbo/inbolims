#' Haal rapportdata uit LIMS DWH
#'
#' @param connection DBI connection object (see odbc::dbConnect())
#' @param project charactervector met projectnamen
#' @param sql_template indien "default" wordt de standaardquery uitgevoerd,
#' indien "all" worden alle voorziene velden geïmporteerd,
#' bij "minimal" worden enkel de essentiële velden geïmporteerd
#' @param sample_types NOT USED FOR THE MOMENT charactervector met de types staal die je wil inlezen
#' c("Project", "DUP", "BLANK", "CONTROL", "PRBLANCO", "STANDARD")
#' Voor genetica heb je nog andere stalen als QC_METHOD, SUBSAMPLE, ...
#' @param include_non_reportable FALSE indien enkel rapporteerbare resultaten gewenst zijn, TRUE indien je alle resultaten wenst (opgelet dit kunnen er heel veel zijn), of een character vector met talkens ANALYSENAAM__COMPONENTNAAM (dubbele underscore), dus bv  c("ALK_OEP_W__Filtratie", "ALK_OEP_w__Opmerking")
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
#' @param include_non_reportable 
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
                           sample_types = c("Project"),
                           include_non_reportable = FALSE,
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
    select("Type", "Veldnaam", "Afkorting", "Kolom",
           template = contains(sql_template)
    ) %>%
    filter(!is.na(.data$template), .data$template > 0) %>%
    arrange(.data$template)
  
  #calculate non reportables
  rep_crit_row <- which(template_information |> pull("Kolom") == "IsReportable=1")
  sql_add <- ""
  
  if (is.logical(include_non_reportable)) {
    if (include_non_reportable & length(rep_crit_row)) {
      template_information <- template_information |> 
        dplyr::filter("Kolom" != "IsReportable=1")
    } else {
      #do not change anything
    }
  } else { 
    template_information <- template_information |> 
      dplyr::filter(.data$Kolom != "IsReportable=1")
    
    nonreportables <- stringr::str_split(include_non_reportable,
                                pattern = "__",
                                simplify = TRUE)
    nonreportables <- 
      apply(nonreportables,
            1,
            function(x) paste0("(r.LimsAnalysisName = '", x[1], "'", 
                               " and ", "r.Component = '", x[2], "')")) |> 
      paste(collapse = " OR ")
    sql_add <- paste(" AND (r.isReportable = 1 OR (", nonreportables, "))")
  }

  # if (!length(sample_types))
  #   sample_types <- c("Project")
  
  sql_query <- parse_sql_report_query(template_information,
                                      project)
  sql_query <- paste(sql_query, sql_add, sep = "\n")
 
  if (show_query) {
    cat(sql_query)
  }

  rv <- DBI::dbGetQuery(connection, sql_query)
}
