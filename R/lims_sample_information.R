#' Verkrijg de sample informatie
#'
#' Verkrijg de informatie van stalen voor gekozen projecten
#'
#' @param con connectie naar lims dwh
#' @param project een character string met projecten,
#' eventueel met SQL jokertekens "\%" en "_"
#' @param fields welke velden wil je terugkrijgen, #nolint
#' indien het begint met "template:" wordt een staaltemplate gekozen,
#' anders is dat gewoon een vector van velden. Je kan "template:"
#' en eigen veldnamen combineren bv.
#' c("template:default", "FieldObserver", "FieldSamplingDate")
#'
#' @return tibble met sample informatie
#' @importFrom DBI dbGetQuery
#' @importFrom readr read_tsv
#' @importFrom dplyr pull
#' @export
#'
#' @examples
#' \dontrun{
#' library(inbolims)
#' con <- lims_connect()
#' sample_info <- lims_sample_information(con, project = "I-20G%")
#' sample_info <-
#'   lims_sample_information(
#'     con,
#'     project = c("I-19G024-01", "I-20G%"),
#'     fields = c("template:default", "FieldSamplingDate")
#'   )
#' }
lims_sample_information <-
  function(con,
           project,
           fields = "template:default") {
    all_names <- lims_table_fields(con, "dimSample")
    chosen_fields <- character(0)
    for (fld in fields) {
      if (substring(fld, 1, 9) == "template:") {
        tfld <- sample_fields_from_template(fld)
        chosen_fields <- c(chosen_fields, tfld)
      } else {
        if (fld %in% all_names) {
          chosen_fields <- c(chosen_fields, fld)
        } else {
          stop(paste0(fld, " niet herkend in de data"))
        }
      }
    }

    project_conditions <- paste(
      sprintf("project LIKE '%s'", project),
      collapse = " OR "
    )

    qry <- paste0(
      "select ",
      paste(chosen_fields, collapse = ", "),
      " from dimSample",
      " where ", project_conditions
    )
    dbGetQuery(conn = con, qry)
  }
