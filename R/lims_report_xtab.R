#' Maak kruistabel van de ingelezen rapportdata
#'
#' @param reportdata data verkregen uit de functie lims_report_data
#' @param resulttype "formatted" voor de geformatteerde waarde bv <1.20
#' of "measured" de originele waarde bv 1.185455
#' @param sample_fields Vector of sample fields that should appear
#' in the xtab file which are present in the report data
#' @return kruistabel met resultaten
#' @export
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_wider
#' @examples
#' \dontrun{
#' conn <- lims_connect()
#' reportdata <- read_lims_data(conn, project = c("I-19W001-01"))
#' xtab <- lims_report_xtab(reportdata)
#' }
lims_report_xtab <- function(reportdata,
                             resulttype = "measured",
                             sample_fields = c("Project", "ExternSampleID")) {
  if (resulttype == "measured") {
    xtab <- reportdata %>%
      tidyr::pivot_wider(
        id_cols = "OrigineelStaal",
        names_from = "Sleutel",
        values_from = "WaardeRuw"
      )
  } else {
    xtab <- reportdata %>%
      tidyr::pivot_wider(
        id_cols = "OrigineelStaal",
        names_from = "Sleutel",
        values_from = "WaardeGeformatteerd"
      )
  }

  if (length(sample_fields)) {
    columns <- c("OrigineelStaal", sample_fields)
  } else {
    columns <- "OrigineelStaal"
  }
  sample_info <- reportdata %>%
    select(all_of(columns)) %>%
    distinct()

  xtab <- sample_info %>%
    inner_join(xtab, by = "OrigineelStaal")

  xtab
}
