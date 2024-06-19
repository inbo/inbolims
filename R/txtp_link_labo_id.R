#' Link het labo_id met het externe id
#'
#' De wetenschappers en labo gebruiken een ander ID om stalen te identificeren,
#' hiermee worden deze gekoppeld.
#' @param conn db connectie
#' @param data dataset waarvan het labo_id gelinkt moet worden aan het extern id
#' @param analysis naam van de textuuranalyse.
#' Indien de exacte naam niet gekend is kan met jokertekens gewerkt worden,
#' bv "TEXTUUR\%" wat zoekt in alle analyses die begint met TEXTUUR
#' @param labo_id_col naam van de kolom die het labo_id bevat
#' @param extern_id_col naam van de kolom waar je het externe id in wil
#' @importFrom DBI dbGetQuery
#' @importFrom lubridate date
#' @import dplyr
#' @return dezelfde dataset met een extra kolom die het extern staalid bevat
#' @export
#' @examples
#' \dontrun{
#' # example code
#' # conn <- #zorg dat je een connectie-object met de databank hebt
#' # data <- data.frame(sample = c(('24-004722','24-004723',
#'                                  '24-004724','24-004725')))
#' link_labo_id(conn, data, analysis = "TEXTUUR%")
#' }
link_labo_id <- function(conn,
                         data,
                         analysis = "TEXTUUR_LD%",
                         labo_id_col = "sample",
                         extern_id_col = "FieldSampleID") {
  data$sample <- data[[labo_id_col]]
  # aanpassing omdat soms ---testnummer aan de sample gehangen wordt
  found_indices <- unlist(gregexpr("---", data$sample))
  last_index <- ifelse(found_indices < 0, nchar(data$sample), found_indices - 1)
  data$sample <- substring(data$sample, 1, last_index)

  qry <- paste0(
    "select sample = LabSampleID, \n",
    extern_id_col, " = FieldSampleID,  \n",
    "datum = max(AnalysisDate), \n",
    "analyse = LimsAnalysisName \n",
    " from dimSample s inner join factResult r ",
    "on s.SampleKey = r.SampleKey \n",
    " where s.LabSampleID in ",
    "('", paste(unique(data %>% pull(sample)),
      collapse = "','"
    ),
    "') and LimsAnalysisName like '", analysis, "' \n",
    "group by LabSampleID, FieldSampleID, LimsAnalysisName"
  )

  linktable <- dbGetQuery(conn, qry) %>%
    mutate(datum = lubridate::date(.data$datum)) %>%
    unique() #datums differ in minutes
  unique_analyses <- unique(linktable$analysis)
  if (length(unique_analyses) > 1) {
    cat("Verschillende analyses:\n")
    print(unique_analyses)
    stop("Meer dan 1 analyse die voldoet aan de voorwaarde. Kies er 1 van")
  }

  returndata <- data %>%
    left_join(linktable, by = c("sample" = "sample"))

  if (labo_id_col != "sample") {
    returndata <- returndata %>% select(-sample)
  }

  returndata
}
