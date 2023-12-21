#' Parse the query based on the template information and the chosen projects
#'
#' @param template dataset containing the template_information.
#' The structure should be exact as the file shipped with the package
#' @param project character string of projects to filter
#'
#' @return SQL server query
#' @export
#' @examples{
#' temp <- get_report_config_info(template = "default")
#' query <- parse_sql_report_query(temp, "I-19W001-01")
#' query
#' }
#'
parse_sql_report_query <- function(template, project) {
  projects <- paste0("('", paste(project, collapse = "','"), "')")
  template <- template %>% arrange(across(starts_with("template")))
  template <- template %>%
    rename(template = matches("^template"))

  fields <- template %>%
    filter(.data$Type == "select", !is.na(.data$template)) %>%
    mutate(ss = ifelse(.data$Afkorting != "expr",
                       paste0(.data$Veldnaam, " = ",
                              .data$Afkorting, ".[", .data$Kolom, "]"),
                       paste0(.data$Veldnaam, " = ",
                              .data$Kolom)
    )
    ) %>%
    pull(.data$ss) %>%
    paste(collapse = ",\n")
  tables <- template %>%
    filter(.data$Type == "tabel") %>%
    pull(.data$Kolom) %>%
    paste(collapse = " \n")

  filters <- template %>%
    filter(.data$Type == "filter") %>%
    mutate(flt = paste0(.data$Afkorting, ".", .data$Kolom)) %>%
    pull(.data$flt) %>%
    paste(collapse = " AND \n")
  filters <- gsub("NA.where 1 = 1 AND", "", filters)
  filters <- gsub("<<PROJECTEN>>", projects, filters)

  qry <- paste("select ", fields, "from ", tables, "where ", filters)
  return(qry)
}
