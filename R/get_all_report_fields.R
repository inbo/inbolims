#' Show available fields
#'
#'
#' @return dataset containing 2 colums: the name of a field in the database
#' and the corresponding description
#' @export
#' @importFrom readr read_tsv
#' @importFrom dplyr filter select
#'
#' @examples{
#' get_all_report_fields()
#' }

get_all_report_fields <- function() {
  read_tsv(file.path(
    system.file(package = "inbolims"),
    "report_config",
    "template_fields.tsv"
  )) %>%
    filter(.data$Type == "select") %>%
    select(.data$Veldnaam, .data$Beschrijving)
}
