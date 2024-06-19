#' Get report config information
#'
#' @param template a character vector of templates you want info from.
#' If "all" then all templates are shown
#'
#' @return dataset wih report template information
#' @export
#'
#' @examples{
#' get_report_config_info(template = "default")
#' }
get_report_config_info <- function(template = "default") {
  data <- read_tsv(file.path(
    system.file(package = "inbolims"),
    "report_config",
    "template_fields.tsv"
  ))
  names <- colnames(data)
  if (template == "all") {
    template_cols <- names[substring(names, 1, 9) == "template:"]
  } else {
    template_cols <- paste0("template:", template)
  }
  base_cols <- c(
    "Type", "Veldnaam", "Tabel", "Afkorting",
    "Kolom", "Beschrijving"
  )
  data %>% select(all_of(c(base_cols, template_cols)))
}
