#' Toon beschikbare templates
#'
#' @return data.frame with template name and number of fields
#' and a description of the template
#' @importFrom readr read_tsv
#' @export
#'
show_available_templates <- function() {
  template_content <-
    read_tsv(file.path(
      system.file(package = "inbolims"),
      "report_config",
      "template_fields.tsv"
    ))

  template_cols <- grep("template:", colnames(template_content), value = TRUE)
  stripped_cols <- gsub("template:", "", template_cols)

  summary_result <- template_content %>%
    filter(.data$Type == "select") %>%
    group_by(.data$Type) %>%
    summarise(across(
      all_of(template_cols),
      function(x) {
        sum(x > 0, na.rm = TRUE)
      }
    ))
  long_summary_result <- summary_result %>%
    select(-.data$Type) %>%
    pivot_longer(
      cols = starts_with("template:"),
      names_to = "template_name",
      values_to = "fields"
    ) %>%
    mutate(template_name = gsub("template:", "", .data$template_name))

  template_desc <-
    read_tsv(file.path(
      system.file(package = "inbolims"),
      "report_config",
      "template_description.tsv"
    ))

  template_names <- data.frame(template_name = stripped_cols) %>%
    left_join(long_summary_result, by = "template_name") %>%
    left_join(template_desc, by = "template_name")

  template_names
}
