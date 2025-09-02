#' Velden uit sample_field_template
#'
#' @param template sample_fields keuze
#'
#' @return character vector met kolomnamen
#' @importFrom stringr str_extract
#' @export
#'
#' @examples
#' sample_fields_from_template("default")
sample_fields_from_template <- function(template) {
  if (is.null(template)) {
    files <-
      list.files(
        file.path(
          system.file(package = "inbolims"),
          "report_config"
        ),
        pattern = "sample_fields_.*\\.tsv"
      )
    templates <- str_extract(files, "(?<=sample_fields_).*(?=\\.tsv)")
    return(templates)
  }

  if (substring(template, 1, 9) == "template:") {
    template <- substring(template, 10)
  }

  path <- file.path(
    system.file(package = "inbolims"),
    "report_config",
    paste0("sample_fields_", template, ".tsv")
  )

  if (!file.exists(path)) {
    warning(paste0(
      "template ", template, " bestaat niet. \n",
      "Vind bestaande templates via ",
      "sample_fields_template_info(NULL)"
    ))
    return(character())
  }

  fields <- read_tsv(path, show_col_types = FALSE) |> pull(.data$field_name)
  fields
}
