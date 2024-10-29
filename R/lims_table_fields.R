#' Get all availaible column names from a table in the data warehouse
#'
#' @param con connection to the lims Dwh. Typically by lims_connect()
#' @param table_name name of the table from which the fields are wanted.
#' Important tables are dimSample and factResult.
#' @param export_type how to return: "character", "tibble" or "data.frame"
#'
#' @return character vector with column names of the table
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr case_match
#' @export
#'
#' @examples
#' library(inbolims)
#' con <- lims_connect()
#' lims_table_fields(con, "dimSample")
lims_table_fields <- function(con, table_name, export_type = "character") {
  rv <- dbGetQuery(conn = con,
             paste0("select COLUMN_NAME ",
                    "from INFORMATION_SCHEMA.COLUMNS ",
                    "where TABLE_NAME = '", table_name, "'"))

  switch(export_type,
             "character" = as.character(rv |> pull("COLUMN_NAME")),
             "tibble"  = tibble(rv),
             "data.frame" = as.data.frame(rv),
             .default = rv)
}
