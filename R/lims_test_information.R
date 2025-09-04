#' Show test overview
#'
#' @param conn database connection via lims_connect
#' @param project project code, e.g. "I-19W001-01",
#' sql joker characters are allowed, e.g. "I-19W%"
#' @param include_non_reportable logical, if TRUE include non reportable tests,
#' @param include_components logical, if TRUE include components in the output
#' @param include_suffix logical, if TRUE include suffix
#'  (TestReplicateCount__ResultReplicate)
#'
#' @returns data frame with test information
#' @export
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr filter select group_by summarise distinct pull
#'
#' @examples
#' \dontrun{
#' conn <- lims_connect()
#' lims_test_information(conn, project = "I-19W001-01")
#' lims_test_information(conn,
#'   project = "I-19W001-01",
#'   include_components = FALSE
#' )
#' lims_test_information(conn,
#'   project = "I-19W001-01",
#'   include_non_reportable = FALSE
#' )
#' }
lims_test_information <- function(conn, project,
                                  include_non_reportable = TRUE,
                                  include_components = TRUE,
                                  include_suffix = FALSE) {
  project_conditions <- paste(
    sprintf("project LIKE '%s'", project),
    collapse = " OR "
  )
  qry <-
    paste0(
      "SELECT ",
      " r.LimsAnalysisName, ",
      " r.Component, ",
      " r.IsReportable, ",
      " concat(r.TestReplicateCount,",
      "'__',",
      "r.ResultReplicate) as suffix",
      " FROM factResult r ",
      " WHERE ", project_conditions,
      " GROUP BY ",
      " r.LimsAnalysisName, ",
      " r.Component, ",
      " r.IsReportable, ",
      " concat(r.TestReplicateCount,",
      "'__',",
      "r.ResultReplicate)",
      " ORDER BY r.LimsAnalysisName, r.Component"
    )
  tests <- DBI::dbGetQuery(conn, qry)

  # ignore suffix if not wanted
  if (!include_suffix) {
    tests <- tests |>
      dplyr::select(-"suffix") |>
      dplyr::distinct()
  }

  # If non reportables are not included, we filter them out
  if (!include_non_reportable) {
    tests <- tests |>
      dplyr::filter(.data$IsReportable == 1)
  }

  # If components are not included, we count how many components are reported
  if (!include_components) {
    tests <- tests |>
      dplyr::group_by(.data$LimsAnalysisName, .data$Component) |>
      dplyr::summarise(
        reported = sum(.data$IsReportable == 1),
        not_reported = sum(.data$IsReportable == 0),
        partial_reported =
          ifelse(
            .data$reported > 0 &
              .data$not_reported > 0,
            TRUE,
            FALSE
          ),
        .groups = "drop_last"
      ) |>
      dplyr::summarise(
        reported = sum(.data$reported),
        not_reported = sum(.data$not_reported),
        partial_reported = sum(.data$partial_reported),
        .groups = "drop"
      )
  }

  return(tests)
}
