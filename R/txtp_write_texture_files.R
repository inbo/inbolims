###################


#' Schrijf textuurdata weg per staal in de target directory
#'
#' @param target_path locatie waar de files moeten terechtkomen
#' @param data textuurdata die weggeschreven moet worden
#' @param verbose toon output terwijl de routine loopt
#'
#' @return er wordt een file weggeschreven per aanwezig staal
#' @export
#' @importFrom dplyr filter select pull
#' @importFrom readr write_excel_csv2
#'
write_texture_files <- function(target_path, data, verbose = TRUE) {
  for (samp in unique(data %>% pull(.data$sample))) {
    tmp <- data %>%
      filter(.data$sample == samp) %>%
      transmute(
        .data$sample, .data$FieldSampleID,
        .data$lower_boundary, .data$upper_boundary,
        .data$value, .data$sd,
        .data$datum
      )
    if (any(is.na(tmp %>% pull(.data$FieldSampleID)))) {
      if (verbose) {
        print(paste0(samp, "niet weggeschreven want geen extern id"))
      }
      next
    }
    fn <- paste0(
      max(tmp %>% pull(.data$FieldSampleID)),
      "_",
      date_as_text(max(tmp %>% pull(.data$datum))),
      ".csv"
    )
    if (verbose) print(fn)
    write_excel_csv2(tmp %>% select(-.data$datum),
                     file = file.path(target_path, fn)
    )
  }
  return()
}
