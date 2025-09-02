#' Converteer datumvector of POSIXct vector naar text in formaat yyyy-mm-dd
#'
#' @param x vector met datums (character)
#' @param na_result resultaat indien geen geldige datum
#'
#' @return character vector with datums
#' @export
#'
date_as_text <- function(x, na_result = "NODATE") {
  x[is.na(x) | substring(x, 1, 2) != "20"] <- NA
  x <- paste0(substring(x, 1, 10))
  x[is.na(x)] <- na_result
  x
}
