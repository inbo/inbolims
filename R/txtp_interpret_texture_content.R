
#' Interpreteer de geparste textuurfile
#'
#' @param textuurdata geparste textuurdata
#' @param verbose moet output getoond worden tijdens de routine
#' @param digits aantal digits voor de waarde en sd
#' @import dplyr
#' @importFrom tidyr pivot_longer separate
#' @import magrittr
#'
#' @return tidy dataset met alle gegevens
#' @export
#'
interprete_texture_content <-
  function(textuurdata, verbose = TRUE, digits = 3) {
  textuur_long <- textuurdata %>%
    pivot_longer(cols = -ends_with("boundary")) %>%
    separate(.data$name, into = c("sample", "param"), sep = "___")

  textuur_wide <- textuur_long %>%
    pivot_wider(
      id_cols = c(
        .data$lower_boundary,
        .data$upper_boundary,
        .data$sample
      ),
      names_from = .data$param,
      values_from = .data$value
    ) %>%
    mutate(
      value = round(.data$value, digits),
      sd = round(.data$UCL1S - .data$value, digits),
      lower_boundary = round(.data$lower_boundary, 2),
      upper_boundary = round(.data$upper_boundary, 2)
    ) %>%
    select(c(1:4, 7, 5, 6))

  if (verbose) {
    cat(
      "\nOK\ndataset geinterpreteerd:\n",
      "dimensies: ", nrow(textuur_wide), "x", ncol(textuur_wide), "\n",
      "aantal stalen: ", length(unique(textuur_wide %>% pull(sample))), "\n"
    )
  }
  return(textuur_wide)
}

####################
