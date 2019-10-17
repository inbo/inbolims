
#' Kwantielberekening
#'
#' @param data lims data waarvoor de kwantielen berekend moeten worden
#' @param group geef hier de kolomnaam in die dient om de groepen te selecteren waarvoor de kwantielen berekend worden
#' @param probs de probabiliteiten waarvoor de kwantielen moeten berekend worden. Een vector van elementen tussen 0 en 1
#' @param file nog niet gebruikt. Het bestand waarnaar de tabel geschreven wordt
#' @param ... extra parameters die aan de kwanitelfunctie kunnen meegegeven worden
#'
#' @return tibbble met de waarden horende bij ieder kwantiel. Indien groepen gekozen, een aparte kolom per groep
#' @export
lims_quantile <- function(data, group = NULL, probs = c(0,0.025,0.25,0.50,0.75,0.975,1), file = NA, ...) {
  if (is.null(group)) {
    sumdat <- data %>%
      do({
        enframe(quantile(.data$ResultNumeric, probs = probs))
      })
    
  } else {
    sumdat <- data %>%
      group_by_(group) %>% 
      do({
        enframe(quantile(.data$ResultNumeric, probs = probs))
      }) %>%
      mutate(name = as.numeric(substring(.data$name, 1, nchar(.data$name) - 1))) %>%
      spread_(key_col = group, value_col = "value") %>%
      mutate(name = paste0(.data$name, "%"))
  }
  sumdat
}

