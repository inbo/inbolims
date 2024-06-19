#' Kruistabel naar csv wegschtrijven met toevoeging header
#'
#' @param data kruistabeldata uit lims_report_xtab
#' @param path pad waar de file geschreven moet worden
#' @return csv file
#' @export
#'
lims_report_export <- function(data, path) {
  namen <- tibble(Naam = colnames(data))

  header <- separate(namen,
    col = .data$Naam, sep = "___",
    into = c("Analyse", "Component", "Iteratie"),
    fill = "right"
  )
  header$COMBI <- namen
  newdata <- as.data.frame(t(header))
  newdata <- newdata %>%
    mutate(across(.cols = everything(), .fns = as.character))
  colnames(newdata) <- make.names(header %>% pull(.data$COMBI) %>% unlist())

  datach <- as.data.frame(data) # for loop (accross probleem)
  for (i in seq_len(ncol(datach))) {
    datach[, i] <- as.character(datach[, i])
    datach[, i] <- gsub("\\.", ",", datach[, i])
  }

  newdata <- bind_rows(newdata, datach)
  newdata[is.na(newdata)] <- ""
  write_excel_csv2(newdata, file = path) # niet csv2 want alles is character
}
