#' Title
#'
#' @param data lims report data (from lims_report_data)
#' @param plot if NULL no plot is created, if "boxplot" a boxplot is shown,
#' if "histogram" a histogram
#' @param log when plot, use the log-scale?
#' @importFrom ggplot2 scale_y_log10 geom_boxplot facet_wrap ggplot aes
#' @importFrom ggplot2 element_text element_blank theme xlab rel geom_histogram
#' @importFrom stats quantile
#'
#' @return list with measured parameters and some base statistics
#' @export
#'
#' @examples
#' \dontrun{
#' view(lims_result_statistics(reportdata))
#' }
lims_result_statistics <- function(data, plot = "boxplot", log = TRUE) {
  cols_to_check <- c("LimsAnalyseNaam", "AnalyseNaam", "Component", "Eenheid")
  if (!("NumeriekeWaarde" %in% colnames(data))) {
    data$NumeriekeWaarde <- as.numeric(data$WaardeRuw)
  }

  rv <- data %>%
    group_by(across(all_of(intersect(cols_to_check, colnames(.))))) %>%
    summarise(
      min = min(.data$NumeriekeWaarde, na.rm = TRUE),
      q25 = quantile(.data$NumeriekeWaarde, 0.25, na.rm = TRUE),
      med = quantile(.data$NumeriekeWaarde, 0.50, na.rm = TRUE),
      avg = round(mean(.data$NumeriekeWaarde, na.rm = TRUE), 5),
      q75 = quantile(.data$NumeriekeWaarde, 0.75, na.rm = TRUE),
      max = max(.data$NumeriekeWaarde, na.rm = TRUE),
      aantal = n(),
      aantalstalen = n_distinct(.data$OrigineelStaal),
      aantalmissend = sum(is.na(.data$NumeriekeWaarde)),
    )
  if (length(plot)) {
    ggobj <- ggplot(
      data,
      aes(
        x = substring(
          .data$Sleutel, 1,
          nchar(.data$Sleutel) - 6
        ),
        y = .data$NumeriekeWaarde
      )
    )
    if (plot == "boxplot") {
      ggobj <- ggobj + geom_boxplot()
    } else if (plot == "histogram") {
      ggobj <- ggobj +
        geom_histogram(aes(x = .data$NumeriekeWaarde), inherit.aes = FALSE)
    } else {
      stop("no valid plot type")
    }
    ggobj <- ggobj +
      facet_wrap(~ substring(.data$Sleutel, 1, nchar(.data$Sleutel) - 6),
        scales = "free"
      ) +
      xlab("Analysecomponent") +
      theme(
        strip.text = element_blank(),
        axis.text.x = element_text(size = rel(0.8))
      )
    if (plot == "histogram") {
      ggobj <- ggobj +
        theme(strip.text = element_text(size = rel(0.5)))
    }
    if (log) ggobj <- ggobj + scale_y_log10()
    print(ggobj)
  }
  rv
}
