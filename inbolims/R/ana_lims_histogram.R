#' Histogram van het resultaat
#'
#' @param data lims dataset to plot
#' @param group charactervariabele die de groeperingskolom bepaalt
#' @param xlab naam x-as
#' @param ylab naam y-as
#' @param file bestandsnaam waar de figuur wordt bewaard (in de werkdirectory)
#' @param show toon de figuur op het scherm
#' @param suffix korte naam voor de figuur, deze zal scatter_date_suffix.png noemen
#' @param dpi resolutie 
#' @param width breedte in inch
#' @param height  hoogte in inch
#' @param ncol Als een groepvariabele gedefinieerd is, in hoeveel kolommen moet de figuren getoond worden
#' @param ... andere variabelen zoals bins of breaks die gebruikt worden door geom_histogram
#' @return gg histogram
#' @export
lims_histogram <- function(data, group = NULL, xlab, ylab, file = NA, show = TRUE, suffix = "", dpi = 300, width = 7, height = 5, ncol = 1, ...) {
  if (is.null(group)) {
    p <- ggplot(data, aes_string(x = "ResultNumeric"))
    
  } else {
    data$facet <- data[[group]]
    p <- ggplot(data, aes_string(x = "ResultNumeric")) + facet_wrap(~facet, ncol = ncol)    
  }
  
  p <- p + geom_histogram(...)
  
  if (!missing(xlab)) {
    p <- p + xlab(xlab)
  }
  
  if (show) {
    print(p)
  }
  
  if (!is.null(file)) {
    if (!is.na(file)) {
      ggsave(p, filename = file)
    } else {
      ggsave(p, filename = paste0("scatter_date_", suffix, ".png"), dpi = dpi, width = width, height = height)
    }
  }
  
  p
}

