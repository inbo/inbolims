
#' Densityplot van het lims resultaat
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
#' @param quantiles indien TRUE worden de 2.5, 25, 50, 75 en 97.5 percentielen als lijnen weergegeven
#' @param ... parameters die aan geom_density kunnen meegegeven worden
#'
#' @return gg densityplot
#' @export
lims_density <- function(data, group = NULL, xlab, ylab, file = NA, show = TRUE, suffix = "", dpi = 300, width = 7, height = 5, ncol = 1, quantiles = TRUE, ...) {
  
  probs <- c(0.025,0.25,0.50,0.75,0.975)
  if (is.null(group)) {
    p <- ggplot(data, aes_string(x = "ResultNumeric"))
    
  } else {
    data$facet <- data[[group]]
    p <- ggplot(data, aes_string(x = "ResultNumeric")) + facet_wrap(~facet, ncol = ncol)    
  }
  
  p <- p + geom_density(...)
  
  if (!missing(xlab)) {
    p <- p + xlab(xlab)
  }
  if (quantiles & is.null(group)) {
    quants <- quantile(data$ResultNumeric, probs = probs, na.rm = TRUE)
    print(quants)
    p <- p + geom_vline(xintercept = quants, color = c("red", "gold", "green4", "gold", "red"))
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
