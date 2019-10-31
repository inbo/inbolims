
#' Scatterplot van het resultaat van de laatste punten
#'
#' @param data lims dataset to plot
#' @param group charactervariabele die de groeperingskolom bepaalt
#' @param xlab naam x-as
#' @param ylab naam y-as
#' @param clab naam van de kleur-as
#' @param file bestandsnaam waar de figuur wordt bewaard (in de werkdirectory)
#' @param show toon de figuur op het scherm
#' @param suffix korte naam voor de figuur, deze zal scatter_date_suffix.png noemen
#' @param dpi resolutie 
#' @param width breedte in inch
#' @param height  hoogte in inch
#' @param add_path verbind de punten met een lijn
#' @param ... parameters die aan opgeroepen subfuncties kunnen meegegeven worden
#' @importFrom ggplot2 aes_string geom_boxplot geom_density geom_vline ggsave facet_wrap geom_histogram geom_path scale_color_discrete
#' @importFrom tidyr spread_
#' @importFrom stats quantile
#' @importFrom tibble enframe
#' @return gg scatterplot
#' @export
lims_scatter_nr <- function(data, group = NULL, xlab, ylab, clab, file = NA, show = TRUE, suffix = "", dpi = 300, width = 7, height = 5, add_path = TRUE, ...){
  
  data <- data %>%
    arrange(.data$AnalysisDate) %>% 
    mutate(Nr = 1:n())
  
  if (is.null(group)) {
    p <- ggplot(data, aes_string(x = "Nr", y = "ResultNumeric"))
    
  } else {
    p <- ggplot(data, aes_string(x = "Nr", y = "ResultNumeric", color  = group))    
  }
  p <- p + geom_point()
  
  if (add_path) {
    p <- p + geom_path()
  }
  
  if (!missing(xlab)) {
    p <- p + xlab(xlab)
  }
  if (!missing(ylab)) {
    p <- p + ylab(ylab)
  }
  if (!missing(clab)) {
    p <- p + scale_color_discrete(name = clab)
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
