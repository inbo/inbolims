
#' Boxplot van het resultaat
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
#' @param ... parameters die aan opgeroepen subfuncties kunnen meegegeven worden
#' @importFrom ggplot2 ggplot 
#' @return gg boxplot
#' @export
lims_boxplot <- function(data, group = NULL, xlab, ylab, file = NA, show = TRUE, suffix = "", dpi = 300, width = 7, height = 5, ...) {
  if (is.null(group)) {
    p <- ggplot(data, aes_string(x = "", y = "ResultNumeric"))
    
  } else {
    p <- ggplot(data, aes_string(x = group, y = "ResultNumeric"))    
  }
  
  p <- p + geom_boxplot()
  
  if (!missing(xlab)) {
    p <- p + xlab(xlab)
  }
  if (!missing(ylab)) {
    p <- p + ylab(ylab)
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

#####################################################################

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
#' @importFrom ggplot2 ggplot geom_vline
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

#####################################################################

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
#' @importFrom ggplot2 ggplot geom_histogram
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

########################################################################


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

##################################################################################


#' Scatterplot van het resultaat ten opzichte van de tijdsas
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
#' @param ... parameters die aan opgeroepen subfuncties kunnen meegegeven worden
#' @importFrom ggplot2 ggplot geom_point
#' @return gg scatterplot
#' @export
lims_scatter_date <- function(data, group = NULL, xlab, ylab, clab, file = NA, show = TRUE, suffix = "", dpi = 300, width = 7, height = 5, ...){
  if (is.null(group)) {
    p <- ggplot(data, aes_string(x = "AnalysisDate", y = "ResultNumeric"))
    
  } else {
    p <- ggplot(data, aes_string(x = "AnalysisDate", y = "ResultNumeric", color  = group))    
  }
  p <- p + geom_point()
  
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

###################################################################################


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
#' @importFrom ggplot2 ggplot geom_point
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
