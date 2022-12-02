
#' QC plot voor digital droplet pcr analyse
#'
#' @param data toestelfile die op zijn minst Sample, Concentration en Threshold bevat, waarbij de samples alfabetisch staan volgens  stijgende verdunning en er een blanco staal is. Ieder staal moet minstens enkele observaties bevatten
#' @param blank de naam van de Sample die als blanco moet gerekend worden
#' @param limit_type welke waarde moet gebruikt worden om de LOD en LOQ op de grafiek te zetten. De theoretische concenteratie ("theo") of de gemeten concentratie ("meas")
#' @param LOD_min_positives minimum fractie van detecties om boven de LOD te komen. Standaard 1/20
#' @param LOD_manual een manuele LOD als je die zelf wil berekenen
#' @param dilution_factor hoeveel keer wordt er extra verdund van staal tot staal. Standaard 5
#' @param dilution_base hoeveel keer is het eerste staal verdund. Standaard 20
#' @param LOQ_slope_tolerance  Bij weinig verdunning verwacht je de helling dichtbij 1, maar hoe meer verdunt hoe lager deze hellling. Deze parameter bevat de limiet voor wat nog als lineair bereik gerekend wordt. Standaard is dit op 0.60 gezet.
#' @param log_add aangezien er logtransformaties gebruikt worden, en een 0 niet kan omgezet worden, bevat deze parameter wat er bijgeteld wordt voor alle concentraties. Standaard 0.0001
#' @param LOQ_manual manuele waarde voor LOQ als je die zelf wil berekenen
#' @param ... extra parameters die doorgegeven worden aan de functie \code{\link{ddPCR_calculate_fit}} waarvan in deze functie gebruikt wordt gemaakt
#' @return een ggplot object met de kwaliteitsfiguur of indien plot_it = FALSE een data.frame met alle nodige plotgegevens
#' @importFrom ggplot2 scale_color_manual scale_linetype_manual scale_shape_manual geom_text
#' @importFrom dplyr tibble
#' @importFrom stats sd
#' @importFrom tidyr gather spread
#' @importFrom ggplot2 ggplot aes geom_point geom_line
#' @export
#' @examples 
#' \dontrun{
#' library(ggplot2)
#' fpath <- system.file("extdata", "ddPCR_voorbeelddata.csv", package="inbolims")
#' example_data <- read.csv2(fpath, stringsAsFactors = FALSE)
#' plotdata <- ddPCR_qc_calc(example_data, estim_formula = "connect")
#' ggplot(plotdata)
#' summary(plotdata)
#' }
ddPCR_qc_calc <- function(data, 
                          dilution_base = 20,
                          dilution_factor = 5,
                          blank = "blanco",
                          limit_type = "theo",
                          LOD_min_positives = 0.05,
                          LOD_manual = NULL, 
                          LOQ_slope_tolerance = 0.60,
                          LOQ_manual = NULL,
                          log_add = 1e-4,
                          ...)
{
  # Data voorbereiding
  samples <- unique(data$Sample) #volgorde van de file blijft behouden
  first_sample <- as.character(samples[samples != blank][1])
  other_samples <- as.character(samples[!(as.character(samples) %in% c(blank, first_sample))])
  non_blanks <- c(first_sample, other_samples)
  verdunningen <- dilution_base * c(dilution_factor ^ ((1:length(non_blanks)) - 1), Inf) #Inf voor verdunning blanco
  
  dfObs <- data %>%
    mutate(Sample = factor(.data$Sample, levels = c(non_blanks, blank)))
  
  ### Berekenen van statistieken per staal (dus over de replicates heen)
  
  smrydata <- dfObs %>% 
    select(.data$Sample, .data$Concentration) %>%
    group_by(.data$Sample) %>%
    summarise(Mean_conc = mean(.data$Concentration),
              SD_conc = sd(.data$Concentration),
              Mean_conc_nozero = mean(.data$Concentration[.data$Concentration > 0]),
              SD_conc_nozero = sd(.data$Concentration[.data$Concentration > 0]),
              N_pos = sum(.data$Concentration > 0),
              Frac_pos = .data$N_pos / n(),
              LCL_1s = .data$Mean_conc_nozero - .data$SD_conc_nozero,
              UCL_1s = .data$Mean_conc_nozero - .data$SD_conc_nozero,
              Sum_in_1s = sum(.data$Concentration >=  .data$LCL_1s & .data$Concentration <= .data$UCL_1s)) %>%
    mutate(   Frac_in_1s = .data$Sum_in_1s / .data$N_pos,
              Dilution  = verdunningen,
              Theoretical_conc = max(.data$Mean_conc) / .data$Dilution * dilution_base)
           
  
  ### Berekenen van LOD (smrydata, alle waarden, originele schaal)
  if (!is.null( LOD_manual)) {
    dfLOD <- data.frame(Sample = NA, LOD_theo = LOD_manual, LOD_meas = LOD_manual)
  } else {
    dfLOD <- smrydata %>% 
    select(.data$Sample, .data$Theoretical_conc, .data$Mean_conc_nozero, .data$Frac_pos) %>%
    filter(.data$Frac_pos >= LOD_min_positives) %>% 
    arrange(.data$Theoretical_conc) %>%
    transmute(Sample = .data$Sample, 
              LOD_theo = .data$Theoretical_conc,
              log10_LOD_theo = log10(.data$LOD_theo),
              LOD_meas = .data$Mean_conc_nozero,
              log10_LOD_meas = log10(.data$LOD_meas)) %>%
    slice(1)
  }
  
  ###Threshold
  
  threshold <- mean(data$Threshold)
  
  ###BEREKENING LOQ op basis van een schattingsmethode gebaseerd op het log10-log10 buigpunt
  
  dfNoZero <- dfObs %>% 
    filter(.data$Concentration > 0) %>% 
    left_join(smrydata %>%
                select(.data$Sample, .data$Theoretical_conc), 
                by = "Sample")
  
  dfLOQcalc <- ddPCR_calculate_fit(x = log10(dfNoZero$Theoretical_conc), 
                                   y = log10(dfNoZero$Concentration), 
                               ...)
  if (!is.null(LOQ_manual)) {
    dfLOQ <- tibble(LOQ_theo = LOQ_manual, LOQ_meas = LOQ_manual)
  } else {
    check_theo <- log10(dfNoZero$Theoretical_conc)
    dfLOQ <- 
      filter(dfLOQcalc, .data$x %in% check_theo , !duplicated(.data$x)) %>%
      mutate(Slope = c((.data$y[-length(.data$y)] - .data$y[-1]) / (.data$x[-length(.data$x)] - .data$x[-1]), NA)) %>%
      summarise(log10_LOQ_theo = .data$x[.data$Slope >= LOQ_slope_tolerance][1], 
                log10_LOQ_meas = .data$y[.data$Slope >= LOQ_slope_tolerance][1]) %>% 
      mutate( LOQ_theo = 10^(.data$log10_LOQ_theo),
              LOQ_meas = 10^(.data$log10_LOQ_meas))
  }
 
  ###Prepareer data voor een grote plotdataset
  
  #zorg dat dfObs en dfLOQ hetzelfde aantal rijen hebben
  maxrows <- max(nrow(dfObs), nrow(dfLOQcalc))
  dfPlotObs <- left_join(dfObs, smrydata %>% select(.data$Theoretical_conc, .data$Sample), by = "Sample")
  if (nrow(dfObs) < maxrows) {
    tmp <- as.data.frame(matrix(nrow = maxrows - nrow(dfObs), ncol = ncol(dfObs), data = NA))
    names(tmp) <- names(dfObs)
    dfObs <- bind_rows(dfObs, tmp)
  }
  

  dfPlot <- 
    bind_rows(dfPlotObs %>% mutate(which_data = "Obs"), 
              smrydata %>% 
                transmute(Concentration = .data$Theoretical_conc, Theoretical_conc = .data$Theoretical_conc) %>% 
                mutate(which_data = "Theo"),
              dfLOQcalc %>% transmute(Theoretical_conc = 10^(.data$x), Concentration = 10^(.data$y), which_data = "Fit")) %>%
    mutate(LOD = ifelse(limit_type == "theo", dfLOD$LOD_theo, dfLOD$LOD_meas),
           LOQ = ifelse(limit_type == "theo", dfLOQ$LOQ_theo, dfLOQ$LOQ_meas)) %>% 
    transmute(.data$Sample, .data$which_data,
              log10Concentration = log10(.data$Concentration + log_add),
              log10Theoretical = log10(.data$Theoretical_conc + log_add),
              log10LOD = log10(.data$LOD + log_add),
              log10LOQ = log10(.data$LOQ + log_add),
              Threshold = threshold)
  
  summary_out <- smrydata %>% 
    select(Staal = .data$Sample, Verdunning = .data$Dilution, Theo = .data$Theoretical_conc,
           Meting = .data$Mean_conc, SD = .data$SD_conc, 
           Meting_nietnul = .data$Mean_conc_nozero, SD_nietnul = .data$SD_conc_nozero,
           Frac_pos = .data$Frac_pos) %>% 
    gather(key = "Statistiek", value = "Waarde", -.data$Staal) %>% 
    mutate(Statistiek = factor(.data$Statistiek, levels = unique(.data$Statistiek))) %>% 
    spread(key = .data$Staal, value = .data$Waarde) %>% 
    mutate(Statistiek = as.character(.data$Statistiek))
  
  summary_generic <- tibble(Statistiek = c("Threshold", "LOD_theo", "LOQ_theo", 
                                               "LOD_meting", "LOQ_meting"),
                                Waarde = c(threshold,  dfLOD$LOD_theo, dfLOQ$LOQ_theo, 
                                           dfLOD$LOD_meas, dfLOQ$LOQ_meas)) 
  summary_out <- bind_rows(summary_out, summary_generic) %>% 
     select(1, ncol(.), 2:(ncol(.) - 1)) %>% 
     slice(c(8, 1:7, 9:12))
    
  
  rvlist <- list(plotdata = dfPlot, summary = summary_out)
  class(rvlist) <- c("ddPCR", "list")
  return(rvlist)
}


###############################################################################


#' Samenvattende QC statistieken voor de digital droplet PCR outputfile
#'
#' @param object data die gegenereerd wordt door de functie ddPCR_qc_calc
#' @param ... voorlopig niet gebruikt
#' @return data.frame met de samenvattende statistieken
#' @export
#'
summary.ddPCR <- function(object, ...) {
  print(object$summary)
  
}


################################################################################


#' GGplot of ddPCR data (generated by ddPCR_qcalc)
#'
#' @param data dataset generated from ddPCR_qccalc. Must have at least these columns:
#' \itemize{
#'   \item log10Concentration
#'   \item log10Theoretical
#'   \item which_data groeperingsvariabele bestaande uit 3 niveau's. De naam is vrij te kiezen, maar de alfabetische volgorde zal bepalen welke kleur en lijntype eraan gematcht worden
#'   \item log10LOQ
#'   \item log10LOD
#' }
#' @param mapping wordt niet gebruikt in dit geval, maar is nodig om de ggplot functie te kunnen gebruiken
#' @param colors vector van lengte 4 met de kleuren van de verschillende plotdelen die bepaald worden door de volgorde van de niveau's in which_data
#' \itemize{
#'   \item kleur 1 is het kleur voor de eerste groep (standaard de gefitte lijn)
#'   \item kleur 2 is het kleur voor de tweede groep (standaard de observaties)
#'   \item kleur 3 is het kleur voor de derde groep (standaard de theoretische concentratie)
#'   \item kleur 4 is het kleur voor de LOD en LOQ lijnen
#' }
#' @param shapes numerieke vector van lengte 3 die de symbolen overeenkomstig de groepen in which_data definieert. Bij een negatief getal wordt het punt niet getoond, symbool met nummer 1 is een lege bol, symbool met nummer 4 is een kruisje. Standaard wordt de eerste shape negatief gezet, omdat dit standaard overeenkomt met de gefitte lijn, waarvoor we de punten niet willen zien
#' 
#' @param linetypes vector in dezelfde volgorde als colors die de lijntypes definieert. Voor een lijn die niet getekend moet worden geef je de waarde "blank", een volle lijn is "solid", een stippelliljn "dashed" en dan heb je nog heel wat andere types die je kan terugvinden via google "ggplot linetypes"
#' @param LOD_label tekst die bij de LOD lijn moet gezet worden
#' @param LOQ_label tekst die bij de LOQ lijn moet gezet worden
#' @param xlab astitel voor de x-as
#' @param ylab astitel voor de y-as
#' @importFrom ggplot2 geom_vline
#' @param log_add hoeveel wordt bij de concentratie geteld om de log te nemen (log10(variabele + log_add)). Deze waarde wordt gebruikt om enkel de datarijen te selecteren met een concentratie groter dan 0, en moet overeenstemmen met de log_add in ddPCR_qc_calc
#' @return ggplot object met 5 lagen: de geobserveerde waarden, de fit door de niet-nulwaarden van de geobserveerde waarden, de theoretische waarden, een vertikale lijn voor LOD, een vertikale lijn voor LOQ
#' @export
ggplot.ddPCR <- function(data, mapping = NULL,  
                         colors = c("black", "red", "blue", "green4"),
                         shapes = c(-1, 1, 4),
                         linetypes = c("solid", "blank", "solid", "dashed"),
                         LOD_label = "LOD", 
                         LOQ_label = "LOQ", 
                         xlab = expression(paste(log[10], "(calculated DNA concentration)")),
                         ylab = expression(paste(log[10], "(measured DNA concentration)")),
                         log_add = 1e-4) {
  dfplot <- data[["plotdata"]]
  maxconc <- max(dfplot$log10Concentration, na.rm = TRUE)
  ggplot(filter(dfplot, .data$log10Theoretical > log10(log_add)), 
         aes(x = .data$log10Theoretical, y = .data$log10Concentration, 
             color = .data$which_data, shape = .data$which_data, linetype = .data$which_data)) + 
    geom_point() + 
    geom_line() + 
    scale_color_manual(name = "", values = colors[1:3]) + 
    scale_linetype_manual(name = "", values = linetypes) + 
    scale_shape_manual(name = "", values = shapes) + 
    geom_vline(xintercept = dfplot$log10LOD[1], color = colors[4], linetype = 2) + 
    geom_vline(xintercept = dfplot$log10LOQ[1], color = colors[4], linetype = 2) + 
    geom_text(aes(x = dfplot$log10LOD[1], y = maxconc, label = LOD_label), 
              hjust = 1.1, color = colors[4]) + 
    geom_text(aes(x = dfplot$log10LOQ[1], y = maxconc, label = LOQ_label), 
              hjust = -0.1, color = colors[4]) + 
    xlab(xlab) + 
    ylab(ylab)
}





#' Berekeningen van een trendlijn in de qc grafiek voor digital droplet PCR
#'
#' @param x vector met de x-coordinaten in de grafiek
#' @param y vector met de y-coordinaten in de grafiek
#' @param start startwaarden voor het berekenen van de verschillende formules. Vector met 3 numerieke elementen.
#' @param estim_formula welke formule gebruikt wordt. Keuze tussen connect, growth, loess, polynomial, elu en sigmoid. Standaard wordt connect gebruikt, welke gewoon de gemiddelde respons berekend bij elke x waarde en deze als een lijn verbindt, wat het voordeel heeft dat er geen aannames moeten gemaakt worden, maar als nadeel dat de curve er niet altijd mooi uitziet
#' @param loess_span waarde die specifiek aan de loess functie wordt doorgegeven als f (smoother span)
#' @importFrom stats optim lowess
#' @return een dataset met een x-waarde en een y-waarde. De x is ofwel de echte x-waarden (voor bv connect), maar gebruikt de range van x opgedeeld in 100 stukjes voor een exponentiële functie
#' @export
#'
ddPCR_calculate_fit <- function(x, y, start = c('a' = 1, 'b' = 1, 'c' = 1), estim_formula = "connect", loess_span = 0.5){
  
  find_best_curve <- function(x, y, params, formula) {
    a <- params[1]
    b <- params[2]
    c <- params[3]
    expected = formula(x, a, b, c)
    sum((y - expected)^2) * ifelse(any(c(abs(a), abs(b), abs(c)) > 5), abs(a) + abs(b) + abs(c), 1)
  }
  
  if (estim_formula == "connect") {
    meany <- tapply(y, x, mean)
    xvals <- tapply(x, x, mean)
    xs <- xvals
    ys <- meany
    
  } else if (estim_formula == "growth")  {
    formula <- function(x, a, b, c) {
      a * (1 + b * exp(x))^c
    }
    opt <- optim(par = start, fn = find_best_curve, x = x, y = y, formula = estim_formula)
    print(opt$par)
    xs <- sort(c(x, seq(min(x), max(x), length = 100)))
    ys <- formula(xs, a = opt$par['a'], b = opt$par['b'], c = opt$par['c'])
    
  } else if (estim_formula == "loess") {
    mod <- lowess(x, y, f = loess_span)
    xs <- mod$x
    ys <- mod$y
    
  } else if (estim_formula == "polynomial") {
    formula <- function(x, a, b, c) {
      a * x^4 + b*x + c
    }
    opt <- optim(par = start, fn =  find_best_curve, x = x, y = y, formula = estim_formula)
    print(opt$par)
    xs <- sort(c(x, seq(min(x), max(x), length = 100)))
    ys <- formula(xs, a = opt$par['a'], b = opt$par['b'], c = opt$par['c'])
    
  } else if (estim_formula == "elu") {
    formula <- function(x, a, b, c){
      ifelse((x - b) >= 0, b + x, a * (exp(x - b) - 1))  
    }
    opt <- optim(par = start, fn = find_best_curve, x = x, y = y, formula = estim_formula)
    print(opt$par)
    xs <- sort(c(x, seq(min(x), max(x), length = 100)))
    ys <- formula(xs, a = opt$par['a'], b = opt$par['b'], c = opt$par['c'])
    
  } else if (estim_formula == "sigmoid") {
    formula <- function(x, a, b, c)  {
      c + a / (1 + exp(-x + b))
    }    
    opt <- optim(par = start, fn = find_best_curve, x = x, y = y, formula = estim_formula)
    print(opt$par)
    xs <- sort(c(x, seq(min(x), max(x), length = 100)))
    ys <- formula(xs, a = opt$par['a'], b = opt$par['b'], c = opt$par['c'])
  } else {
    xs <- x
    ys <- y
  }
  tibble(x = xs, y = ys)
}


