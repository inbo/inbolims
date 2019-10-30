#'example_data <- data.frame(Sample = rep(c(paste0("S", sprintf("%02d", 1:7)), "blanco"), rep(10,8)),
#'                           Concentration = c(18.3,19.9,20.7,17.6,16.7,18.4,16.7,16.0,17.7,16.2,
#'                                            3.7,4.9,3.8,3.7,3.6,3.3,4.2,4.0,4.7,3.9,
#'                                             0.79,0.9,1.3,0.82,0.9,0.38,1.0,0.56,0.56,0.65,
#'                                             0.08,0.15,0.28,0.16,0.0,0.18,0.33,0.0,0.0,0.23,
#'                                             0.0,0.0,0.08,0.08,0.0,0.08,0.0,0.0,0.07,0.0,
#'                                             0.0,0.0,0.08,0.07,0,0,0,0,0,0,
#'                                             rep(0,20)))
#'ddPCR_qcplot(example_data, estim_formula = "connect")
#' 
#'





#' QC plot voor digital droplet pcr analyse
#'
#' @param data toestelfile die op zijn minst Sample en  Concentration bevat, waarbij de samples alfabetisch staan volgens  stijgende verdunning en er een blanco staal is. Ieder staal moet minstens enkele observaties bevatten
#' @param blanco de naam van de Sample die als blanco moet gerekend worden
#' @param LOD_min_positives minimum fractie van detecties om boven de LOD te komen. Standaard 1/20
#' @param LOD_manual een manuele LOD als je die zelf wil berekenen
#' @param verdunningsfactor hoeveel keer wordt er extra verdund van staal tot staal. Standaard 5
#' @param LOQ_slope_tolerance  Bij weinig verdunning verwacht je de helling dichtbij 1, maar hoe meer verdunt hoe lager deze hellling. Deze parameter bevat de limiet voor wat nog als lineair bereik gerekend wordt. Standaard is dit op 0.60 gezet.
#' @param log_add aangezien er logtransformaties gebruikt worden, en een 0 niet kan omgezet worden, bevat deze parameter wat er bijgeteld wordt voor alle concentraties. Standaard 0.0001
#' @param plot indien TRUE (standaard) dan wordt een ggplot getoont met de informatie, indien FALSE wordt enkel een data.frame teruggegeven waaruit je de plot zelf kan maken
#' @param print indien TRUE (standaard) dan worden LOD en LOQ getoond in de output
#' @param LOQ_manual manuele waarde voor LOQ als je die zelf wil berekenen
#' @param limit_color kleur voor de LOD en LOQ op de grafiek. Standaard "green4"
#' @param ... extra parameters die doorgegeven worden aan de functie ddPCR_calculate_fit waarvan in deze functie gebruikt wordt gemaakt
#' @return een ggplot object met de kwaliteitsfiguur of indien plot_it = FALSE een data.frame met alle nodige plotgegevens
#' @importFrom ggplot2 scale_color_manual scale_linetype_manual scale_shape_manual geom_text
#' @importFrom dplyr tibble
#' @importFrom stats sd
#' @export
ddPCR_qcplot <- function(data, 
                         verdunningsfactor = 5 ,
                         blanco = "blanco",
                         LOD_min_positives = 0.05,
                         LOD_manual = NULL, 
                         LOQ_slope_tolerance = 0.60,
                         LOQ_manual = NULL,
                         log_add = 1e-4,
                         plot = TRUE,
                         print = TRUE,
                         limit_color = "green4",
                         ...)
{
  
  ### Data voorbereiding
  samples <- sort(unique(data$Sample))
  non_blanks <- as.character(samples[!(samples %in% blanco)])
  verdunningen <- c(verdunningsfactor^((1:length(non_blanks)) - 1), Inf) #Inf voor verdunning blanco
  
  
  dfObs <- data %>%
    mutate(Sample = factor(.data$Sample, levels = c(non_blanks, blanco)))
  
  dfNoZero <- dfObs %>% 
    filter(.data$Concentration > 0)
  
  ### Berekenen van statistieken per staal (dus over de replicates heen)
  smrydata <- dfObs %>% 
    select(.data$Sample, .data$Concentration) %>%
    group_by(.data$Sample) %>%
    summarise(Mean_conc = mean(.data$Concentration),
              SD_conc = sd(.data$Concentration),
              Mean_conc_nozero = mean(.data$Concentration[.data$Concentration > 0]),
              SD_conc_nozero = sd(.data$Concentration[.data$Concentration > 0]),
              Aantal_pos = sum(.data$Concentration > 0),
              Fractie_pos = .data$Aantal_pos / n(),
              Fractie_within_1s = sum(.data$Concentration >= .data$Mean_conc_nozero - .data$SD_conc_nozero & 
                                        .data$Concentration <= .data$Mean_conc_nozero + .data$SD_conc_nozero) / .data$Aantal_pos) %>%
    mutate(Verdunning = verdunningen,
           Theoretical_conc = max(.data$Mean_conc) / .data$Verdunning)
  
  
  ### Berekenen van LOD (smrydata, alle waarden, originele schaal)
  if (!is.null( LOD_manual)) {
    dfLOD <- data.frame(Sample = NA, x = LOD_manual)
  } else {
    dfLOD <- smrydata %>% 
    select(.data$Sample, .data$Theoretical_conc, .data$Mean_conc_nozero, .data$Fractie_pos) %>%
    filter(.data$Fractie_pos >= LOD_min_positives) %>% 
    arrange(.data$Theoretical_conc) %>%
    transmute(Sample = .data$Sample, 
              LOD_theo = .data$Theoretical_conc,
              log10_LOD_theo = log10(.data$LOD_theo),
              LOD_meas = .data$Mean_conc_nozero,
              log10_LOD_meas = log10(.data$LOD_meas)) %>%
    slice(1)
  }
  
  ###BEREKENING LOQ op basis van een schattingsmethode gebaseerd op het log10-log10 buigpunt
  dfLOQcalc <- ddPCR_calculate_fit(x = log10(dfNoZero$Theoretical_conc), 
                                   y = log10(dfNoZero$Concentration), 
                               ...)
  if (!is.null(LOQ_manual)) {
    dfLOQ <- tibble(x = LOQ_manual)
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
 
  ###Print de resultaten
  if (print) {
    cat("Samenvattende tabel:\n\n")
    print(smrydata %>% select(.data$Sample, .data$Verdunning, Theo_conc = .data$Theoretical_conc,
                              Measured = .data$Mean_conc, Measured_nozero = .data$Mean_conc_nozero, 
                              SD_nozero = .data$SD_conc_nozero, .data$Fractie_pos, .data$Fractie_within_1s))
    cat("\nLOD: ", dfLOD$LOD_meas, "\n")
    cat("LOQ: ", dfLOQ$LOQ_meas, "\n")
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
    mutate(LOD = dfLOD$LOD_meas,
           LOQ = dfLOQ$LOQ_meas) %>% 
    transmute(.data$Sample, .data$which_data,
              log10Concentration = log10(.data$Concentration + log_add),
              log10Theoretical = log10(.data$Theoretical_conc + log_add),
              log10LOD = log10(.data$LOD + log_add),
              log10LOQ = log10(.data$LOQ + log_add))
  
  if (!plot) {
    return(dfPlot)
  }
  
  maxconc <- max(dfPlot$log10Concentration, na.rm = TRUE)
  
  ggplot(filter(dfPlot, .data$log10Theoretical > log10(log_add)), 
         aes(x = .data$log10Theoretical, y = .data$log10Concentration, 
             color = .data$which_data, shape = .data$which_data, linetype = .data$which_data)) + 
    geom_point() + 
    geom_line() + 
    scale_color_manual(name = "", values = c("black", "red", "blue")) + 
    scale_linetype_manual(name = "", values = c("solid","blank","solid")) + 
    scale_shape_manual(name = "", values = c(-1, 1, 4)) + 
    geom_vline(xintercept = dfPlot$log10LOD[1], color = limit_color, linetype = 2) + 
    geom_vline(xintercept = dfPlot$log10LOQ[1], color = limit_color, linetype = 2) + 
    geom_text(aes(x = .data$log10LOD[1], y = maxconc, label = "LOD"), hjust = 1.1, color = limit_color) + 
    geom_text(aes(x = .data$log10LOQ[1], y = maxconc, label = "LOQ"), hjust = -0.1, color = limit_color) + 
    xlab(expression(paste(log[10], "(calculated DNA concentration)"))) + 
    ylab(expression(paste(log[10], "(measured DNA concentration)")))
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
    
  } else if (estim_formula == "loess"){
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
      ifelse ((x-b) >= 0, b + x, a * (exp(x-b)-1))  
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






