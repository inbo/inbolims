#' Haal resultaten uit het LIMS datawarehouse
#'
#' @param conn connectiestring naar de database. Wordt aangemaakt via dwh_connect() of manueel
#' @param config character string of data.frame die aangeeft welke configuratie moet gebruikt worden (zie \link{limsdwh_report_query})
#' @param version indien NULL wordt de laatste versie van het opgegeven reporttype gebruikt, indien hier gespecifieerd wordt de gekozen versie gebruikt.
#' @param project naam van een project, bv "I-19W001-01", of meerdere namen als vector bv. c("I-19W001-01", "I-19W001-02"). Als derde mogelijkheid kan je ook 1 project selecteren met SQL joker karakters als \% en _. Als je alle projecten van contract 19W001 wil kan je dit als volgt specificeren: "\%19W001\%" 
#' @param extra eventueel extra criteria toe te voegen aan de SQL, moet beginnen met 'and '
#' @param ... other arguments, passed to limsdwh_report_convert_config_to_query like "show_query"
#' @return data.frame uit het lims datawarehouse met alle analyseresultaten die gerapporteerd worden door het labo
#' @export
#' @examples 
#' \dontrun{
#' conn <- limsdwh_connect()
#' reportdata <- limsdwh_report(conn, project = 'I-17W003-01')
#' }
#'
limsdwh_report <- function(conn, 
                           config = "DEFAULT",
                           version = NULL,
                           project = NULL, 
                           extra = NULL,
                           ...) {

  qry <- limsdwh_report_query(project, extra, config, version, ...)
  RODBC::sqlQuery(conn, qry)
}


#######

#' Convert config file to sql query
#'
#' @param config naam van de configuratie, of een dataset in de vorm van de configuratiedataset (kolommen ConfigName, Version, Order, Table, Field, ReportableName, FieldType). De controlevelden OrigineelStaal (OriginalSample), TestReplicaat(TestReplicate), ResultaatReplicaat (ResultReplicate), LimsAnalysenaam (LimsAnalysisName) zijn altijd nodig, als je wil crosstabuleren later
#' @param version versienummer
#' @param project vector van projecten, of 1 project met de SQL jokercharacters \% en/of \_
#' @param extra eventueel extra criteria toe te voegen aan de SQL, moet beginnen met 'and '
#' @param include_control Niet gebruikt op dit moment. Indien FALSE worden enkel de velden met FieldType "R" in the config file getoond, Control fields are needed to make a crosstabulation of Result (Uses OriginalSample, TestReplicate, ResultReplicate en Analysis.)
#' @param show_query print de gevormde query op het scherm voor eventueel later gebruik of kleine aanpassingen
#' @importFrom readr read_delim
#' @importFrom  dplyr filter mutate arrange
#' @return character string containing the query to pass to the datawarehouse
#' @export

limsdwh_report_query <- function(project, extra = '', 
                                 config = "DEFAULT", version = NULL, 
                                 include_control = TRUE, show_query = FALSE) {
  
  qryfile <- system.file("dwh_bevraging/basisquery_rapport.sql", package = "inbolims")
  #qryfile <- "inbolims/inst/dwh_bevraging/basisquery_rapport.sql"
  #qryfile <- "inbolims/inst/dwh_bevraging/basisquery_rapport_bouw.sql"
  if (qryfile == "") {
    stop("qryfile dwh_bevraging/basisquery_rapport.sql niet gevonden in basispad inbolins library. Check of de installatie correct verlopen is")
  } else {
    qry <- readLines(qryfile)
  }
  
  ### Vind de gekozen config
  if (!inherits(x = config, what = "data.frame")) {
    configdatapath <- system.file("report_config", "ReportFieldsConfig.csv", package = "inbolims")
    if (length(configdatapath)) {
      configdata <- read_delim(configdatapath, delim = ";") %>%
        filter(.data$ConfigName == config)
      if (nrow(configdata) > 0) {
        if (is.null(version)) {
          version <- rev(sort(configdata$Version))[1]
        }
        configdata <- configdata %>% filter(.data$Version == version) %>%
          arrange(.data$Order)
      } else {
        warning("specified reporttype does not exist. Default configuration used instead")
        configdata <- read_delim(configdatapath, delim = ";") %>%
          filter(.data$ConfigName == "DEFAULT" , .data$Version == 1) %>%
          arrange(.data$Order)
      }
    } else {
      stop("config dataset not found. contact the package maintainer")
    }
  } else {   #gebruik je eigen configdata
    configdata <- config
  }

  ### Maak de projectselectie
  if (!is.null(project)) {
    if (length(project) == 1) {
      if (unlist(regexpr("%", text = project)) > 0 | unlist(regexpr("_", text = project)) > 0)
        wh_prj <- paste0("and s.Project like '", project , "'")
      else
        wh_prj <- paste0("and s.Project = '", project, "'")
    }
    else {
      project <- paste0("('",paste(project, collapse = "','"), "')")
      wh_prj <- paste0("and s.Project in ", project)
    }
  }
  else {
    stop("het project moet altijd meegegeven worden als een enkel project, een vector van projecten, of een project met SQL joker karakters")
  }
  qry[qry == "<<PROJECTCRITERIUM>>"] <- wh_prj
  
  if (length(extra)) {
    qry[qry == "<<EXTRA CRITERIA>>"] <- extra
  } else {
    qry[qry == "<<EXTRA CRITERIA>>"] <- ""
  }
  
  
  qry <- paste(qry, collapse = "\n")
  if (show_query) cat(qry)
  
  return(qry)
     
#  ### BOUW DE QUERY
#   tablejoins <- paste0("
# from dimSample
# left join factResult  on factResult.SampleKey = dimSample.sampleKey 
# left join dimAnalysis  on dimAnalysis.AnalysisKey = factResult.AnalysisKey")
# 
#   
#   whereclause <- paste("where dimSample.SampleStatus = 'A' and factResult.IsReportable <> 0", wh_prj, sep = " \n")
#   
#   tablefields <- configdata %>% 
#     rowwise() %>% 
#     mutate(SQLfields = ifelse(!is.na(.data$Table), 
#                               paste0(.data$ReportableName, " = ", .data$Table, ".", .data$Field),
#                               paste0(.data$ReportableName, " = ", .data$Field))) %>% 
#     pull(.data$SQLfields) %>% paste(collapse = ", \n")
#   
#   query <- paste("select ", tablefields, tablejoins, whereclause)
#   if (show_query) {
#     print(query)
#   }
#   return(query)
}


########

#' Staalinformatie halen uit de ingelezen lims datawarehouse data
#'
#' @param df dataset ingelezen via limsdwh_report
#'
#' @return data.frame met enkel de staalinformatie
#' @import dplyr
#' @export
#'
limsdwh_report_sample <- function(df) {
  df %>% 
    select(.data$Klant, .data$Contract, .data$Project, 
           .data$ExternStaalID, .data$Labocode, .data$LinkStaal, 
           .data$MatrixDetail, .data$SamplingPoint, 
           .data$FieldSamplingDate, .data$FieldObserver,
           .data$SamplePreparation, .data$FieldSampleRemark) %>%
    filter(!duplicated(.)) %>%
    arrange(.data$Project, .data$ExternStaalID, .data$Labocode)
}


##########


#' Kruistabel maken van de gegevens uit het lims datawarehouse
#'
#' @param df dataset die bekomen is via limsdwh_report
#' @param link naam waarop de stalen gelinkt worden in de kruistabel (slechts 1 rij per link), behalve als collapse_repli op FALSE staat, dan wordt sowieso de labocode gebruikt
#' @param value de kolomnaam uit df die de cellen zal bemannen in de kruistabel
#' @param collapse_repli Indien FALSE, dan wordt er gelinkt op de labocode ipv de link, indien "first" dan wordt de eerste meting genomen, indien "last" de laatste meting en indien "avg" het gemiddelde tussen de metingen
#'
#' @return dataset met als rijen de staalidentificatie en de kolommen alle resultaten van alle testen
#' @export
#'
limsdwh_report_xtab <- function(df, link = "Labocode", value = "WaardeRuw", collapse_repli = FALSE) {
  df <- df %>% 
    transmute(.data$Labocode, .data$LinkStaal, .data$ExternStaalID, 
              link = .data[[link]], 
              splitcol = paste(.data$LimsAnalysisName, 
                               .data$Component, 
                               ifelse(.data$ReplicaatNr > 1 & !is.na(.data$ReplicaatNr), 
                                      .data$ReplicaatNr, ""), 
                               sep = ":"),
              value = .data[[value]])
  
  dup_labcode <- df %>%
    group_by(.data$Labocode, .data$splitcol) %>% 
    summarize(aantal = n()) %>% 
    arrange(desc(.data$aantal), .data$Labocode)
  dup_linkstaal   <- group_by(df, .data$LinkStaal, .data$splitcol) %>% summarize(aantal = n())
  dup_externid    <- group_by(df, .data$ExternStaalID, .data$splitcol) %>% summarize(aantal = n())
  dup_gekozenlink <- df %>%
    group_by(.data$link, .data$splitcol) %>% 
    summarize(aantal = n()) %>%
    arrange(desc(.data$aantal), .data$link)
  
  if (any(dup_labcode$aantal > 1)) stop("Verschillende waarden voor een analyse van een labostaal zou niet mogen voorkomen. Contacteer de LIMS verantwoordelijke")
  
  if (any(dup_gekozenlink$aantal > 1) & collapse_repli == FALSE) {
    stop("Het linkstaal is niet uniek en collapse_repli is niet ingevuld (FALSE)")
  }
  
  if (all(dup_gekozenlink$aantal <= 1)) {
    dfxtab <- df %>% 
      select(.data$link, .data$splitcol, .data$value) %>%
      spread(key = .data$splitcol, value = .data$value)
  } else {
    if (collapse_repli == "first") {
      message("Het resultaat van het eerste staalnummer wordt gekozen bij duplicaatresultaten")
      dfxtab <- df %>% 
        select(.data$Labocode, .data$link, .data$splitcol, .data$value) %>%
        arrange(.data$labocode, .data$splitcol) %>%
        group_by(.data$link, .data$splitcol) %>% 
        summarize(value = .data$value[1]) %>% 
        spread(key = .data$splitcol, value = .data$value)     
    } else if (collapse_repli == "last") {
      message("Het resultaat van het meest recente staalnummer wordt gekozen bij duplicaatresultaten")
      dfxtab <- df %>% 
        select(.data$Labocode, .data$link, .data$splitcol, .data$value) %>%
        arrange(desc(.data$labocode), .data$splitcol) %>%
        group_by(.data$link, .data$splitcol) %>% 
        summarize(value = .data$value[1]) %>% 
        spread(key = .data$splitcol, value = .data$value)      
    } else if (collapse_repli == "avg") {
      message("numerieke waarden worden uitgemiddeld indien mogelijk (let op , en .). De andere waarden worden NA")
      dfxtab <- df %>% 
        select(.data$Labocode, .data$link, .data$splitcol, .data$value) %>%
        arrange(desc(.data$Labocode), .data$splitcol) %>%
        group_by(.data$link, .data$splitcol) %>% 
        mutate(value = as.numeric(.data$value)) %>%
        summarize(value = mean(.data$value, na.rm = TRUE)) %>% 
        spread(key = .data$splitcol, value = .data$value)         
    } else {
      stop("geen geldige methode gekozen om resultaten te kiezen bij duplicaten. Vul de parameter collapse_repli in of kies een andere link")
    }
  }
  names(dfxtab)[1] = link
  dfxtab
}