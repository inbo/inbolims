#' Haal resultaten uit het LIMS datawarehouse
#'
#' @param conn connectiestring naar de database. Wordt aangemaakt via dwh_connect() of manueel
#' @param project naam van een project, bv "I-19W001-01", of meerdere namen als vector bv. c("I-19W001-01", "I-19W001-02"). Als derde mogelijkheid kan je ook 1 project selecteren met SQL joker karakters als \% en _. Als je alle projecten van contract 19W001 wil kan je dit als volgt specificeren: "\%19W001\%" 
#' @param start vanaf welke monsternamedatum moet gezocht worden (standaard NULL, is vanaf 2014). De verwachte vorm is yyyy-mm-dd 
#' @param end  tot welke monsternamedatum moet gezocht worden (standaard vandaag). De verwachte vorm is yyyy-mm-dd
#' @param analysis naam van een analyse (LimsAnalysisName). Net zoals bij project kan je een vector met meerdere analyses kiezen, of 1 analysenaam doorgeven met SQL jokercharacters \% of _.
#' @param format_dec hoe wordt in het geformatteerd resultaat het decimaalteken weergegeven: "," of "."
#' @param show_query Moet er een print getoond worden van welke query gebruikt werd om de data te bevragen?
#'
#' @return data.frame uit het lims datawarehouse met alle analyseresultaten die gerapporteerd worden door het labo
#' @export
#'
limsdwh_report <- function(conn, 
                           project = NULL, 
                           start = NULL, 
                           end = Sys.Date(), 
                           analysis = NULL, 
                           format_dec = ",",
                           show_query = FALSE) {
  
  wh_prj <- wh_ana <- wh_tim <- ""
  
  if (!is.null(project)) { 
    if (length(project) == 1) {
      if (unlist(regexpr("%", text = project)) > 0 | unlist(regexpr("%", text = project)) > 0)
        wh_prj <- paste0("and s.Project like '", project , "'")
      else
        wh_prj <- paste0("and s.Project = '", project, "'") 
    }
    else {
      project <- paste0("('",paste(project, collapse = "','"), "')")
      wh_prj <- paste0("and s.Project in ", project)      
    }
  }
  
  if (!is.null(analysis)) {
    if (length(analysis) == 1) {
      if (unlist(regexpr("%", text = analysis)) > 0 | unlist(regexpr("%", text = analysis)) > 0)
        wh_ana <- paste0("and f.LimsAnalysisName like '", analysis , "'") 
      else
        wh_ana <- paste0("and f.LimsAnalysisName = '", analysis, "'") 
    }
    else {
      analysis <- paste0("('",paste(analysis, collapse = "','"), "')")
      wh_ana <- paste0("and f.LimsAnalysisName in ", analysis)    
    }
  }

  start <- try(as.POSIXct(start), silent = TRUE)
  if (inherits(start, "try-error")) {
    start <- "2014-01-01"
  }
  end <- try(as.POSIXct(end), silent = TRUE)
  if (inherits(end, "try-error")) { 
    stop("geen geldige eindtijd")
  } else {
    if (start > end) {
      tmp <- start
      start <- end
      end <- start
      rm(tmp)    
    }
    wh_tim <- paste0("and s.FieldSamplingDate between '", start, "' AND '", end, "'")
  }
  
  qrybase <- paste0(
"
  select 
  cus.Customer as Klant
  , left(right(s.Project, len(s.Project) - 2), len(s.Project) - 5) as Contract
  , s.Project 
  , s.FieldSampleID as ExternStaalID
  , s.LabSampleID as Labocode
  , f.LIMSOriginalSampleNumber as LinkStaal
  , s.MatrixDetail
  , s.SamplingPoint
  , s.FieldSamplingDate
  , s.FieldObserver
  , s.SamplePreparation
  , s.FieldSampleRemark
  , f.LimsAnalysisName
  , f.Instrument
  , f.Component
  , a.SAPcode
  , a.Analysis",
  "\n",
  ifelse(format_dec == ",", 
         ", replace(f.ResultFormatted, '.', ',') as WaardeGeformatteerd", 
         ", f.ResultFormatted as WaardeGeformatteerd"),
"
  , f.Result as WaardeRuw
  , LimietSymbool = 
  case left(f.ResultFormatted, 1) 
  when '>' then '>'
  when '<' then '<'
  else ''
  end
  , u.Unit
  , f.ResultFormattedNumeric as WaardeNumeriek
  , f.ResultReplicate as ReplicaatNr
  from dimSample s
  inner join factResult f on s.SampleKey = f.SampleKey
  inner join dimCustomer cus on s.Customer = cus.Customer
  inner join dimAnalysis a on a.AnalysisKey = f.AnalysisKey
  inner join dimUnit u on u.UnitKey = f.UnitKey
  where f.IsReportable = 1
  and s.SampleStatus = 'A'
  and (s.SampleType is null or s.SampleType = 'SUBSAMPLE')
  and f.Project is not null
  and f.ResultStatus <> 'X'
"
  )

  qrywhere <- paste(wh_prj, wh_ana, wh_tim)
  qry <- paste(qrybase, qrywhere, "order by f.Project, s.FieldSampleID, f.LimsAnalysisName", sep = "\n")
  if (show_query == TRUE) {
    cat(qry, "\n")
  }
  DBI::dbGetQuery(conn, qry)
}


########

#' Staalinformatie halen uit de ingelezen lims datawarehouse data
#'
#' @param df dataset ingelezen via limsdwh_report
#'
#' @return data.frame met enkel de staalinformatie
#' @export
#'
report_sample <- function(df) {
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
report_xtab <- function(df, link = "Labocode", value = "WaardeRuw", collapse_repli = FALSE) {
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