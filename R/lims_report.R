
#' Haal rapportdata uit LIMS DWH
#'
#' @param project charactervector met projectnamen 
#' @param template indien "default" wordt de standaardquery uitegevoerd, indien iets anders moet  dit een geldige query zijn voor het DWH en minstens de outputvelden ORIGINAL_SAMPLE, ANALYSIS, COMPONENT en ENTRY bevatten
#' @param deployment default "prd", "uat" indien op de ontwikkelomgeving gewerkt wordt
#' @param show_query indien TRUE toon de query net voordat deze uitgevoerd wordt
#' @return data.frame met minstens de velden ORIGINAL_SAMPLE, ANALYSIS, COMPONENT en ENTRY 
#' @export
#'
#' @examples
#' \dontrun{
#' lims_report_data(project = c("I-19W001-01"))
#' }
lims_report_data <- function(project, 
                             template = "default", 
                             deployment = "prd", 
                             show_query = FALSE) {
  conn <- lims_connect(deployment = deployment)
  print(conn)
  if (template == "default") {
    sqlquery <- lims_report_sql_create(project, deployment = deployment)
  } else {
    #query zou reeds moeten kloppen 
  }
  if (show_query) {
    cat(sqlquery)    
  }
  rv <- DBI::dbGetQuery(conn, sqlquery)
  rv %>% transmute(OrigineelStaal, LimsStaalNummer, 
                   ContractID, Klant, Project, VerantwoordelijkLabo,
                   ExternSampleID, LaboCode, SampleProduct, 
                   ProductGrade, Matrix, 
                   Monsternemer, Monsternamedatum, Toestand,
                   VoorbehandelingExtern, Opmerking, 
                   LimsAnalyseNaam, LimsAnalyseVersie, SapCode, 
                   AnalyseNaam, Component, 
                   Resultaattype = ifelse(is.na(NumeriekeWaarde), 'TXT', 'NUM'),
                   Instrument, Batch, 
                   WaardeRuw, WaardeGeformatteerd, Eenheid, NumeriekeWaarde, 
                   ArchiefStaal, Xcoord, Ycoord, Diepte, Toponiem, 
                   resultaatcode = paste(LimsAnalyseNaam, Component,
                                         paste0(TestReplicaat,
                                                ResultaatReplicaat), 
                                         sep = "___"))
}




#' Vul de query dynamisch in
#'
#' @param project charactervector met projectnamen
#' @param deployment "prd" indien op productie, "uat" indien in developmentomgeving
#'
#' @return lange string met de query
#' @export
#'
lims_report_sql_create <- function(project = NULL, deployment = "prd") {
  sqlfile <- system.file(paste0("dwh_bevraging/basisquery_rapport_dwh_", 
                                deployment, 
                                ".sql"), 
                         package = "inbolims") 
  if (!length(project)) {
    stop("geen projectnaam doorgegeven")
  }
  project <- paste0("('", paste(project, collapse = "','"), "')")
  qry_from_file <- paste(readLines(sqlfile), collapse = '\n')
  qry <- gsub("USE W0003_00_LIMS", "", qry_from_file)
  qry <- gsub("'<<INSERT_PROJECT_NAME>>'", project, qry)
  qry
}





#' Verkrijg de sample metadata
#'
#' @param reportdata data verkregen uit de functie lims_report_data
#'
#' @return dataset met sample informatie
#' @export
#'
lims_report_samples <- function(reportdata) {
  dfSamplesOnOrig <- reportdata %>%
    group_by(Project, OrigineelStaal, ExternSampleID) %>% 
    summarize(FirstSample = min(LimsStaalNummer), 
              Aantal_stalen = n_distinct(LimsStaalNummer),
              Aantal_analyses = n_distinct(LimsAnalyseNaam), 
              Aantal_resultaten = n_distinct(paste0(LimsAnalyseNaam, 
                                                    Component)),
              .groups = "drop_last") %>% 
    ungroup()
  
  dfSamplesAll <- reportdata %>% 
    group_by(OrigineelStaal, ContractID, Klant, Project, VerantwoordelijkLabo, 
             LimsStaalNummer, ExternSampleID, LaboCode, SampleProduct, 
             ProductGrade, Matrix, Monsternamedatum, Monsternemer, Toestand, 
             VoorbehandelingExtern, Opmerking) %>% 
    summarise(Aantal_records = n(), 
              ArchiefStaal = max(ArchiefStaal), 
              Xcoord = max(Xcoord), 
              Ycoord = max(Ycoord), 
              Diepte = max(Diepte), 
              Toponiem = max(Toponiem), .groups = "drop_last") %>% 
    ungroup() %>% 
    select(-OrigineelStaal, -ExternSampleID, -Project)
  
  dfSamples <- dfSamplesOnOrig %>% 
    inner_join(dfSamplesAll, 
               by = c("FirstSample" = "LimsStaalNummer")) %>% 
    select(Project, OrigineelStaal, LaboCode, ExternSampleID, 
           ProductGrade, Matrix, 
           Monsternemer, Monsternamedatum, Toestand, 
           VoorbehandelingExtern, Opmerking,
           ArchiefStaal, Xcoord, Ycoord, Diepte, Toponiem,
           Aantal_analyses, Aantal_resultaten, Aantal_stalen) %>% 
    arrange(Project, ExternSampleID)
  
  dfSamples
}





#' Maak kruistabel van de ingelezen rapportdata
#'
#' @param reportdata data verkregen uit de functie lims_report_data
#' @return
#' @export
#' @importFrom dplyr mutate 
#' @importFrom tidyr pivot_wider
lims_report_xtab <- function(reportdata) {
  sampledata <- lims_report_samples(reportdata)
  xtab <- reportdata %>% 
    tidyr::pivot_wider(id_cols = OrigineelStaal, 
                names_from = resultaatcode, 
                values_from = WaardeRuw)
  xtab <- sampledata %>% 
    inner_join(xtab, by = "OrigineelStaal" )
  
  for (i in 16:ncol(xtab)) {
    #print(i)
    results <- xtab[,i, drop = TRUE]
    results <- ifelse(results %in% c("OFL", "NA"), NA, results)
    nas <- sum(is.na(results))
    suppressWarnings(new <- as.numeric(xtab[,i, drop = TRUE]))
    newnas <- sum(is.na(new))
    if (newnas == nas) {
      xtab[,i] <- new
    }
  }
  
  xtab # %>%  
    #select()
}




#' Kruistabel naar csv wegschtrijven met toevoeging header
#'
#' @param data kruistabeldata uit lims_report_xtab
#' @param path pad waar de file geschreven moet worden
#' @return csv file
#' @export
#'
lims_report_export <- function(data, path) {
  namen <- tibble(Naam = colnames(data))
  
  header <- separate(namen, col = Naam, sep = "___", 
                     into = c("Analyse", "Component", "Iteratie"), 
                     fill = "right")
  header$COMBI <- namen
  newdata <- as.data.frame(t(header))
  newdata <- newdata %>% 
    mutate(across(.cols = everything(), .funs = as.character))
  colnames(newdata) <- make.names(header %>% pull(COMBI) %>% unlist())
  
  check <<- newdata
  
  datach <- as.data.frame(data) #for loop (accross probleem)
  for (i in 1:ncol(datach)) {
    datach[,i] <- as.character(datach[,i]) 
    datach[,i] <- gsub("\\.", ",", datach[,i])
  }

  newdata <- bind_rows(newdata, datach)
  newdata[is.na(newdata)] <- ""
  write_excel_csv2(newdata, file = path) #niet csv2 want alles is character
}


