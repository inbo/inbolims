
#' Haal rapportdata uit LIMS DWH
#'
#' @param project charactervector met projectnamen 
#' @param sqlquery indien "default" wordt de standaardquery uitegevoerd, indien iets anders moet  dit een geldige query zijn voor het DWH en minstens de outputvelden ORIGINAL_SAMPLE, ANALYSIS, COMPONENT en ENTRY bevatten
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
                             sqlquery = "default", 
                             deployment = "prd", 
                             show_query = FALSE) {
  conn <- lims_connect(deployment = deployment)
  print(conn)
  if (sqlquery == "default") {
    sqlquery <- lims_sql_create(project, deployment = deployment)
  } else {
    #query zou reeds moeten kloppen 
  }
  if (show_query) {
    cat(sqlquery)    
  }
  rv <- RODBC::sqlQuery(conn, sqlquery)
  rv
}

#' Vul de query dynamisch in
#'
#' @param project charactervector met projectnamen
#' @param deployment "prd" indien op productie, "uat" indien in developmentomgeving
#'
#' @return lange string met de query
#' @export
#'
lims_sql_create <- function(project = NULL, deployment = "prd") {
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


#' Maak kruistabel van de ingelezen rapportdata
#'
#' @param reportdata data verkregen uit de functie lims_report_data
#' @return
#' @export
#' @importFrom dplyr mutate 
#' @importFrom tidyr pivot_wider
lims_report_xtab <- function(reportdata) {
  sampledata <- lims_report_samples(reportdata)
  reportdata <- reportdata %>% 
    dplyr::mutate(COMBI = paste(LimsAnalyseNaam, 
                                Component,
                                #paste(TestReplicaat,ResultaatReplicaat, sep = "."), 
                                sep = "__"))
  xtab <- reportdata %>% 
    tidyr::pivot_wider(id_cols = OrigineelStaal, 
                names_from = COMBI, 
                values_from = WaardeRuw)
  xtab <- sampledata %>% 
    inner_join(xtab, by = "OrigineelStaal" )
  
  xtab
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
              N_Samp = n_distinct(LimsStaalNummer),
              N_Ana = n_distinct(LimsAnalyseNaam), 
              N_Res = n_distinct(paste0(LimsAnalyseNaam, Component))) %>% 
    ungroup()
  
  dfSamplesAll <- reportdata %>% 
    group_by(Project, LimsStaalNummer, OrigineelStaal, LaboCode, ExternSampleID, 
             ProductGrade, Matrix, Monsternamedatum, Monsternemer, Toestand, 
             VoorbehandelingExtern, Opmerking) %>% 
    summarise(N_Records = n()) %>% 
    ungroup() %>% 
    select(-OrigineelStaal, -ExternSampleID, -Project)
  
  dfSamples <- dfSamplesOnOrig %>% 
    inner_join(dfSamplesAll, 
               by = c("FirstSample" = "LimsStaalNummer")) %>% 
    select(Project, OrigineelStaal, LaboCode, ExternSampleID, ProductGrade, Matrix, 
           Monsternemer, Monsternamedatum, Toestand, VoorbehandelingExtern, Opmerking, 
           N_Ana, N_Res, N_Samp) %>% 
    arrange(Project, ExternSampleID)
    
  dfSamples
}


#' Kruistabel naar csv wegschtrijven met toevoeging header
#'
#' @param data kruistabeldata uit lims_report_xtab
#' @param path pad waar de file geschreven moet worden
#' @return csv file
#' @export
#'
lims_xtab_to_csv <- function(data, path) {
  namen <- colnames(data)
  header <- separate(data.frame(Naam = namen), col = Naam, sep = "__", 
                     #into = c("Analyse", "Component", "Iteratie"), 
                     into = c("Analyse", "Component"), 
                     fill = "right")
  header$COMBI <- namen
  newdata <- as.data.frame(t(header))
  newdata <- newdata %>% mutate(across(.cols = everything(), .funs = as.character))
  colnames(newdata) <- make.names(header$COMBI)
  
  datach <- as.data.frame(data) #for loop want mutate across wil niet alles omzetten
  for (i in 1:ncol(datach)) {
    datach[,i] <- as.character(datach[,i]) 
    datach[,i] <- gsub("\\.", ",", datach[,i])
  }

  newdata <- bind_rows(newdata, datach)
  newdata[is.na(newdata)] <- ""
  write_excel_csv2(newdata, file = path) #niet csv2 want alles is character
}


