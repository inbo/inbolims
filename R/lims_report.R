
#' Show available fields
#'
#'
#' @return dataset containing 2 colums: the name of a field in the database and the corresponding description
#' @export
#' @importFrom readr read_tsv
#' @importFrom dplyr filter select
#'
#' @examples{
#' get_available_report_fields()
#' }

get_available_report_fields <- function() {
  read_tsv(file.path(system.file(package = "inbolims"), 
                     "report_config",
                     "template_fields.tsv")) %>% 
    filter(.data$Type == "select" ) %>% 
    select(.data$Veldnaam, .data$Beschrijving)
}

#################################################


#' Get report config information
#'
#' @param template a character vector of templates you want info from. If "all" then all templates are shown
#'
#' @return dataset wih report template information
#' @export
#'
#' @examples{
#' get_report_config_info(template = "default")
#' }

get_report_config_info <- function(template = "default") {
  data <- read_tsv(file.path(system.file(package = "inbolims"), 
                            "report_config",
                            "template_fields.tsv"))
  names <- colnames(data)
  
  if (template == "all") {
    template_cols <- names[substring(names, 1, 9) == "template:"]
  } else {
    template_cols <- paste0("template:", template)
  }
    
  base_cols <- c("Type", "Veldnaam", "Tabel", "Afkorting", 
                 "Kolom", "Beschrijving")
  
  data %>% select(all_of(c(base_cols, template_cols)))
}


#################################################

#' Haal rapportdata uit LIMS DWH
#'
#' @param connection DBI connection object (see odbc::dbConnect())
#' @param project charactervector met projectnamen 
#' @param sql_template indien "default" wordt de standaardquery uitgevoerd
#' @param show_query indien TRUE toon de query op het scherm net voordat deze uitgevoerd wordt, je kan deze eventueel kopiëren en aanpassen en doorgeven aan custom_sql_query
#' @param custom_fields charactervector die controleert welke veldnamen je in de output ziet. De velden ORIGINAL_SAMPLE, ANALYSIS, COMPONENT en ENTRY worden altijd standaard meegeleverd. Via de functie get_available_report_fields kan je alle namen van de velden zien.
#' @param custom_where_clause custom SQL where clause dat geplakt wordt aan "where PROJECT in ('project_x')", dus een where clause dat je specificeerd als " ResultNumeric > 0 OR ResultNumeric < 100, zal vertaald worden naar de sql where clause: "where PROJECT in ('project_x') AND (ResultNumeric > 0 OR ResultNumeric < 100)"
#' @param custom_sql_query geldige Sql Query bruikbaar voor de connectie. Indien dit veld actief is, wordt sql_template, custom_fields en custom_where_clause genegeerd
#' @param deployment default "prd", "uat" indien op de ontwikkelomgeving gewerkt wordt. Voorlopig is enkel "prd" actief ondersteund
#' @return data.frame met minstens de velden ORIGINAL_SAMPLE, ANALYSIS, COMPONENT en ENTRY 
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- lims_connect()
#' reportdata <- read_lims_data(conn, project = c("I-19W001-01"))
#' }
read_lims_data <- function(connection, 
                           project, 
                           sql_template = "default", 
                           show_query = FALSE,
                           custom_fields = NULL,
                           custom_where_clause = NULL,
                           custom_sql_query = NULL,
                           deployment = "prd"
                          ) {
  #if custom query defined, just execute the query and exit
  if (!is.null(custom_sql_query)) {
    rv <- DBI::dbGetQuery(connection, custom_sql_query)
    return(rv)
  }
  
  template_information <- get_report_config_info(template = sql_template) %>% 
    select(.data$Type, .data$Veldnaam, .data$Afkorting, .data$Kolom, 
           template = contains(sql_template)) %>% 
    filter(.data$template > 0) %>% 
    arrange(.data$template)
  
  sql_query <- parse_sql_report_query(template_information, project)
  if (show_query) {
    cat(sql_query)    
  }
  
  rv <- DBI::dbGetQuery(connection, sql_query)
}

#########################################################################

#' Parse the query based on the template information and the chosen projects
#'
#' @param template dataset containing the template_information. The structure should be exact as the file shipped with the package
#' @param project character string of projects to filter 
#'
#' @return SQL server query
#' @export
#'
parse_sql_report_query <- function(template, project) {
  projects <- paste0("('", paste(project, collapse = "','"),"')")
  template <- template %>% arrange(.data$template)
  
  fields <- template %>% 
    filter(.data$Type == "select") %>% 
    mutate(ss = paste0(.data$Veldnaam, " = ", 
                       .data$Afkorting, ".[", .data$Kolom, "]")) %>% 
    pull(.data$ss) %>% 
    paste(collapse = ",\n")
  
  tables <- template %>% 
    filter(.data$Type == "tabel") %>% 
    pull(.data$Kolom) %>% 
    paste(collapse = " \n")
  
  filters <- template %>% 
    filter(.data$Type == "filter") %>% 
    mutate(flt = paste0(.data$Afkorting, ".", .data$Kolom )) %>% 
    pull(.data$flt) %>% 
    paste(collapse = " AND \n") 
  filters <- gsub("NA.where 1 = 1 AND", "", filters)
  filters <- gsub("<<PROJECTEN>>", projects, filters)
  
  qry <- paste("select ", fields, "from ", tables,  "where ", filters)
  return(qry)
}


#' Maak kruistabel van de ingelezen rapportdata
#'
#' @param reportdata data verkregen uit de functie lims_report_data
#' @return kruistabel met resultaten
#' @export
#' @importFrom dplyr mutate 
#' @importFrom tidyr pivot_wider
#' @examples
#' \dontrun{
#' long_format <- lims_report_data(project = c("I-19W001-01"))
#' XTAB_format <- lims_report_xtab(long_format)
#' }
lims_report_xtab <- function(reportdata) {
  sampledata <- lims_report_samples(reportdata)
  reportdata <- reportdata %>% 
    dplyr::mutate(COMBI = paste(.data$LimsAnalyseNaam, 
                                .data$Component,
                                paste(.data$TestReplicaat,
                                      .data$ResultaatReplicaat, sep = "."), 
                                sep = "__"))
  xtab <- reportdata %>% 
    tidyr::pivot_wider(id_cols = .data$OrigineelStaal, 
                names_from = .data$COMBI, 
                values_from =.data$WaardeRuw)
  xtab <- sampledata %>% 
    inner_join(xtab, by = "OrigineelStaal" )
  
  xtab # %>%  
    #select()
}


#' Verkrijg de sample metadata
#'
#' @param reportdata data verkregen uit de functie lims_report_data
#'
#' @return dataset met sample informatie
#' @export
#' @examples 
#' \dontrun{
#' reportdata <- lims_report_data(project = c("I-19W001-01"))
#' sampledata <- lims_report_samples(reportdata)
#' }
#'
lims_report_samples <- function(reportdata) {
  dfSamplesOnOrig <- reportdata %>%
    group_by(.data$Project, .data$OrigineelStaal, .data$ExternSampleID) %>% 
    summarize(FirstSample = min(.data$LimsStaalNummer), 
              Aantal_stalen = n_distinct(.data$LimsStaalNummer),
              Aantal_analyses = n_distinct(.data$LimsAnalyseNaam), 
              Aantal_resultaten = n_distinct(paste0(.data$LimsAnalyseNaam, 
                                                    .data$Component)),
              .groups = "drop_last") %>% 
    ungroup()
  
  df_parent <- reportdata %>% 
    select(.data$OrigineelStaal, 
           .data$LimsStaalNummer, 
           HoofdLaboCode = .data$LaboCode) %>% 
    filter(.data$OrigineelStaal == .data$LimsStaalNummer) %>% 
    distinct(.data$OrigineelStaal, .data$HoofdLaboCode) 

  dfSamplesOnOrig <- dfSamplesOnOrig %>% 
    left_join(df_parent, by = "OrigineelStaal") 
  
  dfSamplesAll <- reportdata %>% 
    group_by(.data$OrigineelStaal, .data$ContractID, .data$Klant, 
             .data$Project, .data$VerantwoordelijkLabo, 
             .data$LimsStaalNummer, .data$ExternSampleID, 
             .data$LaboCode, .data$SampleProduct, 
             .data$ProductGrade, .data$Matrix, .data$Monsternamedatum,
             .data$Monsternemer, .data$Toestand, 
             .data$VoorbehandelingExtern, .data$Opmerking) %>% 
    summarise(Aantal_records = n(), 
              ArchiefStaal = max(.data$ArchiefStaal), 
              Xcoord = max(.data$Xcoord), 
              Ycoord = max(.data$Ycoord), 
              Diepte = max(.data$Diepte), 
              Toponiem = max(.data$Toponiem), .groups = "drop_last") %>% 
    ungroup() %>% 
    select(-.data$OrigineelStaal, -.data$ExternSampleID, -.data$Project)
  
  dfSamples <- dfSamplesOnOrig %>% 
    inner_join(dfSamplesAll, 
               by = c("FirstSample" = "LimsStaalNummer")) %>% 
    select(.data$Project, .data$OrigineelStaal, .data$HoofdLaboCode, 
           .data$LaboCode, .data$ExternSampleID, 
           .data$ProductGrade, .data$Matrix, 
           .data$Monsternemer, .data$Monsternamedatum, .data$Toestand, 
           .data$VoorbehandelingExtern, .data$Opmerking,
           .data$ArchiefStaal, .data$Xcoord, .data$Ycoord, 
           .data$Diepte, .data$Toponiem,
           .data$Aantal_analyses, 
           .data$Aantal_resultaten, 
           .data$Aantal_stalen) %>% 
    arrange(.data$Project, .data$ExternSampleID)
  
  dfSamples
}

#' #' Maak kruistabel van de ingelezen rapportdata
#' #'
#' #' @param reportdata data verkregen uit de functie lims_report_data
#' #' @return kruistabel met resultaten
#' #' @export
#' #' @importFrom dplyr mutate 
#' #' @importFrom tidyr pivot_wider
#' lims_report_xtab <- function(reportdata) {
#'   sampledata <- lims_report_samples(reportdata)
#'   xtab <- reportdata %>% 
#'     tidyr::pivot_wider(id_cols = .data$OrigineelStaal, 
#'                 names_from = .data$resultaatcode, 
#'                 values_from = .data$WaardeRuw)
#'   xtab <- sampledata %>% 
#'     inner_join(xtab, by = "OrigineelStaal" )
#'   
#'   for (i in 16:ncol(xtab)) {
#'     #print(i)
#'     results <- xtab[,i, drop = TRUE]
#'     results <- ifelse(results %in% c("OFL", "NA"), NA, results)
#'     nas <- sum(is.na(results))
#'     suppressWarnings(new <- as.numeric(xtab[,i, drop = TRUE]))
#'     newnas <- sum(is.na(new))
#'     if (newnas == nas) {
#'       xtab[,i] <- new
#'     }
#'   }
#'   
#'   xtab # %>%  
#'     #select()
#' }

#' Kruistabel naar csv wegschtrijven met toevoeging header
#'
#' @param data kruistabeldata uit lims_report_xtab
#' @param path pad waar de file geschreven moet worden
#' @return csv file
#' @export
#'
lims_report_export <- function(data, path) {
  namen <- tibble(Naam = colnames(data))
  
  header <- separate(namen, col = .data$Naam, sep = "___", 
                     into = c("Analyse", "Component", "Iteratie"), 
                     fill = "right")
  header$COMBI <- namen
  newdata <- as.data.frame(t(header))
  newdata <- newdata %>% 
    mutate(across(.cols = everything(), .funs = as.character))
  colnames(newdata) <- make.names(header %>% pull(.data$COMBI) %>% unlist())
  
  check <<- check <- newdata
  
  datach <- as.data.frame(data) #for loop (accross probleem)
  for (i in 1:ncol(datach)) {
    datach[,i] <- as.character(datach[,i]) 
    datach[,i] <- gsub("\\.", ",", datach[,i])
  }

  newdata <- bind_rows(newdata, datach)
  newdata[is.na(newdata)] <- ""
  write_excel_csv2(newdata, file = path) #niet csv2 want alles is character
}


#' Title
#'
#' @param data lims report data (from lims_report_data)
#' @param plot logical if values are to be put in a graph
#' @param log when plot, use the log-scale?
#' @importFrom ggplot2 scale_y_log10 geom_boxplot facet_wrap
#' @importFrom stats quantile
#'
#' @return list with measured parameters and some base statistics
#' @export
#'
#' @examples
#' \dontrun{
#' view(lims_measured_parameters(reportdata))
#' }
lims_measured_parameters <- function(data, plot = TRUE, log =TRUE) {
  rv <- data %>% 
    group_by(.data$AnalyseNaam, .data$Component, .data$Eenheid) %>% 
    summarise(min = min(.data$NumeriekeWaarde, na.rm=TRUE),
              q25 = quantile(.data$NumeriekeWaarde, 0.25, na.rm=TRUE),
              med = quantile(.data$NumeriekeWaarde, 0.50, na.rm=TRUE),
              avg = round(mean(.data$NumeriekeWaarde, na.rm=TRUE),5),
              q75 = quantile(.data$NumeriekeWaarde, 0.75, na.rm=TRUE),
              max = max(.data$NumeriekeWaarde, na.rm=TRUE),
              aantal = n(), 
              aantalstalen = n_distinct(.data$OrigineelStaal),
              aantalmissend = sum(is.na(.data$NumeriekeWaarde)),)
  if (plot) {
    ggobj <- ggplot(data, aes(x = .data$Component, y = .data$NumeriekeWaarde)) + 
      geom_boxplot() + 
      facet_wrap(~AnalyseNaam, scales = "free")
  if (log) ggobj <- ggobj + scale_y_log10()
  print(ggobj)
  }
  rv
}


