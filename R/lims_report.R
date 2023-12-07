#' Show available fields
#'
#'
#' @return dataset containing 2 colums: the name of a field in the database
#' and the corresponding description
#' @export
#' @importFrom readr read_tsv
#' @importFrom dplyr filter select
#'
#' @examples{
#' get_available_report_fields()
#' }

get_available_report_fields <- function() {
  read_tsv(file.path(
    system.file(package = "inbolims"),
    "report_config",
    "template_fields.tsv"
  )) %>%
    filter(.data$Type == "select") %>%
    select(.data$Veldnaam, .data$Beschrijving)
}

#################################################


#' Get report config information
#'
#' @param template a character vector of templates you want info from.
#' If "all" then all templates are shown
#'
#' @return dataset wih report template information
#' @export
#'
#' @examples{
#' get_report_config_info(template = "default")
#' }

get_report_config_info <- function(template = "default") {
  data <- read_tsv(file.path(
    system.file(package = "inbolims"),
    "report_config",
    "template_fields.tsv"
  ))
  names <- colnames(data)

  if (template == "all") {
    template_cols <- names[substring(names, 1, 9) == "template:"]
  } else {
    template_cols <- paste0("template:", template)
  }

  base_cols <- c(
    "Type", "Veldnaam", "Tabel", "Afkorting",
    "Kolom", "Beschrijving"
  )

  data %>% select(all_of(c(base_cols, template_cols)))
}


#########################################################################

#' Parse the query based on the template information and the chosen projects
#'
#' @param template dataset containing the template_information.
#' The structure should be exact as the file shipped with the package
#' @param project character string of projects to filter
#'
#' @return SQL server query
#' @export
#' @examples{
#' temp <- get_report_config_info(template = "default")
#' query <- parse_sql_report_query(temp, "I-19W001-01")
#' query
#' }
#'
parse_sql_report_query <- function(template, project) {
  projects <- paste0("('", paste(project, collapse = "','"), "')")
  template <- template %>% arrange(across(starts_with("template")))
  template <- template %>%
    rename(template = matches("^template"))
  
  
  fields <- template %>%
    filter(.data$Type == "select", !is.na(.data$template)) %>%
    mutate(ss = ifelse(.data$Afkorting != "expr", 
                       paste0(.data$Veldnaam, " = ",
                              .data$Afkorting, ".[", .data$Kolom, "]"),
                       paste0(.data$Veldnaam, " = ",
                              .data$Kolom)
                )
    ) %>%
    pull(.data$ss) %>%
    paste(collapse = ",\n")
  
  tables <- template %>%
    filter(.data$Type == "tabel") %>%
    pull(.data$Kolom) %>%
    paste(collapse = " \n")
  
  filters <- template %>%
    filter(.data$Type == "filter") %>%
    mutate(flt = paste0(.data$Afkorting, ".", .data$Kolom)) %>%
    pull(.data$flt) %>%
    paste(collapse = " AND \n")
  filters <- gsub("NA.where 1 = 1 AND", "", filters)
  filters <- gsub("<<PROJECTEN>>", projects, filters)
  
  qry <- paste("select ", fields, "from ", tables, "where ", filters)
  return(qry)
}

#################################################

#' Haal rapportdata uit LIMS DWH
#'
#' @param connection DBI connection object (see odbc::dbConnect())
#' @param project charactervector met projectnamen
#' @param sql_template indien "default" wordt de standaardquery uitgevoerd, 
#' indien "all" worden alle voorziene velden geïmporteerd, 
#' bij "minimal" worden enkel de essentiële velden geïmporteerd
#' @param show_query indien TRUE toon de query op het scherm
#' net voordat deze uitgevoerd wordt,
#' je kan deze eventueel kopiëren en aanpassen en doorgeven aan custom_sql_query
#' @param custom_fields charactervector die controleert
#' welke veldnamen je in de output ziet.
#' De velden ORIGINAL_SAMPLE, ANALYSIS, COMPONENT en ENTRY
#' worden altijd standaard meegeleverd.
#' Via de functie get_available_report_fields kan je alle namen
#' van de velden zien.
#' @param custom_where_clause custom SQL where clause dat geplakt wordt aan
#'  "where PROJECT in ('project_x')",
#'  dus een where clause dat je specificeerd als
#'  " ResultNumeric > 0 OR ResultNumeric < 100,
#'  zal vertaald worden naar de sql where clause:
#'  "where PROJECT in ('project_x')
#'  AND (ResultNumeric > 0 OR ResultNumeric < 100)"
#' @param custom_sql_query geldige Sql Query bruikbaar voor de connectie.
#' Indien dit veld actief is, wordt sql_template,
#' custom_fields en custom_where_clause genegeerd
#' @param deployment default "prd", "uat" indien op de ontwikkelomgeving
#' gewerkt wordt.
#' Voorlopig is enkel "prd" actief ondersteund
#' @return data.frame met minstens de velden ORIGINAL_SAMPLE,
#' ANALYSIS, COMPONENT en ENTRY
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- lims_connect()
#' reportdata <- read_lims_data(conn,
#'                              project = c("I-19W001-01"),
#'                              sql_template = "default")
#' }
read_lims_data <- function(connection,
                           project,
                           sql_template = "default",
                           show_query = FALSE,
                           custom_fields = NULL,
                           custom_where_clause = NULL,
                           custom_sql_query = NULL,
                           deployment = "prd") {
  # if custom query defined, just execute the query and exit
  if (!is.null(custom_sql_query)) {
    rv <- DBI::dbGetQuery(connection, custom_sql_query)
    return(rv)
  }

  template_information <- get_report_config_info(template = sql_template) %>%
    select(.data$Type, .data$Veldnaam, .data$Afkorting, .data$Kolom,
      template = contains(sql_template)
    ) %>%
    filter(!is.na(.data$template), .data$template > 0) %>%
    arrange(.data$template)

  sql_query <- parse_sql_report_query(template_information, project)
  if (show_query) {
    cat(sql_query)
  }

  rv <- DBI::dbGetQuery(connection, sql_query)
}


##############################################################################

#' Maak kruistabel van de ingelezen rapportdata
#'
#' @param reportdata data verkregen uit de functie lims_report_data
#' @param resulttype "formatted" voor de geformatteerde waarde bv <1.20 
#' of "measured" de originele waarde bv 1.185455 
#' @param sample_fields Vector of sample fields that should appear 
#' in the xtab file which are present in the report data
#' @return kruistabel met resultaten
#' @export
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_wider
#' @examples
#' \dontrun{
#' conn <- lims_connect()
#' reportdata <- read_lims_data(conn, project = c("I-19W001-01"))
#' xtab <- lims_report_xtab(reportdata)
#' }
lims_report_xtab <- function(reportdata, 
                             resulttype = "measured",
                             sample_fields = c("Project", "ExternSampleID")) {
  #sampledata <- lims_report_samples(reportdata)
  
  if (resulttype == "measured") {
    xtab <- reportdata %>%
      tidyr::pivot_wider(
        id_cols = .data$OrigineelStaal,
        names_from = .data$Sleutel,
        values_from = .data$WaardeRuw
      )    
  } else {
    xtab <- reportdata %>%
      tidyr::pivot_wider(
        id_cols = .data$OrigineelStaal,
        names_from = .data$Sleutel,
        values_from = .data$WaardeGeformatteerd  
    )
  }
  
  if (length(sample_fields)) {
    columns <- c("OrigineelStaal", sample_fields)
  } else {
    columns = "OrigineelStaal"
  }
  sample_info <- reportdata %>% select(all_of(columns)) %>% distinct()
  
  xtab <- sample_info %>%
    inner_join(xtab, by = "OrigineelStaal")

  xtab
}

##########################################################################

#' Verkrijg de sample metadata
#'
#' @param reportdata data verkregen uit de functie lims_report_data
#'
#' @return dataset met sample informatie
#' @export
#' @examples
#' \dontrun{
#' conn <- lims_connect()
#' reportdata <- read_lims_data(conn, project = c("I-19W001-01"))
#' sampledata <- lims_report_samples(reportdata)
#' }
#'
lims_report_samples <- function(reportdata) {
  
  columns_present <- c("Project",
                       "OrigineelStaal",
                       "ExternSampleID",
                       "LimsStaalNummer",
                       "LaboCode") %in% colnames(reportdata) 
  if (!all(columns_present)) {
    stop("De kolommen Project, OrigineelStaal, ExternSampleID en LimsStaalNummer
         moeten minstens aanwezig zijn")
  } 

  df_samples_on_orig <- reportdata %>%
    group_by(.data$Project, .data$OrigineelStaal, .data$ExternSampleID) %>%
    summarize(
      Hoofdstaal = min(.data$LimsStaalNummer),
      Aantal_substalen = n_distinct(.data$LimsStaalNummer),
      .groups = "keep"
    )
  
  if("AnalyseNaam" %in% colnames(reportdata)) {
    reportdata$Analyse <- reportdata$AnalyseNaam    
  } else if ("LimsAnalyseNaam" %in% colnames(reportdata)) {
    reportdata$Analyse <- reportdata$LimsAnalyseNaam  
  }

  if (all(c("Analyse", "Component") %in% colnames(reportdata))) {
    df_analyses <- reportdata %>% 
      group_by(.data$Project, .data$OrigineelStaal, .data$ExternSampleID) %>%
      summarise(Aantal_analyses = n_distinct(.data$Analyse),
                Aantal_resultaten = n_distinct(paste0(.data$Analyse,
                                                      .data$Component)),
      .groups = "keep") %>%
      ungroup()     
    df_samples_on_orig <- df_samples_on_orig %>% left_join(df_analyses)
  }

  df_parent <- reportdata %>%
    select(.data$OrigineelStaal,
           .data$LimsStaalNummer,
           HoofdLaboCode = .data$LaboCode) %>%
    filter(.data$OrigineelStaal == .data$LimsStaalNummer) %>%
    distinct(.data$OrigineelStaal, .data$HoofdLaboCode)

  df_samples_on_orig <- df_samples_on_orig %>%
    left_join(df_parent, by = "OrigineelStaal")

  groupbycols <- c("OrigineelStaal", "ContractID", "Klant", "Project",
                   "VerantwoordelijkLabo", "LimsStaalNummer", "ExternSampleID",
                   "LaboCode", "SampleProduct", "ProductGrade", "Matrix",
                   "Monsternamedatum", "Monsternemer", "Toestand",
                   "VoorbehandelingExtern", "Opmerking")
  df_samples_all <- reportdata %>%
    group_by(across(all_of(intersect(groupbycols, colnames(.))))) %>%
    summarise(
      Aantal_records = n(),
      ArchiefStaal = if_any(c("ArchiefStaal"), ~ max(.)),
      Xcoord = if_any(c("Xcoord"), ~ max(.)),
      Ycoord = if_any(c("Ycoord"), ~ max(.)),
      Diepte = if_any(c("Diepte"), ~ max(.)),
      Toponiem = if_any(c("Toponiem"), ~ max(.)),
      .groups = "drop_last") %>%
    ungroup() %>%
    select(-.data$OrigineelStaal, -.data$ExternSampleID, -.data$Project)

  
  collist <- c("Project", "OrigineelStaal", "LaboCode",
               "ExternSampleID", "ProductGrade", "Matrix" ,"Monsternemer",
               "Monsternamedatum", "Toestand", "VoorbehandelingExtern",
               "Opmerking", "ArchiefStaal", "Xcoord", "Ycoord", "Diepte",
               "Toponiem", "Aantal_analyses", "Aantal_resultaten",
               "Aantal_resultaten")
  df_samples <- df_samples_on_orig %>%
    inner_join(df_samples_all,
      by = c("Hoofdstaal" = "LimsStaalNummer")
    ) %>%
    select(
      intersect(collist, colnames(.))
    ) %>%
    arrange(.data$Project, .data$ExternSampleID)

  df_samples
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

  header <- separate(namen,
    col = .data$Naam, sep = "___",
    into = c("Analyse", "Component", "Iteratie"),
    fill = "right"
  )
  header$COMBI <- namen
  newdata <- as.data.frame(t(header))
  newdata <- newdata %>%
    mutate(across(.cols = everything(), .fns = as.character))
  colnames(newdata) <- make.names(header %>% pull(.data$COMBI) %>% unlist())

  datach <- as.data.frame(data) # for loop (accross probleem)
  for (i in seq_len(ncol(datach))) {
    datach[, i] <- as.character(datach[, i])
    datach[, i] <- gsub("\\.", ",", datach[, i])
  }

  newdata <- bind_rows(newdata, datach)
  newdata[is.na(newdata)] <- ""
  write_excel_csv2(newdata, file = path) # niet csv2 want alles is character
}


#' Title
#'
#' @param data lims report data (from lims_report_data)
#' @param plot if NULL no plot is created, if "boxplot" a boxplot is shown,
#' if "histogram" a histogram
#' @param log when plot, use the log-scale?
#' @importFrom ggplot2 scale_y_log10 geom_boxplot facet_wrap ggplot aes
#' @importFrom ggplot2 element_text element_blank theme xlab rel geom_histogram
#' @importFrom stats quantile
#'
#' @return list with measured parameters and some base statistics
#' @export
#'
#' @examples
#' \dontrun{
#' view(lims_measured_parameters(reportdata))
#' }
lims_measured_parameters <- function(data, plot = "boxplot", log = TRUE) {
  cols_to_check <- c("LimsAnalyseNaam", "AnalyseNaam", "Component", "Eenheid")
  if (!("NumeriekeWaarde" %in% colnames(data))) {
    data$NumeriekeWaarde <- as.numeric(data$WaardeRuw)
  }
  
  rv <- data %>%
    group_by(across(all_of(intersect(cols_to_check, colnames(.))))) %>%
    summarise(
      min = min(.data$NumeriekeWaarde, na.rm = TRUE),
      q25 = quantile(.data$NumeriekeWaarde, 0.25, na.rm = TRUE),
      med = quantile(.data$NumeriekeWaarde, 0.50, na.rm = TRUE),
      avg = round(mean(.data$NumeriekeWaarde, na.rm = TRUE), 5),
      q75 = quantile(.data$NumeriekeWaarde, 0.75, na.rm = TRUE),
      max = max(.data$NumeriekeWaarde, na.rm = TRUE),
      aantal = n(),
      aantalstalen = n_distinct(.data$OrigineelStaal),
      aantalmissend = sum(is.na(.data$NumeriekeWaarde)),
    )
  if (length(plot)) {
    ggobj <- ggplot(data, 
                    aes(x = substring(.data$Sleutel, 1, nchar(.data$Sleutel)-6),
                        y = .data$NumeriekeWaarde))
      if (plot == "boxplot") {
        ggobj <- ggobj + geom_boxplot()
      } else if (plot == "histogram") {
        ggobj <- ggobj + 
          geom_histogram(aes(x = .data$NumeriekeWaarde), inherit.aes = FALSE)
      } else {
        stop("no valid plot type")
      }
      ggobj <- ggobj + 
        facet_wrap(~substring(.data$Sleutel, 1, nchar(.data$Sleutel)-6),
                   scales = "free") +
      xlab("Analysecomponent") +
      theme(strip.text = element_blank(),
            axis.text.x = element_text(size = rel(0.8)))
      if (plot == "histogram") ggobj <- ggobj +
        theme(strip.text = element_text(size = rel(0.5)))
    if (log) ggobj <- ggobj + scale_y_log10()
    print(ggobj)
  }
  rv
}
