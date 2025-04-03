#' Verkrijg de sample metadata
#'
#' @param reportdata data verkregen uit de functie lims_report_data
#' @importFrom dplyr .
#'
#' @return dataset met sample informatie
#' @export
#' @examples
#' \dontrun{
#' conn <- lims_connect()
#' reportdata <- read_lims_data(conn,
#'                              project = c("I-19W001-01"),
#'                              show_query = TRUE)
#' sampledata <- lims_report_samples(reportdata)
#' }
#'
lims_report_samples <- function(reportdata) {
  columns_present <- c(
    "Project",
    "OrigineelStaal",
    "ExternSampleID",
    "LimsStaalNummer",
    "LaboCode"
  ) %in% colnames(reportdata)
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

  if ("AnalyseNaam" %in% colnames(reportdata)) {
    reportdata$Analyse <- reportdata$AnalyseNaam
  } else if ("LimsAnalyseNaam" %in% colnames(reportdata)) {
    reportdata$Analyse <- reportdata$LimsAnalyseNaa
  }

  if (all(c("Analyse", "Component") %in% colnames(reportdata))) {
    df_analyses <- reportdata %>%
      group_by(.data$Project, .data$OrigineelStaal, .data$ExternSampleID) %>%
      summarise(
        Aantal_analyses = n_distinct(.data$Analyse),
        Aantal_resultaten = n_distinct(paste0(
          .data$Analyse,
          .data$Component
        )),
        .groups = "keep"
      ) %>%
      ungroup()
    df_samples_on_orig <- df_samples_on_orig %>% left_join(df_analyses)
  }

  df_parent <- reportdata %>%
    select(.data$OrigineelStaal,
      .data$LimsStaalNummer,
      HoofdLaboCode = .data$LaboCode
    ) %>%
    filter(.data$OrigineelStaal == .data$LimsStaalNummer) %>%
    distinct(.data$OrigineelStaal, .data$HoofdLaboCode)

  df_samples_on_orig <- df_samples_on_orig %>%
    left_join(df_parent, by = "OrigineelStaal")

  groupbycols <- c(
    "OrigineelStaal", "ContractID", "Klant", "Project",
    "VerantwoordelijkLabo", "LimsStaalNummer", "ExternSampleID",
    "LaboCode", "SampleProduct", "ProductGrade", "Matrix",
    "Monsternamedatum", "Monsternemer", "Toestand",
    "VoorbehandelingExtern", "Opmerking"
  )
  df_samples_all <- reportdata %>%
    group_by(across(all_of(intersect(groupbycols, colnames(.))))) %>%
    summarise(
      Aantal_records = n(),
      ArchiefStaal = if_any(c("ArchiefStaal"), ~ max(.)),
      Xcoord = if_any(c("Xcoord"), ~ max(.)),
      Ycoord = if_any(c("Ycoord"), ~ max(.)),
      Diepte = if_any(c("Diepte"), ~ max(.)),
      Toponiem = if_any(c("Toponiem"), ~ max(.)),
      .groups = "drop_last"
    ) %>%
    ungroup() %>%
    select(-.data$OrigineelStaal, -.data$ExternSampleID, -.data$Project)

  collist <- c(
    "Project", "OrigineelStaal", "LaboCode",
    "ExternSampleID", "ProductGrade", "Matrix", "Monsternemer",
    "Monsternamedatum", "Toestand", "VoorbehandelingExtern",
    "Opmerking", "ArchiefStaal", "Xcoord", "Ycoord", "Diepte",
    "Toponiem", "Aantal_analyses", "Aantal_resultaten",
    "Aantal_resultaten"
  )
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
