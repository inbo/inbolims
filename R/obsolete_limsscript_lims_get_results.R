

#######################################################

#' Lees ruwe data in
#'
#' @param conn connectie met de databank bekomen via lims_connect(). Indien NULL zal de standaard lims_connect() door deze functie worden uitgevoerd, alsook een disconnect
#' @param sampletype vector met gekozen sampletypes. NULL geeft enkel de hoofdstalen. Standaard wordt NULL, DUP en SUBSAMPLE gekozen, omdat dit de 3 types zijn die de gewone stalen weergeven. PBL en BLANK zijn de blanco's
#' @param contract numerieke vector met gekozen contractnummers. NULL geeft ze allemaal
#' @param project charactervector met gekozen projecten. NULL geeft ze allemaal
#' @param analysis charactervector met gekozen analyses. NULL is allemaal
#' @param component charactervector met gekozen componenten. NULL is allemaal
#' @param matrix charactervector met gekozen matrices. NULL is allemaal
#' @param start datumtijd variabele met de starttijd
#' @param end datumtijd variabele met de einddtijd
#' @param samplestatus welke sample status (standaard C en A). NULL geeft ze allemaal
#' @param resultstatus welke resultaatstatus (standaard E en M en A). NULL geeft ze allemaal
#' @param n_max hoeveel resultaten maximaal tonen
#' @param print_query toon de querystring op het scherm
#' @return dataset met de gevraagde resultaten
#' @export
#'
#' @examples
#' \dontrun{
#' data <- lims_get_results(lims_connect(),project = "I-18W001-01")
#' data <- lims_get_results(analysis = "LOI_AS_OVEN_550", start = "2018-12-30")
#' data <- lims_get_results(analysis = "CACO3_TIT_V", component = c("CaCO3%", "Gewicht"), 
#'                          start = "2018-01-01", end = "2019-01-01")
#' }
lims_get_results <- function(conn = NULL, sampletype = c("NULL", "DUP", "SUBSAMPLE"), contract = NULL, project=NULL, analysis=NULL, component=NULL, matrix = NULL, start=end - 4*31536000, end = Sys.time(), samplestatus = c("C", "A"), resultstatus = c("E", "M", "A"), n_max = 100000, print_query = TRUE){

  disconnect <- FALSE
  if (is.null(conn)) {
    disconnect <- TRUE
    conn <- try(limsdwh_connect())
    if (class(conn) != "Microsoft SQL Server") 
      stop("Geen geldige databankconnectie kunnen tot stand brengen")
  }

  start <- try(as.POSIXct(start))
  if (inherits(start, "try-error")) stop("geen geldige starttijd")
  end <- try(as.POSIXct(end))
  if (inherits(end, "try-error")) stop("geen geldige eindtijd")
  if (start > end) {
    tmp <- start
    start <- end
    end <- start
    rm(tmp)
  }

  wh_sty <- ""
  wh_ctr <- ""
  wh_prj <- ""
  wh_ana <- ""
  wh_cmp <- ""
  wh_mat <- ""
  wh_tim <- ""
  wh_sst <- ""
  wh_rst <- ""

  if (any(sampletype %in% c("NA", "NULL"))) {
    wh_sty <- "s.SampleType is null"
    sampletype <- sampletype[-which(sampletype %in% c("NA", "NULL"))]
  }
  if (length(sampletype)) {
    sampletype <- paste0("('",paste(sampletype, collapse = "','"), "')")
    wh_sty <- paste(wh_sty, "or s.SampleType in ", sampletype)
  } else {
    wh_sty <- wh_sty
  }
    
  if (!is.null(contract)) { #voorlopig nog contractnummer, in DWH staat contractnaam niet
    contract <- paste0("(",paste(contract, collapse = ","), ")")
    wh_ctr <- paste("and s.ContractNumber in", contract)
  }
  
  if (!is.null(project)) { 
    project <- paste0("('",paste(project, collapse = "','"), "')")
    wh_prj <- paste("and s.Project in", project)
  }
  
  if (!is.null(analysis)) {
    analysis <- paste0("('",paste(analysis, collapse = "','"), "')")
    wh_ana <- paste("and f.LimsAnalysisName in", analysis)
  }
  
  if (!is.null(component)) {
    component <- paste0("('",paste(component, collapse = "','"), "')")
    wh_cmp <- paste("and f.Component in", component)
  }
  
  if (!is.null(matrix)) {
    matrix <- paste0("('",paste(matrix, collapse = "','"), "')")
    wh_mat <- paste("and s.Matrix in", matrix)
  }
  
  wh_tim <- paste0("and f.AnalysisDate between '", start, "' AND '", end, "'")
  
  if (!is.null(samplestatus)) {
    samplestatus <- paste0("('",paste(samplestatus, collapse = "','"), "')")
    wh_sst <- paste("and s.SampleStatus in", samplestatus)
  }

  if (!is.null(resultstatus)) {
    resultstatus <- paste0("('",paste(resultstatus, collapse = "','"), "')")
    wh_rst <- paste("and f.ResultStatus in", resultstatus)
  }
  
  
  qrysel <- paste(
    "select top(", as.integer(n_max), ") s.Customer, s.ContractNumber, s.Project, s.LabSampleID, s.FieldSampleID,",
    "s.SampleType, s.SampleStatus, s.Matrix, s.MatrixDetail,",
    "f.LimsSampleNumber, f.LIMSOriginalSampleNumber,",
    "f.LimsAnalysisName, f.AnalysisVersion, f.Batch,",
    "f.Component, u.Unit,",
    "f.AnalysisDate, f.Result, f.ResultNumeric, f.ResultStatus",
    "from dimSample s",
    "inner join factResult f on s.SampleKey = f.sampleKey",
    "inner join dimUnit u on f.UnitKey = u.UnitKey")
  
  qrywhere <- paste("where ( f.LimsSampleNumber is not NULL or ",  wh_sty, ") ")
  qrywhere <- paste(qrywhere, wh_ctr, wh_prj, wh_ana, wh_cmp, wh_mat, wh_tim, wh_sst, wh_rst)
  
  qry <- paste(qrysel, qrywhere, "order by f.AnalysisDate desc")
  print(qry)
  AnalysisDate <- NULL #gewoon om geen NOTE te krijgen bij compilatie
  data <- DBI::dbGetQuery(conn, qry)
  data <- dplyr::mutate(data, AnalysisDate = as.POSIXct(AnalysisDate))
  data
}

