#' Toon lijst met mogelijke keuzes voor de tabelvelden
#'
#' @param conn connectie met de databank. Indien NULL wordt de standaard lims_connect() gebruikt
#' @param keuze keuze waarvan je een lijst van mogelijkheden wilt. Je kan hier kiezen tussen Analyse, Matrix, SampleType, Project, StatusCode, Component
#' @param lab selecteer het labo waar de objecten toe behoren: VAST, WATER of GENETICA
#' @param how hoe wil je dat de informatie getoond wordt; Indien "show" toont dit gewoon de mogelijkheden, indien "list" kan je een of meerdere objecten selecteren uit een getoonde lijst
#' @param multiple if TRUE meerdere elementen kunnen geselecteerd worden, indien FALSE slechts 1 element.
#' @param analyse alleen gebruikt om componenten te selecteren. indien NULL krijg je een keuzelijst, anders wordt de hier gekozen waarde gebruikt
#' @param ... andere argumenten die gebruikt kunnen worden in de functies \link{select.list} of \link{View}
#' @return data.frame met alle waarden van de keuzelijsten, zoals in parameter keuze gedefinieerd, ofwel indien how = "list", dan krijg je een selectiemogelijkheid, en wordt de selectie teruggegeven
#' @export
#'
#' @examples
#' \dontrun{
#' #zorg dat je een connectiebestand met de naam conn hebt
#' lims_list_objects(conn, keuze = "Analysis", how = "show")
#' lims_list_objects(conn, keuze = "Matrix", how = "select", multiple = TRUE)
#' }
lims_list_objects <- function(conn, keuze, lab, how = c("select", "show"), multiple = FALSE, analyse = NULL, ...){
  if (keuze == "Analysis")    q <- "select distinct LimsAnalysisName from dimAnalysis" 
  if (keuze == "Matrix")      q <- "select distinct Matrix  from dimMatrix"
  if (keuze == "SampleType")  q <- "select distinct SampleType  from DimSample"
  if (keuze == "Project")     q <- "select distinct  Project  from DimProject"
  if (keuze == "StatusCode")  q <- "select distinct StatusCode from dimStatus"
  if (keuze == "Component") {
    if (is.null(analyse)) {
      analyse <-  lims_list_objects(conn, keuze = "Analysis", how = "list", multiple = FALSE)
    } else {
      #gebruik analyse uit de argumenten
    }
    q <- paste0("select distinct component from dimComponent where LimsAnalysisName = '", analyse, "'")
  }
  if (keuze == "Customer")    q <- "select distinct Customer from dimCustomer"
  if (keuze == "Contract")    q <- "select distinct ContractNumber from dimProject"
  if (keuze == "lab")         q <- "select Lab = 'VAST' union select Lab = 'WATER' union select Lab = 'GENETICA'"
  
  #Voer de query uit
  df <- DBI::dbGetQuery(conn, q)
  if (how[1] == "show") {
    utils::View(df)
    return(df)
  } else  {
    sel <- utils::select.list(df[[1]], multiple = multiple, ...)
    return(sel)
  }
}
