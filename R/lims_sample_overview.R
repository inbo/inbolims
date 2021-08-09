
#' Lims overzicht projectstalen
#'
#' @param conn datawarehouse connection, typically the result of limsdwh_connect()
#' @param customer vector of selected customers. If customer is specified with !, then this will result in a not selection of the customer. e.g customer = c("GEN_DIV", "GEN_BOS") or customer = c("!TESTKLANT")
#' @param project vector of selected projects. "\%" and "\_" can be used as wildcharts as in SQL
#' @param lab vector met de gekozen labo's: mogelijkheden zijn VAST, WATER, GENETICA
#' @param status vector die bepaalt welke statussen van stalen die getoond worden. I (incompleet), P (bezig), C (compleet), A (geauthoriseerd), X (gecanceld) zijn de mogelijkheden
#' @param sql_filter toevoeging aan het where statement in de gegenereerde SQL code
#' @param sql_order toevoeging van een custom ordering via SQL code
#' @param start vroegste logindatum van het staal
#' @param end laatste logindatum van het staal
#' @param show_query toon de gegenereerde SQL query
#' @param show_fields niet gebruikt
#' @importFrom DBI dbGetQuery
#' @return data.frame with selected samples
#' @export
#' @examples
#' \dontrun{
#' conn <-limsdwh_connect()
#' data <- lims_sample_overview(conn)
#' }
lims_sample_overview <- function(conn, customer = NULL, project = NULL, 
                                 lab = c("VAST", "WATER", "GENETICA"), 
                                 status = c("I", "P", "C", "A"), 
                                 sql_filter = NULL, sql_order = NULL,
                                 start = NULL, end = NULL, show_query = FALSE,
                                 show_fields = c("")) {
  
  wh_cst <- wh_prj <- wh_lab <- wh_flt <- wh_ord <-  ""
  
  #selecteer de klanten
  if (!is.null(customer)) {
      notcustomers <- customer[regexpr("!", customer) > 0]
      customers <- customer[regexpr("!", customer) < 0]
      if (length(customers)) {
        customers <- paste0("('", paste(customers, collapse = "', '"), "')")
        wh_cst <- paste0(" and CUSTOMER in ", customers)
      }
      if (length(notcustomers)) {
        notcustomers <- gsub("!", "", notcustomers)
        notcustomers <- paste0("('", paste(notcustomers, collapse = "', '"), "')") 
        wh_cst <- paste(wh_cst, " and CUSTOMER not in ", notcustomers)
      }
  }

  #selecteer de projecten
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
  if (!is.null(lab)) {
    
  }
  
  sqlquery <- paste("
    select top(10000) s.* ,
    EXPGRP = case when p.ProjectLoginTemplate like '%VAST%' then 'VAST' 
                  when p.ProjectLoginTemplate like '%WATER%' then 'WATER' 
                  when p.ProjectLoginTemplate like '%GEN%' then 'GENETICA' 
                  else 'INBO' 
             end
    from dimSample s 
    inner join dimProject p on s.ProjectKey = p.ProjectKey 
    where s.LIMSSampleNumber is not null",
                    wh_cst, 
                    wh_prj, 
                    wh_lab)
  if (show_query) cat(sqlquery, "\n")
  return(dbGetQuery(conn, sqlquery))
}


