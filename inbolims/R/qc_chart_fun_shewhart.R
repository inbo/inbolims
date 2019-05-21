#' Selecteer de waarden die als basis voor de controlekaart dienen
#'
#' @param conn db connection
#' @param num number of samples to keep or all samples of a year if coded as JD2017 or all samples from the last D days, coded starting with a D, so num can be like: 30 or JD2017 or D60
#' @param batch the name of the batch, which will be used to check which control samples are returned 
#' @param analysis the name of the analysis
#' @param components a vector of one or more components within the analysis
#' @param start the earliest startdate coded as "YYYY-mm-dd". Only if num is a numeric value
#' @param end the latest enddate coded as "YYYY-mm-dd". Only used if num is a numeric value
#'
#' @return a dataset with all lab information (SAMPLE_NAME, SAMPLE_NUMBER, TEXT_ID, ANALYSIS, NAME, BATCH, ENTRY, C_DATE_BATCHRUN, ENTERED_ON, Nr, CV)
#' @export
#' @importFrom dplyr bind_rows row_number left_join
#' @import dplyr
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' batch = "IC_AN-190430-1"
#' analysis = "IC_ANIONEN"
#' components = c("NO2", "NO3")
#' testdf <- select_control_samples(conn, num = 30, batch, analysis, components)
#' }
select_control_samples <- function(conn, num, batch, analysis, components, 
                                   start = "1900-01-01", end = "2200-01-01"){
  
  #Kies de datums afhankelijk van de laatste N of een opgegeven jaar
  today <- Sys.Date()  
  if (suppressWarnings(is.na(as.numeric(num)))) {
    #indien num geen getal is
    if (substring(num,1,2) == "JD") {
      year <- as.numeric(substring(num,3,6))
      start <- paste0(year, "-01-01")
      end <- paste0(year + 1, "-01-01")
      num <- 5000
    } else if (substring(num, 1, 1) == "D") {
      days_previous <- as.numeric(substring(num, 2))
      end <- as.character(format(today + 1, format = "%Y-%m-%d"))
      start <- as.character(format(today - days_previous, format = "%Y-%m-%d"))
      num <- 5000
    }
  } else {
    #indien num een getal is
    start <- start
    end <- end
  }
  
  #Zoek de verschillende controlestalen die in dit type batch kunnen voorkomen
  sql01 <- paste0("select distinct(s.sample_name) from test t",
                  " inner join sample s on t.sample_number = s.sample_number",
                  " where t.batch = '", batch ,"'",
                  " and s.sample_type in ('CONTROL', 'BLANK', 'PRBLANCO')")
  dfSampNames <- DBI::dbGetQuery(conn, sql01)

  #Maak een query om alle data op te halen
  alldata <- NULL
  for (samptyp in dfSampNames[[1]]) {
    for (comp in components) {
      print(c(samptyp, comp))
      sql02 <- paste0("SELECT top(", num, ") ",
                      "s.SAMPLE_NAME, s.SAMPLE_NUMBER, s.TEXT_ID, r.ANALYSIS, r.NAME, t.BATCH, ", 
                      "r.ENTRY, b.C_DATE_BATCHRUN, r.ENTERED_ON",
                      " from sample s inner join test t on s.sample_number = t.sample_number",
                      " inner join result r on t.test_number = r.test_number",
                      " left join batch b on t.BATCH = b.NAME",
                      " where r.ENTRY is not NULL and r.STATUS in ('E', 'M', 'A')",
                      " and s.SAMPLE_NAME = '", samptyp, "'", " AND r.NAME ='", comp, "'",
                      " and t.ANALYSIS = '", analysis, "'",
                      " and r.ENTERED_ON >= '",start, "'",
                      " and r.ENTERED_ON < '", end, "'",
                      " ORDER BY b.C_DATE_BATCHRUN desc, b.NAME, r.ENTERED_ON desc")
      tmp <- DBI::dbGetQuery(conn, sql02)
      alldata <- bind_rows(alldata, tmp)
    }
  }
  alldata <- 
    na.omit(alldata) %>% 
    arrange(desc(.data$C_DATE_BATCHRUN), desc(.data$ENTERED_ON)) %>%
    group_by(.data$ANALYSIS, .data$NAME, .data$SAMPLE_NAME) %>% 
    mutate(Nr = n() - row_number() + 1)
  
  print(alldata)
  
  ###link data with product specifications
  
  sql03 <-
    paste0("
         select ps0.GRADE, ps0.ANALYSIS, ps0.COMPONENT, CV = ps0.NOMINAL_VALUE
         from product_spec ps0
         inner join
         (select PRODUCT, GRADE, ANALYSIS, COMPONENT,MAX_VERSION = max(VERSION)
         from product_spec
         group by PRODUCT, GRADE, ANALYSIS, COMPONENT) ps1
         on ps0.GRADE = ps1.GRADE and ps0.ANALYSIS = ps1.ANALYSIS and ps0.COMPONENT = ps1.COMPONENT and ps0.VERSION = ps1.MAX_VERSION"
    )
  cvdata <- DBI::dbGetQuery(conn, sql03)
  alldata <- 
    left_join(alldata, cvdata,
              by = c("SAMPLE_NAME" = "GRADE", "ANALYSIS", "NAME" = "COMPONENT")) %>% 
    arrange(.data$SAMPLE_NAME, .data$Nr) %>%
    mutate(ENTRY = as.numeric(.data$ENTRY))

  ### RETURN
  
  write.csv2(alldata, file = "QC_CHARTS.csv")
  
  alldata
}

#############################################################################


#' Calculate Shewhart rules (generic)
#'
#' @aliases lims_shewhart_default
#' @export
#' @rdname lims_shewhart
lims_shewhart <- function(x, ...){
  UseMethod("lims_shewhart")
}

#' Calculate Shewhart rules (lims_data object)
#' 
#' @aliases lims_shewhart_default
#' @export
#' @rdname lims_shewhart
#' @method lims_shewhart lims_data
lims_shewhart.lims_data <- function(x, entrycol = "ENTRY", ...){
  lims_shewhart.default(x = x[[entrycol]], ...)

}

#' Calculate Shewhart rules (default)
#'
#' @param x lims_data object containing all info to make a shewhart plot
#' @param entrycol the column name in the dataset that is used as value column for the results
#' @param center fixed mean value, or calculated when set to NULL
#' @param sd fixed standard deviation, or calculated when set to NULL
#' @param rules definition of rules ??? ADD MORE EXPLANATION ???
#' @param ... other variables passed on
#' @rdname lims_shewhart
#' @method lims_shewhart default
#' @return dataset with the violations for all the 8 shewhart rules
#' @export
lims_shewhart.default <- function(x, center = NULL, sd = NULL, rules=lims_shewhart_rules, ...){
  if (is.null(center)) center <- mean(x)
  if (is.null(sd)) sd <- sd(x)

  plotdata <- lims_shewhart_rules(x, center, sd, runs = c(1,9,6,14,3,5,15,8))
  plotdata <- cbind(Nr = 1:length(x), plotdata)
  plotdata <- as.data.frame(plotdata)
  plotdata$RVIOL <- as.character(paste0(plotdata$rule01,plotdata$rule02,plotdata$rule03,plotdata$rule04,
                           plotdata$rule05,plotdata$rule06,plotdata$rule07,plotdata$rule08))
  plotdata
}


#' Shewhart plot
#'
#' @param plotdata a dataset typically obtained through the function lims_shewhart
#' @param red_rules numeric vector of rule violations that should be marked as red
#' @param orange_rules numeric vector of rule violations that should be marked as orange
#' @param yellow_rules numeric vector of rule violations that should be marked as yellow
#' @param ... not currently in use
#' @importFrom ggplot2 ggplot geom_line geom_point aes xlab ylab 
#' @importFrom rlang .data
#' @return Shewhart ggplot 
#' @export
gg_lims_shewhart <- function(plotdata, red_rules = 1:2, orange_rules = 3:6, yellow_rules = 7:8, ...){
  if (!all(red_rules, orange_rules, yellow_rules) %in% 1:8) stop("chosen rules not valid, should be a vector of integers between 1 and 8")
  if (length(red_rules)) {
    plotdata$RR <- as.numeric(rowSums(plotdata[, paste0("rule", sprintf("%02d",red_rules))]) > 0)
  } else {
    plotdata$RR <- 0
  }
  
  if (length(orange_rules)) {
    plotdata$OR <- as.numeric(rowSums(plotdata[, paste0("rule", sprintf("%02d",orange_rules))]) > 0)
  } else {
    plotdata$OR <- 0
  }
  
  if (length(yellow_rules)) {
    plotdata$YR <- as.numeric(rowSums(plotdata[, paste0("rule", sprintf("%02d",yellow_rules))]) > 0)
  } else {
    plotdata$YR <- 0
  }
  plotdata$color <- ifelse(plotdata$RR, "red", ifelse(plotdata$OR, "orange", ifelse(plotdata$YR, "yellow", "black")))
  plotdata
  
  value <- color <- Nr <- NULL #to avoid NOTE in as-cran
  g <- 
  ggplot(plotdata, aes(x = .data$Nr, y = .data$value)) +
    geom_line(aes(y = .data$lcl_1s), lty = 2, color = "gold") + 
    geom_line(aes(y = .data$ucl_1s), lty = 2, color = "gold") +
    geom_line(aes(y = .data$lcl_2s), lty = 2, color = "orange") + 
    geom_line(aes(y = .data$ucl_2s), lty = 2, color = "orange") +
    geom_line(aes(y = .data$lcl_3s), lty = 2, color = "red") + 
    geom_line(aes(y = .data$ucl_3s), lty = 2, color = "red") +
    geom_line(aes(y = .data$center)) +
    geom_line(lty = 3) +
    geom_point(data = subset(plotdata, color == "black"), color = "darkgreen") +
    geom_point(data = subset(plotdata, color == "red"), color = "red", size = rel(3)) +
    geom_point(data = subset(plotdata, color == "orange"), color = "orange", size = rel(2)) +
    geom_point(data = subset(plotdata, color == "yellow"), color = "yellow") +
    xlab("") + ylab("")
  
  g
}


#' Rule1: point outside 3sigma
#'
#' @param x numeric vactor of lab results
#' @param lcl_3s lower limit for violation
#' @param ucl_3s upper limit for violation
#' @param run should always 1
#'
#' @return TRUE if rule violated
#' @export
qcc_rule01 <- function(x, lcl_3s, ucl_3s, run = 1){
  if (run != 1) stop("run for shewhart rule 01 must be exactly 1")
  x <= lcl_3s | x >= ucl_3s
}

#' Rule2: 9 consecutive points on the same side of the center
#'
#' @param x numeric vector of lab results
#' @param center center line off which the side is calculated
#' @param run how many times the same side before rule violation is triggered
#' @importFrom zoo rollapply
#' @return TRUE if rule violation
#' @export
qcc_rule02 <- function(x, center, run = 9){
  if (length(x) < run) {
    return(rep(FALSE, length(x)))
  } else {
    centered_x <- x - center
    base_data <- zoo::rollapply(centered_x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y) abs(sum(sign(y), na.rm = TRUE)) >= run))    
  }
}

#'Rule3: 6 consecutive points steadily increasing or decreasing
#'
#' @param x numeric vector of lab results
#' @param run how long monotonous increase or decrease before rule violation
#'
#' @return TRUE if rule violation
#' @export
qcc_rule03 <- function(x, run = 6){
  if (length(x) <= run) {
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run + 1, FUN = function(x) x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y) abs(sum(sign(c(tail(y, -1) - head(y, -1))), na.rm = TRUE)) >= (run)))    
  }
}

#'Rule4: 14 consecutive points alternating up and down
#'
#' @param x numeric vector of lab results
#' @param run number of alternating up and down before rule violation
#'
#' @return TRUE if rule violation
#' @export
qcc_rule04 <- function(x, run = 14){
  if (length(x) <= run) {
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run + 1, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    apply(base_data, 1, FUN = function(z) {
      tmp <- sign(tail(z, -1) - head(z, -1))
      tmp[is.na(tmp)] <- 0
      return(all(tmp == rep(c(-1,1),100)[1:run]) | (all(tmp == rep(c(1,-1),100)[1:run])))    
    })
  }
}

#'Rule5: 2 out of 3 consecutive points more than 2 sigma in the same direction
#'
#' @param x numeric vector of lab results
#' @param lcl_2s 2-sigma lower limit
#' @param ucl_2s 2-sigma upper limit
#' @param run (run - 1) violations on run more than 2 sigma in the same direction
#'
#' @return TRUE if rule violated
#' @export
qcc_rule05 <- function(x, lcl_2s, ucl_2s, run = 3){
  if (length(x) < run) {
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y){
      v_upp <- sum(y >= ucl_2s, na.rm = TRUE)
      v_low <- sum(y <= lcl_2s, na.rm = TRUE)
      (v_low >= run - 1) | (v_upp >= run - 1)
    }) )   
  }
}

#'Rule6: 4 out of 5 consecutive points are more than 1 sigma from the center line in the same direction
#'
#' @param x numeric vector of lab results
#' @param lcl_1s 1-sigma lower limit
#' @param ucl_1s 1-sigma upper limit
#' @param run violation if (run - 1) of run results meet the criterion
#' @return TRUE if rule violation
#' @export
qcc_rule06 <- function(x, lcl_1s, ucl_1s, run = 5){
  if (length(x) < run){
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y){
      v_upp <- sum(y >= ucl_1s, na.rm = TRUE)
      v_low <- sum(y <= lcl_1s, na.rm = TRUE)
      (v_low >= run-1) | (v_upp >= run-1)
    }))   
  }
}

#'Rule7: 15 consecutive points are within 1 sigma of the center line
#'
#' @param x numeric vector of lab results
#' @param lcl_1s lower 1-sigma limit
#' @param ucl_1s upper 1-sigma limit
#' @param run how many that meet the criterion before violation is triggered
#' @return TRUE if rule violation
#' @export
qcc_rule07 <- function(x, lcl_1s, ucl_1s, run = 15){
  if (length(x) < run){
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y){
      sum(y <= ucl_1s & y >= lcl_1s, na.rm = TRUE) >= run
    }))    
  }
}

#'Rule8: 8 consecutive points on either side of the center line not within 1 sigma
#'
#' @param x numeric vector of lab results
#' @param lcl_1s  1-sigma lower limit
#' @param ucl_1s  1-sigma upper limit
#' @param run how many before violation is triggerd
#' @return TRUE if violation
#' @export
#'
qcc_rule08 <- function(x, lcl_1s, ucl_1s, run = 8){
  if (length(x) < run){
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y){
      sum((y >= ucl_1s) | (y <= lcl_1s), na.rm = TRUE) >= run
    }))   
  }

}

#' Wrapper function that calculates all 8 Shewhart rules for lab results
#'
#' @param x numeric vector of lab results
#' @param center mean value around which all rules are checked
#' @param sd standard deviation used as basis for the rule violations
#' @param runs vector of 8 elements that contain the run before violation is triggerd for all 8 rules. The first should always be exactly 1, the rest larger than 1.
#' @param ... not used
#'
#' @return matrix containing the value of the lab result, with all 6-sigma stats (-3s, -s, -1s, center, +1s, +2s, +3s) and all 8 rule violations as a TRUE/FALSE vector
#' @export
lims_shewhart_rules <- function(x, center, sd, runs = c(1,9,6,14,3,5,15,8), ...){
  lcl_3s <- center - 3*sd
  lcl_2s <- center - 2*sd
  lcl_1s <- center - 1*sd
  ucl_1s <- center + 1*sd
  ucl_2s <- center + 2*sd
  ucl_3s <- center + 3*sd
  rule01 <- qcc_rule01(x, lcl_3s, ucl_3s, run = runs[1])
  rule02 <- qcc_rule02(x, center = center, run = runs[2])
  rule03 <- qcc_rule03(x, run = runs[3])
  rule04 <- qcc_rule04(x, run = runs[4])
  rule05 <- qcc_rule05(x, lcl_2s, ucl_2s, run = runs[5])
  rule06 <- qcc_rule06(x, lcl_1s, ucl_1s, run = runs[6])
  rule07 <- qcc_rule07(x, lcl_1s, ucl_1s, run = runs[7])
  rule08 <- qcc_rule08(x, lcl_1s, ucl_1s, run = runs[8])
  cbind(value=x, lcl_3s, lcl_2s, lcl_1s, center, ucl_1s, ucl_2s, ucl_3s,
        rule01, rule02, rule03, rule04, rule05, rule06, rule07, rule08)
}


#########################################################################

#' Maak een HTML rapport 
#'
#' @param data dataset verkregen na de functie select_control_samples
#' @param path pad indien afwijkend van de werkdirectory. Zeker eindigen met "/". Zorg dat deze directory reeds bestaat.
#'
#' @importFrom grDevices dev.off png
#' @importFrom stats median na.omit
#' @importFrom utils write.csv2
#' @importFrom dplyr bind_rows arrange desc group_by mutate
#' @return HTML rapport met QC kaarten
#' @export
#'
#' @examples
#' \dontrun{
#' batch = "IC_AN-190430-1"
#' analysis = "IC_ANIONEN"
#' components = c("NO2", "NO3")
#' testdf <- select_control_samples(conn, num = 30, batch, analysis, components)
#' html_qc_report(testdf)
#' }
html_qc_report <- function(data, path = ""){
  
  ### Identificeer directories, en verwijder vorige HTML bestanden
  
  SAMPLE_NAME <- NAME <- C_DATE_BATCHRUN <- ENTERED_ON <- ANALYSIS <- Nr <- NULL
  
  htmlfile <- paste0(path, "index.html")
  figpath <- paste0(path, "figure_html/")
  if (!dir.exists(figpath)) dir.create(figpath)
  
  tmp <- list.files(figpath)
  for (i in 1:length(tmp)) try(file.remove(paste0(figpath, tmp[i])), silent = TRUE)
  
  ### Maak een HTML template, met de regels van boven
  
  cat("<HTML><HEAD><TITLE>QC-chart</TITLE></HEAD><BODY>\n", file = htmlfile, append = FALSE)
  
  cat(file = htmlfile, append = TRUE,
      paste(sep = "\n",
            "<h2><b>De regels zijn als volgt:</b></h2><p>",
            "<ol>",
            "<li><b>Regel 1:</b> 1 point is outside the control limits. -->	A large shift.</li>",
            "<li><b>Regel 2:</b> 8/9 points on the same size of the center line.	--> A small sustained shift.</li>",
            "<li><b>Regel 3:</b> 6 consecutive points are steadily increasing or decreasing.	--> A trend or drift up or down.</li>",
            "<li><b>Regel 4:</b> 14 consecutive points are alternating up and down.	--> Non-random systematic variation.</li>",
            "<li><b>Regel 5:</b> 2 out of 3 consecutive points are more than 2 sigmas from the center line in the same direction. -->	A medium shift</li>",
            "<li><b>Regel 6:</b> 4 out of 5 consecutive points are more than 1 sigma from the center line in the same direction. -->	A small shift.</li>",
            "<li><b>Regel 7:</b> 	15 consecutive points are within 1 sigma of the center line.	--> Stratification.</li>",
            "<li><b>Regel 8:</b> 8 consecutive points on either side of the center line with not within 1 sigma.	 --> A mixture pattern.</li>",
            "</ol><p>"
      ))
  
  ### QC Controlekaarten
  
  for (i in unique(data$SAMPLE_NAME)) {
    newdata <- subset(data, SAMPLE_NAME == i)
    for (j in unique(newdata$NAME)) {
      cat("<p><h1>",'<span style = "color:red">', j,"</span>", i, "</h1>\n\n", file = htmlfile, append = TRUE)
      ppdata <- subset(newdata, NAME == j, c("TEXT_ID", "NAME", "BATCH", "ENTRY", "ENTERED_ON", "CV"))
      ppdata$ENTERED_ON <- substring(ppdata$ENTERED_ON,1,10)
      ppdata$Nr <- 1:nrow(ppdata)
      
      qcdata <- lims_shewhart.lims_data(ppdata, entrycol = "ENTRY")
      g <- gg_lims_shewhart(qcdata)
      #g <- plotly::ggplotly(g)
      cvval <- mean(ppdata$CV)
      
      png(filename = paste0(figpath,"QC_",i,j,".png"), width = 960, height = 620)
      print(g)
      dev.off()
      
      Sys.sleep(0.1)
      figsav <- paste0("figure_html/","QC_",i,j,".png")
      cat("\n<img src=",figsav,"</img>", file = htmlfile, append = TRUE)
      
      ppdata <- cbind(ppdata, "Rule viol" = qcdata[["RVIOL"]])
      gem <- mean(ppdata$ENTRY, na.rm = TRUE)
      med <- median(ppdata$ENTRY, na.rm = TRUE)
      sd <- sd(ppdata$ENTRY, na.rm = TRUE)
      lcl <- gem - 1:3 * sd
      ucl <- gem + 1:3 * sd
      cv <- mean(ppdata$CV)
      
      cat("<p><b>gem</b> = ", round(gem,3), "; <b>CV</b> = ", cv, "<br></br>", file = htmlfile, append = TRUE)
      cat("<b>med</b> = ", round(med, 3), "; <b>sd</b> = ", round(sd, 3), "<br></br></p>\n", file = htmlfile, append = TRUE)
      
      itab <- data.frame(LCL = lcl, UCL = ucl)
      row.names(itab) <- c("1s", "2s", "3s")
      print(xtable::xtable(itab), type = "html", digits = 3, file = htmlfile, append = TRUE, include.rownames = FALSE)
      
      ppdata$CV <- NULL
      print(xtable::xtable(ppdata), type = "html", digits = 3, file = htmlfile, append = TRUE, include.rownames = FALSE )
      
    }
  }
  
  ### EINDIG HTML en Toon deze
  
  cat("\n</BODY></HTML>", file = htmlfile, append = TRUE)
  print(htmlfile)
  
  shell.exec(htmlfile)
}



