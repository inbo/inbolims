args <- commandArgs()

library(RODBC)
library(dplyr)
library(tidyr)

#setwd("C:\\Users\\pieter_verschelde\\Desktop")

if (args[2] == "--interactive"){
  outputpath <- "resultatenIX.csv"
  analyse <- "IC_ANIONEN"
  start <- "2018-01-01 00:00:00"
  einde <- "2018-12-31 23:59:59"
  stype <- "k1"
  dbpwd <- "**********"
  
} else {
  outputpath <- args[6]
  analyse <- args[7]
  start <- paste(args[8], args[9])
  einde <- paste(args[10], args[11])
  stype <- args[12]
  dbpwd <- args[13]
}

dbodbc <- "sql_D0015_00_Lims_prd"
dbuid <- "LimsAppDbo"

if (stype == "k1") {
    stypestring = " and (s.SAMPLE_TYPE is NULL OR s.SAMPLE_TYPE in ('DUP', 'SUBSAMPLE'))"
} else {
    stypestring = " and s.SAMPLE_TYPE in ('BLANK', 'PRBLANCO')"
}

q <- paste0(
" select r.ANALYSIS, r.NAME, r.NUMERIC_ENTRY",
" from RESULT r inner join test t on t.test_number = r.test_number ",
" inner join sample s on t.SAMPLE_NUMBER = s.SAMPLE_NUMBER",
" where r.ENTERED_ON >= '", start, "' and r.ENTERED_ON <= '", einde, "'",
" and r.STATUS != 'X' and r.STATUS != 'R' and s.STATUS != 'X' and t.STATUS != 'X'",
" and r.ANALYSIS = '", analyse, "'", 
stypestring)

cat(q , file = "resultatenoverzicht_query.txt")


conn <- RODBC::odbcConnect(dsn = dbodbc, uid = dbuid, pwd = dbpwd)
dfResultaten <- RODBC::sqlQuery(conn, q)

odbcClose(conn)


dfResultTable <- 
  dfResultaten %>% 
  filter(!is.na(NUMERIC_ENTRY)) %>%
  group_by(ANALYSIS, NAME) %>%
  do({
    numEntry <- .[["NUMERIC_ENTRY"]]
    totaalAantal <- length(numEntry)
    quants <- quantile(numEntry, prob = 0:10/10)
    catEntry <- cut(numEntry, breaks = quants, include.lowest = TRUE) #let op breaks moeten uniek zijn
    quantstab <- data.frame(cat = catEntry) %>% group_by(cat) %>% summarize(aantal = n())
    print(quantstab)
    gemiddeld <- mean(numEntry)
    sdev <- sd(numEntry)
  }) %>% 
  spread(key = NAME, value = value) %>% 
  mutate(par = factor(par, levels = c("gem", "sd", "aantal", paste0(0:10*10, "%")))) %>%
  arrange(par)
  
write.table(dfResultTable, file = outputpath, row.names = FALSE, sep = "\t", dec = ".")
  



