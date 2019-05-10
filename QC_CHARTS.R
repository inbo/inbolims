#Rscript = getConstant("RSCRIPT")
#scriptfile = getConstant("RSCRIPT_QC_CHARTS")
#dbodbc = getConstant("DBODBC")
#dbuid = getConstant("DBUID")
#dbpwd = getConstant("DBPWD")
#batchName = select batch.NAME
#batchName = "pHKCl-170104-1"
#q = "select r.NAME from test t" 
#q = q + " inner join sample s on t.sample_number = s.sample_number" 
#q = q + " inner join result r on r.TEST_NUMBER = t.TEST_NUMBER"
#q = q + " where t.batch = '" + batchName + "'"
#q = q + " and s.sample_type in ('CONTROL', 'BLANK', 'PRBLANCO')"
#q = q + " and r.REPORTABLE in ( 'T', 'F')"
#q = q + " and r.NUMERIC_ENTRY is not NULL"
#q = q + " GROUP BY  r.NAME"
#status = SQL(q, "compListArray")
#q2 = "select distinct(s.sample_name) from test t" 
#q2 = q2 + " inner join sample s on t.sample_number = s.sample_number" 
#q2 = q2 + " where t.batch = '" + batchName + "'"   
#q2 = q2 + " and s.sample_type in ('CONTROL', 'BLANK', 'PRBLANCO')"
#status = SQL(q2, "sampleNameArray")
#numSelections = MultiSelectFromArray(compListArray, "Welke componenten?", "compSelectArray", "T")
#sampleNames = "('" + ArrayToCSVString(SampleNameArray, "','") + "')"
#compNames = "('" + ArrayToCSVString(compSelectArray, "','") + "')"
#numPlot = 30
#operation = "open"
#waitFlag = "T"
#parameters = scriptfile + " " + dbodbc + " " + dbuid + " " + dbpwd + " " + "" + batchName + " " + " " + sampleNames + " " + compNames + " " + numPlot
#status = ShellCommand(Rscript, operation, waitFlag, parameters)



library(RODBC)
setwd("L:\\LabWare-7\\Data\\Rscripts")
file = "QC_CHARTS.log"
sink(file = file, append = FALSE)
print("Argumenten")
print(commandArgs())
arglist <- commandArgs()

print("db pars")
db <- arglist[6]
uid <- arglist[7]
pwd <- arglist[8]
batch <- arglist[9]
analyse <- arglist[10]
#sampnams <- arglist[11]
compnams <- arglist[11]
num <- arglist[12]
sink()
cat("Start connecting db\n", file = file, append = TRUE)


sink(file = file, append = TRUE)



conn <- RODBC::odbcConnect(dsn = db, uid = uid, pwd = pwd)
print(conn)
sql01 <- paste0("select distinct(s.sample_name) from test t", 
     		" inner join sample s on t.sample_number = s.sample_number", 
		" where t.batch = '", batch ,"'",   
		" and s.sample_type in ('CONTROL', 'BLANK', 'PRBLANCO')")

print(sql01)
dfSampNames <- RODBC::sqlQuery(conn, sql01)
comps <- unlist(strsplit(substring(compnams,2, nchar(compnams)-1), split=",", fixed =TRUE))

plotdata <- NULL
for (i in dfSampNames[[1]]){
  for (j in comps){
    sql02 <- paste0("SELECT top(", num, ") s.SAMPLE_NAME, s.SAMPLE_NUMBER, s.TEXT_ID, r.ANALYSIS, r.NAME, r.NUMERIC_ENTRY, r.ENTERED_ON",
                    " from sample s inner join test t on s.sample_number = t.sample_number",
                    " inner join result r on t.test_number = r.test_number",
                    " where r.ENTRY is not NULL and r.STATUS in ('E', 'M', 'A')",
                    " and s.SAMPLE_NAME = '", i, "'", " AND r.NAME = ", j, 
                    " and t.ANALYSIS = '", analyse, "'",
                    " ORDER BY r.ENTERED_ON desc")
    newdata <- na.omit(sqlQuery(conn, sql02))
    print(dim(newdata))
    if (length(newdata)){
      newdata <- newdata[nrow(newdata):1,, drop=FALSE]
      rownames(newdata) <- 1:nrow(newdata)
      newdata$Nr <- 1:nrow(newdata)
      plotdata <- rbind(plotdata, newdata)      
    }
    print(dim(plotdata))    
  }
}


odbcClose(conn)


sink()

write.csv2(plotdata, file = "QC_CHARTS.csv")
Sys.sleep(1)
cat("READY TO RENDER\n", file = file, append = TRUE)
sink(file = file, append = TRUE)
print(plotdata)
sink()
sink(file = file, append = TRUE)
a <- try({
rmarkdown::render("QC_CHARTS.Rmd", clean = FALSE)
shell.exec("L:\\LabWare-7\\Data\\Rscripts\\QC_CHARTS.html")
})
print(a)
sink()



