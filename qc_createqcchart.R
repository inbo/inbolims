###################################################
###########   QC CHARTS       #####################
###################################################

### R CODE BEGINT CA REGEL 50


### >>> Aanroepcode in LIMS

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



### >>> CONFIGURE R SESSION ###
### ----------------------- ###

#rootdirectory nodig omdat Citrix niet omkan met relatieve padverwijzingen

args <- commandArgs()
sink("\\\\LimsBGOPS.inbo.be\\Labware7\\Labware-7\\Data\\R_WORKDIR\\_QC_CHARTS\\mylog.log")
print(args)
print("ddiirr::")
print(getwd())

if (length(args) < 6) {
  rootdirectory <- "C:\\INSYNC\\PROJECTEN_GIT/_LABO/inbolims/"
  rootdirectory <- "L:\\LabWare-7\\Data\\"
  source(paste0(rootdirectory, "R_SCRIPTS\\FUNCTIONS\\fun_db_credentials.R"))
  #args <- readCreds(additional_vars = c("N_MIN-170518-1", "N_MIN_V", "('NO2.N.DS','NO3.N.DS')", 60)) #--let op testwaarde
  args <- readCreds(file = "L:\\LabWare-7\\Data\\R_SCRIPTS\\dbcredentials.txt", 
                    additional_vars = c("Textuur-180614-1", "TEXTUUR_LD", "('LEEM.6.63µm')", 60)) #--let op testwaarde
  } else {
  rootdirectory <- "\\\\LimsBGOPS.inbo.be\\Labware7\\Labware-7\\Data\\"
}

#load libraries - current workdir = source file location
library(RODBC)
library(xtable)
library(ggplot2)
source(paste0(rootdirectory, "R_SCRIPTS\\FUNCTIONS\\fun_qc_charts.R"))

print(args)
print(getwd())
sink()

# set new work directory
wdir <- paste0(rootdirectory, "R_WORKDIR\\_QC_CHARTS")
setwd(wdir)
outdir <- wdir
logfile <- paste0(wdir, "\\QCcharts.log")

print(logfile)

#db parameters and variables - working directory must be source location of script file
dbodbc <- args[6]
dbuid <- args[7]
dbpwd <- args[8]

#init logfile
sink(file = logfile, append = FALSE)
print("db pars")
db <- args[6]
uid <- args[7]
pwd <- args[8]
batch <- args[9]
analyse <- args[10]
compnams <- args[11]
num <- args[12]
sink()

cat("Start connecting db\n", file = logfile, append = TRUE)
today <- Sys.Date()

if (suppressWarnings(is.na(as.numeric(num)))) {
  if (substring(num,1,2) == "JD") {
    year <- as.numeric(substring(num,3,6))
    startdate <- paste0(year, "-01-01")
    enddate <- paste0(year + 1, "-01-01")
    num <- 5000
  } else if (substring(num, 1, 1) == "D") {
    days_previous <- as.numeric(substring(num, 2))
    enddate <- as.character(format(today + 1, format = "%Y-%m-%d"))
    startdate <- as.character(format(today - days_previous, format = "%Y-%m-%d"))
    num <- 5000
  }
} else {
  startdate <- "1900-01-01"
  enddate <- "2200-01-01"
}


### >>> DB Connectie ###
### ---------------- ###

sink(file = logfile, append = TRUE)

conn <- RODBC::odbcConnect(dsn = db, uid = uid, pwd = pwd)
print(conn)
#SQL zoekt naar de SAMPLE_NAME, wat overeenkomt met het type controlestaal,
#dus eigenlijk worden hier de controlestalen die in dergelijke batches kunnen gevonden worden bepaald
sql01 <- paste0("select distinct(s.sample_name) from test t",
     		" inner join sample s on t.sample_number = s.sample_number",
		" where t.batch = '", batch ,"'",
		" and s.sample_type in ('CONTROL', 'BLANK', 'PRBLANCO')")

print(sql01)
dfSampNames <- RODBC::sqlQuery(conn, sql01)
comps <- unlist(strsplit(substring(compnams,2, nchar(compnams) - 1), split = ",", fixed =TRUE))

plotdata <- NULL
for (i in dfSampNames[[1]]) {
  for (j in 1:length(comps)) { #moet met 1:length(comps) werken want als er µm gebruikt wordt verdwijnt anders laatste "'"
    print(paste("staaltype", i, "component", comps[j], "\n"))
    sql02 <- paste0("SELECT top(", num, ") s.SAMPLE_NAME, s.SAMPLE_NUMBER, s.TEXT_ID, r.ANALYSIS, r.NAME, t.BATCH, r.ENTRY, b.C_DATE_BATCHRUN, r.ENTERED_ON",
                    " from sample s inner join test t on s.sample_number = t.sample_number",
                    " inner join result r on t.test_number = r.test_number",
                    " left join batch b on t.BATCH = b.NAME",
                    " where r.ENTRY is not NULL and r.STATUS in ('E', 'M', 'A')",
                    " and s.SAMPLE_NAME = '", i, "'", " AND r.NAME = ", comps[j],
                    " and t.ANALYSIS = '", analyse, "'",
                    " and r.ENTERED_ON >= '",startdate, "'",
                    " and r.ENTERED_ON < '", enddate, "'",
                    " ORDER BY b.C_DATE_BATCHRUN desc, b.NAME, r.ENTERED_ON desc")
    sqldata <- sqlQuery(conn, sql02)
    newdata <- na.omit(sqldata)
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
cvdata <- sqlQuery(conn, sql03)
write.csv2(plotdata, "plotdata_before_merge_with_cvdata.csv")
write.csv2(cvdata, "cvdata.csv")
plotdata <- merge(plotdata, cvdata,
                  by.x = c("SAMPLE_NAME", "ANALYSIS", "NAME"),
                  by.y = c("GRADE","ANALYSIS","COMPONENT"), sort = FALSE)
plotdata <- plotdata[order(plotdata$SAMPLE_NAME, plotdata$Nr), ,drop = FALSE]

odbcClose(conn)


sink()

plotdata$ENTRY <- as.numeric(plotdata$ENTRY)
write.csv2(plotdata, file = "QC_CHARTS.csv")


### >>> HTMLCommand ###
### --------------- ###

sink(file = logfile, append = TRUE)
data <- read.csv2(file = "QC_CHARTS.csv")
print(dim(data))

htmlfile <- "index.html"
figpath <- "figure_html/"
tmp <- list.files(figpath)
for (i in 1:length(tmp)) try(file.remove(paste0(figpath, tmp[i])))

### START HTML
print(str(data))
print(head(data))
print("START HTML")
sink()

cat("<HTML><HEAD><TITLE>QC-chart</TITLE></HEAD><BODY>\n", file = htmlfile, append = FALSE)

# QC Controlekaarten
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




for (i in unique(data$SAMPLE_NAME)) {
  newdata <- subset(data, SAMPLE_NAME == i)
  print(paste("<p>sample ", i, "\n------\n<p>"))
  print(head(newdata))
  for (j in unique(newdata$NAME)) {
    print(paste("component ", j, "\n+++\n"))
    cat("<p><h1>",'<span style = "color:red">', j,"</span>", i, "</h1>\n\n", file = htmlfile, append = TRUE)
    ppdata <- subset(newdata, NAME == j, c("TEXT_ID", "NAME", "BATCH", "ENTRY", "ENTERED_ON", "CV"))
    ppdata$ENTERED_ON <- substring(ppdata$ENTERED_ON,1,10)
    rownames(ppdata) <- 1:nrow(ppdata)
    print("current data")
    print(ppdata)

    print("qcdata:")
    qcdata <- lims_shewhart.lims_data(ppdata, entrycol = "ENTRY")
    print(qcdata)
    print("shewhart:")
    g <- gg_lims_shewhart(qcdata)
    print(str(g))
    cvval <- mean(ppdata$CV)

    png(filename = paste0(figpath,"QC_",i,j,".png"), width = 960, height = 620)
    print(g)
    dev.off()
    print("fig physically created")
    Sys.sleep(0.1)
    figsav <- paste0("figure_html/","QC_",i,j,".png")
    print(figsav)
    cat("\n<img src=",figsav,"</img>", file = htmlfile, append = TRUE)

    ###

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

    tb <- invisible(print(xtable(itab), type = "html", digits = 3))
    cat(paste0("\n<p>", tb, "<p>\n"), file = htmlfile, append = TRUE)

    ppdata$CV <- NULL
    t2 <- invisible(print(xtable(ppdata), type = "html", digits = 3))
    cat(paste0("\n<p>", t2, "</p>\n"), file = htmlfile, append = TRUE)

  }
}

cat("\n</BODY></HTML>", file = htmlfile, append = TRUE)

shell.exec(paste0(wdir,"\\index.html"))



#RMARKDOWN
#-----------
# Sys.sleep(1)
# cat("READY TO RENDER\n", file = file, append = TRUE)
# sink(file = file, append = TRUE)
# print(plotdata)
# sink()
# sink(file = file, append = TRUE)
# a <- try({
# rmarkdown::render("QC_CHARTS.Rmd", clean = FALSE)
#   shell.exec("\\\\LimsBGOPS.inbo.be\\Labware7\\Labware-7\\Data\\_Rscripts\\QC_CHARTS.html")
#   })
# print(a)
# sink()



