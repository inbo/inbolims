############################################################################
# PLAATLAYOUT GENEREREN VOR CRYSTAL REPORT
############################################################################

### >>> CONFIGURE R SESSION ###
### ----------------------- ###

#rootdirectory nodig omdat Citrix niet omkan met relatieve padverwijzingen

args <- commandArgs()

if (length(args) < 6) {
  rootdirectory <- "C:\\INSYNC\\PROJECTEN_GIT/_LABO/inbolims/"
  source(paste0(rootdirectory, "R_SCRIPTS\\FUNCTIONS\\fun_db_credentials.R"))
  args <- readCreds()
} else {
  rootdirectory <- "\\\\LimsBGOPS.inbo.be\\Labware7\\Labware-7\\Data\\"
}

#load libraries - current workdir = source file location
library(xtable)
library(plyr)
library(reshape)
library(RODBC)

# set new work directory
setwd(paste0(rootdirectory, "R_WORKDIR\\_GENETICA\\PLAAT_LAYOUT"))
outdir <- getwd()

#db parameters and variables - working directory must be source location of script file
dbodbc <- args[6]
dbuid <- args[7]
dbpwd <- args[8]

#initialize logfile
logfile <- file.path(getwd(),"ReportDNARun.log")
sink(logfile)
print(as.character(Sys.time()))
cat("Starting script ...\n\n")
sink()


### >>> CONNECTIES MET DB LEGGEN ###
### ---------------------------- ###

sink(logfile, append = TRUE)
print(args)
cat("\n\nStap 1: DBCONNECTIES\n---------------\n")

connectie <- odbcConnect(dsn = dbodbc, uid = dbuid, pwd = dbpwd)
print(warnings())

cat("\n----------\nConnectie\n----------\n")
print(connectie)

LIMSDESIGN <- sqlFetch(channel = connectie, "C_DNA_RUN_REPORT")
sink()


### >>> OPSTELLEN LIMS DESIGN DATASET ###
### --------------------------------- ###

sink(logfile, append = TRUE)
cat("Stap 2: Opstellen van de LIMSDESIGN\n-----------------------------\n")

#Hier de factor levels van plaat maken in volgorde van voorkomen (voor het geval er meer dan 10 platen zijn)
LIMSDESIGN <- data.frame(Replicate     = LIMSDESIGN$ID,
                         Specimen      = LIMSDESIGN$Specimen,
                         Plate         = factor(LIMSDESIGN$Plate, levels = unique(LIMSDESIGN$Plate)),
                         Capilar       = LETTERS[LIMSDESIGN$Capilar],
                         Lane          = LIMSDESIGN$Lane,
                         SampleType    = LIMSDESIGN$SampleType,
                         SampleNumber  = LIMSDESIGN$SampleNumber,
                         ParentSample  = LIMSDESIGN$ParentSample,
                         ParentAliquot = LIMSDESIGN$ParentAliquot,
                         MilliQ        = LIMSDESIGN$MilliQ,
                         DNA           = LIMSDESIGN$DNA)
LIMSDESIGN$Specimen[LIMSDESIGN$Specimen == "leeg"] <- NA
LIMSDESIGN <- LIMSDESIGN[order(LIMSDESIGN$Plate, LIMSDESIGN$Lane, LIMSDESIGN$Capilar),]

#controltypes <- c("CONTROL","BLANK")
exportproject <- paste(substring(LIMSDESIGN$Plate[1],1, unlist(gregexpr("-", LIMSDESIGN$Plate[1]))[2] - 1 ))
exporttype <- "html" #alternatief "pdf"

odbcClose(connectie)
print(LIMSDESIGN)
sink()


### >>> INITIEER DESIGN ###
### ------------------- ###

sink(logfile, append = TRUE)
cat("Stap 3: Opstellen van de RESULTDATA\n-----------------------------\n")

pq <- function(x){paste("'",x,"'",sep = "")}
cat("\n---\n")

connect2 <- odbcConnect(dsn = dbodbc, uid = dbuid, pwd = dbpwd)
print(connect2)

Design <- LIMSDESIGN
Design$Plate <- factor(Design$Plate, levels = unique(Design$Plate)) #unique om volgorde te garanderen voor >10 platen
Design$Plate_Seq <- as.numeric(Design$Plate)
Design$Location <- paste(Design$Plate_Seq, Design$Capilar, Design$Lane, sep = "")
nplates <- length(unique(Design$Plate))

print(nplates)
print(str(Design))
sink()

###

sink(logfile, append = TRUE)
cat("Stap 3b: Opstellen van de RESULTDATA\n-----------------------------\n")

Resultdata <- expand.grid(Plate = levels(Design$Plate),
                          Capilar = LETTERS[1:8],
                          Ixno    = 1:8,
                          Lane01  = '',
                          Lane02  = '',
                          Lane03  = '',
                          Lane04  = '',
                          Lane05  = '',
                          Lane06  = '',
                          Lane07  = '',
                          Lane08  = '',
                          Lane09  = '',
                          Lane10  = '',
                          Lane11  = '',
                          Lane12  = '',
                          stringsAsFactors = F)
Resultdata$Plate <- factor(Resultdata$Plate, levels = levels(Design$Plate))
Resultdata <- Resultdata[order(Resultdata$Plate, Resultdata$Capilar), ]
Resultdata$PlateSeq <-  as.numeric(Resultdata$Plate)
Resultdata$Capilar <- as.character(Resultdata$Capilar)

cat("\n\n----------Resultdata\n---------\n\n")
print(Resultdata)

cat("\n\n----------Design\n---------\n\n")

Design$Specimen <- as.character(Design$Specimen)
Design$SampleType <- as.character(Design$SampleType)
Design$Location <- as.character(Design$Location)
Design$Capilar <- as.character(Design$Capilar)
sink()


### >>> FORMATTEER HET DESIGN ###
### ------------------------- ###

sink(logfile, append = TRUE)
cat("Stap 4: Loopen door het design en de berekeningen formatteren\n-----------------------------\n")

for (i in 1:nrow(Design)) {
  Specimen = Design[i,"Specimen"]
  Plate_Seq = Design[i, "Plate_Seq"]
  SampleType = Design[i, "SampleType"]
  ParentSample = Design[i, "ParentSample"]
  ParentAliquot = Design[i, "ParentAliquot"]
  Location = as.character(Design[i, "Location"])
  Capilar = as.character(Design[i, "Capilar"])
  lane = 3 + Design[i, "Lane"]
  snum = Design[i, "SampleNumber"]
  if (SampleType == "SUBSAMPLE" & !(is.na(SampleType))) {
    pid <- which(Design$SampleNumber == ParentAliquot)[1]
    MilliQ = paste("M", Design[pid, "MilliQ"])
    DNA = paste("D",Design[pid,"DNA"] )
  } else {
    MilliQ = paste("M", Design[i, "MilliQ"])
    DNA = paste("D",Design[i,"DNA"] )
  }

  #Vul de sample labels in
  Resultdata[Resultdata$PlateSeq == Plate_Seq & Resultdata$Capilar == Capilar & Resultdata$Ixno == 1, lane] <- Specimen

  #Als het een subsample betreft geef het originele label in
  if (!is.na(SampleType) & SampleType == "SUBSAMPLE") {
    tmpid = paste("(",Design$Specimen[ParentAliquot == Design$SampleNumber],")", sep = "")
    Resultdata[Resultdata$PlateSeq == Plate_Seq & Resultdata$Capilar == Capilar & Resultdata$Ixno == 2, lane] <- tmpid
    whiOthers <- which(Design$ParentAliquot == ParentAliquot)
    whiOthers <- whiOthers[!(whiOthers == i)] #Het subsample zelf mag niet meegerekend worden
    whiParent <- which(Design$SampleNumber == ParentAliquot)
    reps <- sort(c(whiOthers, whiParent))
    positions <- Design$Location[reps]
    for (k in 1:length(positions)) {
      Resultdata[Resultdata$PlateSeq == Plate_Seq &
                 Resultdata$Capilar  == Capilar   &
                 Resultdata$Ixno == k + 2, lane]  <- positions[k]
    }
  } else if (is.na(SampleType) | SampleType == "") {
      reps <- which(Design$ParentAliquot == snum)
      if (length(reps)) {
        positions <- Design$Location[reps]
        for (k in 1:length(positions)) {
          Resultdata[Resultdata$PlateSeq == Plate_Seq &
                     Resultdata$Capilar  == Capilar   &
                     Resultdata$Ixno == k + 2, lane]  <- positions[k]
        }
      }
    }
  Resultdata[Resultdata$PlateSeq == Plate_Seq & Resultdata$Capilar == Capilar & Resultdata$Ixno == 7, lane] <- MilliQ
  Resultdata[Resultdata$PlateSeq == Plate_Seq & Resultdata$Capilar == Capilar & Resultdata$Ixno == 8, lane] <- DNA
}
sink()


### >>> OPVULLEN PLAAT ###
### ------------------ ###

sink(logfile, append = TRUE)
cat("Stap 5: Plaat opvullen\n-----------------------------\n")

Resultdata$ID <- 1:nrow(Resultdata)
k1 <- seq(3, nrow(Resultdata), by = 8) #8 wegens SNUM, PSNUM, P1, P2, P3, P4, MQ, DNA
k2 <- seq(5, nrow(Resultdata), by = 8)
k3 <- seq(7, nrow(Resultdata), by = 8)
k <- sort(c(k1,k2,k3))
#opvullen volgens k k bevat per staal voor de eerste reultaatregel p1, p2, MQ of p3,p4,DNA
print(k)

for (i in k) {
  Resultdata$Lane01[i] <- paste(Resultdata$Lane01[i], Resultdata$Lane01[i + 1], sep = "  ")
  Resultdata$Lane02[i] <- paste(Resultdata$Lane02[i], Resultdata$Lane02[i + 1], sep = "  ")
  Resultdata$Lane03[i] <- paste(Resultdata$Lane03[i], Resultdata$Lane03[i + 1], sep = "  ")
  Resultdata$Lane04[i] <- paste(Resultdata$Lane04[i], Resultdata$Lane04[i + 1], sep = "  ")
  Resultdata$Lane05[i] <- paste(Resultdata$Lane05[i], Resultdata$Lane05[i + 1], sep = "  ")
  Resultdata$Lane06[i] <- paste(Resultdata$Lane06[i], Resultdata$Lane06[i + 1], sep = "  ")
  Resultdata$Lane07[i] <- paste(Resultdata$Lane07[i], Resultdata$Lane07[i + 1], sep = "  ")
  Resultdata$Lane08[i] <- paste(Resultdata$Lane08[i], Resultdata$Lane08[i + 1], sep = "  ")
  Resultdata$Lane09[i] <- paste(Resultdata$Lane09[i], Resultdata$Lane09[i + 1], sep = "  ")
  Resultdata$Lane10[i] <- paste(Resultdata$Lane10[i], Resultdata$Lane10[i + 1], sep = "  ")
  Resultdata$Lane11[i] <- paste(Resultdata$Lane11[i], Resultdata$Lane11[i + 1], sep = "  ")
  Resultdata$Lane12[i] <- paste(Resultdata$Lane12[i], Resultdata$Lane12[i + 1], sep = "  ")
}

Resultdata <- subset(Resultdata, !(Resultdata$Ixno %in%  c(4,6,8)))
Resultdata$PlateSeq <- NULL
Resultdata$Ixno <- NULL
Resultdata <- Resultdata[c(15,1:14)]


### >>> SQL STATEMENTS ###
### ------------------ ###

sink(logfile, append = TRUE)
cat("Stap 6: SQL statements opstellen\n-----------------------------\n")

Resultdata$sql <-
  with(Resultdata, {
    qstring <- with(Resultdata, paste(
      ID,
      pq(Plate),
      pq(Capilar),
      pq(Lane01),
      pq(Lane02),
      pq(Lane03),
      pq(Lane04),
      pq(Lane05),
      pq(Lane06),
      pq(Lane07),
      pq(Lane08),
      pq(Lane09),
      pq(Lane10),
      pq(Lane11),
      pq(Lane12),
      sep = ","))
    str = paste("INSERT INTO dbo.C_DNA_RUN_REPORT_RESULTS values (", qstring, ");\n")
  })
print(Resultdata)
sink()


### >>> EXECUTE SQL ###
### --------------- ###

sink(logfile, append = TRUE)
cat("Stap 6: Uitvoeren van SQL insert statements\n-----------------------------\n")

junk <- NULL
for (i in 1:nrow(Resultdata)) {
  junk[i] <- sqlQuery(connect2, Resultdata[i,"sql"])
}
print(junk)
odbcClose(connect2)

odbcCloseAll()
sink()


### EINDE ###


