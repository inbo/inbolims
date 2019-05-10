
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
library(RODBC)
source(paste0(rootdirectory, "R_SCRIPTS\\FUNCTIONS\\fun_plates_randomise.R"))

# set new work directory
setwd(paste0(rootdirectory, "R_WORKDIR\\_GENETICA\\DNA_RUN"))
outdir <- getwd()

#db parameters and variables - working directory must be source location of script file
dbodbc <- args[6]
dbuid <- args[7]
dbpwd <- args[8]




###  >>> DB CONNECTION ###
### ------------------ ###


sink("DNAruntest.log")
cat("Stap 1: Werkt de Database connectie\n-------------------------------\n")
print(args)

# The used data is already pre-loaded in the LIMS TABLE C_DNA_EXTRACTION
channel <- odbcConnect(dsn = dbodbc, uid = dbuid, pwd = dbpwd)
DNA <- sqlFetch(channel = channel, "dbo.C_DNA_EXTRACTION")
print(DNA)
sink()


### >>> QC stalen ###
### ------------- ###

sink("DNAruntest.log", append = TRUE)
cat("Stap 2: QC stalen\n-------------------------------\n")

#De originele staalnummers (die teruggelinkt zullen worden aan de tabel)
unique_non_QC_orig <- sort(as.numeric(as.character(DNA$SAMPLE_NUMBER[DNA$SAMPLE_TYPE == "N"])))
QC_samplenumber <- sort(DNA$SAMPLE_NUMBER[DNA$SAMPLE_TYPE == "Q"])

QCsample <- DNA$SAMPLE_NUMBER[DNA$SAMPLE_TYPE == "Q"]
if (length(QCsample) == 0) {
  QCsample <- -1
}
QC <- data.frame(Capilar = c("E", "F"), Line = c(5, 5), ID = c(0, QCsample), Type = c("B", "Q"))
print(QC)
sink()


### >>> RANDOMISE CAPILAR ###
### ----------------------###

sink("DNAruntest.log", append = TRUE)
cat("Stap 3: Datasetlist\n-------------------------------\n")

datasetlist <- randomiseCapilar(
  Specimens = DNA$SAMPLE_NUMBER[DNA$SAMPLE_TYPE == "N"],
  Group = rep("N", sum(DNA$SAMPLE_TYPE == "N")),
  QC = QC,
  nCapilar = 8,
  nLines = 12,
  rReplicates = 0.1,
  minReplicates = 8,
  fillPlate = FALSE,
  FirstLabID = 1,
  Prefix = ""
)
print(datasetlist)

#RandomizeCapilar Returns a list --> convert to dataframe
dataset <- merge(datasetlist$Specimens, datasetlist$Replicates)
dataset$Group <- as.character(dataset$Group)

#order dataset so later on the original samples are filled first
dataset <- dataset[order(dataset$Plate, dataset$Lane, dataset$Capilar),,drop = F]
dataset$Group[duplicated(dataset$Specimen) & dataset$Group == "N"] <- "S"

#make specimen garanteed numeric
dataset$Specimen <- as.numeric(as.character(dataset$Specimen))

#dummy fill in Replicate number (needed for function to work properly)
dataset$Replicate <- 1:nrow(dataset)

#Make a column with the real sample names
#Later we will do a trick to un-randomise the function, so the original samples are not at random
dataset$Specimen_tussentabel <- dataset$Specimen

unique_non_QC_after <- as.numeric(as.character(dataset$Specimen[dataset$Group == "N"]))

#link the original samples order with the randomised order
#this is a cheat, to make sure that the wells are filled in order instead of random
#statistically this is not sound, but is needed for practicallity in lab
linking <- data.frame(SampleNumber = unique_non_QC_orig, After = unique_non_QC_after)
linking <- rbind(linking, c(QC_samplenumber, QC_samplenumber))

dataset <- merge(dataset, linking, all.x = TRUE, by.x = "Specimen", by.y = "After", sort = F)
dataset$SampleNumber[is.na(dataset$SampleNumber) & dataset$Group == "B" ] <- 0

print(dataset)

cat("EINDE DATASETLILST\n\n")

sink()


### >>> OP ORDE STELLEN DATASET ###
### --------------------------- ###

sink("DNAruntest.log", append = TRUE)
cat("Stap 4: Ordenen\n-------------------------------\n")

#order the samples by sample number, and then by positions so duplicates can be found easily
#So we can know if a sample has already been placed on the plate
dataset <- dataset[order(dataset$SampleNumber, dataset$Plate, dataset$Lane, dataset$Capilar),,drop = F]

#If forced Orig before Sub, make sure the first occurence is type N, and the others S
usamples <- unique(dataset$SampleNumber)
for (i in usamples) {
  indicesS <- which(dataset$Group == "S" & dataset$SampleNumber == i)
  indicesN <- which(dataset$Group == "N" & dataset$SampleNumber == i)
  print(c(indicesS, indicesN))
  if ((length(indicesN)) == 1 & (length(indicesS)) >= 1 ) {
    indicesAll <- sort(c(indicesS, indicesN))
    if (length(indicesAll) > 1) {
      dataset$Group[indicesAll[1]] <- "N"
      dataset$Group[indicesAll[-1]] <- "S"
    }
  }
}
sink()


### >>> DBUERIES KLAARMAKEN ###
### ----------------------- ###

sink("DNAruntest.log", append = TRUE)
cat("Stap 5: INSERT STATEMENTS DEFINIEREN\n-------------------------------\n")

if (nrow(dataset) == 0) {
  cat("Geen gegevens gevonden om in te vullen")
}

#Create the SQL INSERT statements as a column in the dataset
dataset$sql <- with(dataset, {
  paste("INSERT INTO dbo.C_DNA_EXTRACTION (DNA_ID, DNA_RUN_ID, SAMPLE_NUMBER, SAMPLE_TYPE, PLATE_SEQ, PLATE_POSITION)",
        " values (",
        Replicate, ",",
        DNA$DNA_RUN_ID[1], ",",
        SampleNumber, ",",
        "'", Group, "'",   ",",
        Plate, ",",
        paste("'",Capilar,Lane,"'", sep = ""),")", sep = "")
})
dataset <- subset(dataset, dataset$Group != 'X')
print(dataset)
sink()


### >>> QUERIES UITVOEREN ###
### --------------------- ###

sink("DNAruntest.log", append = TRUE)
cat("Stap 6: SQL EXECUTE\n-------------------------------\n")

sqlQuery(channel, "DELETE FROM dbo.C_DNA_EXTRACTION")
print("Deletion passed")

junk <- sapply(dataset$sql, sqlQuery, channel = channel)
print("Inserting passed")

odbcClose(channel)
odbcCloseAll()
sink()

### EINDE ###


