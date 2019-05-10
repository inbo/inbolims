### ARGUMENTS

#gets 2 arguments from CMD BATCH: ProjectName and  outputfile


#sink(file = "Q:/BMK/PIETER/Prelog.log")

library('RODBC')
library('reshape')
conn <- odbcConnect("SQLLIMSOEF", uid="LimsAppBusiness", pwd = "1909d90842")

#sink()
#sink(file = "Q:/BMK/PIETER/Prelog.log", append=T)

wdstring <- "\\\\Limstest\\Labware_Oef$\\LIMSOEF\\Data\\Rscripts\\workdir"
setwd(wdstring)

#sink()
#sink(file = "Q:/BMK/PIETER/Prelog.log", append=T)

whichargs <- commandArgs()
# 
# [1] "\\\\Limstest\\Labware_Dev$\\R\\R-3.0.0\\bin\\i386\\Rterm.exe"                    
# [2] "--slave"                                                                         
# [3] "--no-restore"                                                                    
# [4] "--file=\\\\Limstest\\Labware_Dev$\\LIMSDEV\\Data\\Rscripts\\Report_CSV_GEN_SEQ.R"
# [5] "--args"                                                                          
# [6] "I-13073-14"                                                                      
# [7] "Q:\\WOD\\Labo_2014\\0."                                                          
# [8] "LIMS\\I-13073-14_20140414e\\I-13073-14_20140414e_XTAB.csv"                                                                        

print(whichargs)

#sink()
#sink(file = "Q:/BMK/PIETER/Prelog.log", append=T)
####################################################################################################################

projectName <- whichargs[6]
outputfile <- whichargs[7]
username <- whichargs[8] #zou met user moeten overeenkomen

#sink()

#if(substring(outputfile, nchar(outputfile - 3), nchar(outputfile)) != ".csv"){
#    outputfile
#}



#outputfile2 <- NOG AAN TE VULLEN

sink(file = paste(projectName, ".log", sep = ""))

print(whichargs)
print(projectName)
print(outputfile)


#############################################################################################

### SELECT DATA IN DATABASE


sqlstring <- readLines(con = paste("sqlquery_water_xtab_data_", username, ".sql", sep = ""))

#This check does not work on the server!!!!, so we must assume that the first line (utf-coding) must be eliminated
# if(sqlstring[1] == "﻿"){
#     sqlstring <- paste(sqlstring[-1], collapse = "\n")
# } else {
#     sqlstring <- paste(sqlstring, collapse = "\n")
# }

sqlstring <- paste(sqlstring[-1], collapse = "\n")

cat(sqlstring,"\n\n")


### STRUCTURE COLUMN NAMES

descriptivestring <- readLines(con = paste("sqlquery_water_xtab_desc_", username, ".sql", sep = ""))
# if(descriptivestring[1] == "﻿"){
#     descriptivestring <- paste(descriptivestring[-1], collapse = "\n")
# } else {
#     descriptivestring <- paste(descriptivestring, collapse = "\n")
# }
descriptivestring <- paste(descriptivestring[-1], collapse = "\n")

cat(descriptivestring, "\n\n")

### DO THE DATABASE ACTION


cat(conn, "\n\n")
Data <- sqlQuery(conn, sqlstring, stringsAsFactors = FALSE)
AnaData <- sqlQuery(conn, descriptivestring, stringsAsFactors = FALSE)
odbcClose(conn)

cat('Data Dimensions\n------------------------')
print(dim(Data))
print(dim(AnaData))

for (i in 1 : ncol(Data)){
    Data[,i] <- as.character(Data[,i])
}

### MAKE THE CROSSTABLE

cat("---------------\nThe Basic Crosstab\n---------------------\n")

DataCast <- cast(Data, PROJECT + SAMPLE_NUMBER + TEXT_ID + SAMPLE_ID + SAMPLE_TYPE + SAMPLE_STATUS ~ ANALYSIS + NAME + tRep + rRep, fun = max, value = "FORMATTED_ENTRY", na.rm=T )
DataCast$SAMPLE_NUMBER <- as.character(DataCast$SAMPLE_NUMBER) #otherwise generation of NA values possible

print(head(DataCast))
print(dim(DataCast))
write.table(DataCast, "Datacast.txt")


cat("\n------------------------------------------------\n")

ColData <- data.frame( UniqueAnalysis = colnames(DataCast), OrderNumber = 1 : length(colnames(DataCast)))
DataDescript <- merge(ColData, AnaData, by = "UniqueAnalysis", all.x=T, sort=F)
DataDescript <- DataDescript[order(DataDescript$OrderNumber),,drop=F]

DataDescriptT <- t(DataDescript)
DataDescriptT <- DataDescriptT[, !duplicated(DataDescriptT[1,]) , drop = FALSE]
colnames(DataDescriptT) <- colnames(DataCast)
print(head(DataDescriptT))
print(dim(DataDescriptT))
write.table(DataDescriptT, "DatadescriptT.txt")



cat("\n------------------------------------------------\n")

DataCastAdd <- rbind(DataDescriptT[c("REPORTED_NAME","rNAME", "Rep", "DISPLAY_STRING", "UniqueAnalysis"), ], DataCast)

print(head(DataCastAdd))
print(dim(DataCastAdd))
write.table(DataCastAdd, "DataCastAdd.txt")

### WRITE THE CROSSTABLE TO THE OUTPUTFILE

write.csv2(DataCastAdd, file=outputfile,row.names=F)

sink()
