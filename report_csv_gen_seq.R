### ARGUMENTS

#gets 2 arguments from CMD BATCH: ProjectName and  outputfile

testargs <- commandArgs()

#sink(file = "Q:/BMK/PIETER/Prelog.log")
testargs
#sink()

#sink(file = "Q:/BMK/PIETER/Prelog.log",append=T)
library('RODBC')
library('reshape')
print("Libraries Loaded")
#sink()

#sink(file = "Q:/BMK/PIETER/Prelog.log",append=T)
conn <- odbcConnect("SQLLIMSOEF", uid="LimsAppBusiness", pwd = "1909d90842")
print(conn)
#sink()

#sink(file = "Q:/BMK/PIETER/Prelog.log", append=T)
wdstring <- "\\\\Limstest\\Labware_Oef$\\LIMSOEF\\Data\\Rscripts\\workdir"
setwd(wdstring)
getwd()
#sink()

#sink(file = "Q:/BMK/PIETER/Prelog.log", append=T)
whichargs <- commandArgs()
#sink()
# 
# [1] "\\\\Limstest\\Labware_Dev$\\R\\R-3.0.0\\bin\\i386\\Rterm.exe"                    
# [2] "--slave"                                                                         
# [3] "--no-restore"                                                                    
# [4] "--file=\\\\Limstest\\Labware_Dev$\\LIMSDEV\\Data\\Rscripts\\Report_CSV_GEN_SEQ.R"
# [5] "--args"                                                                          
# [6] "I-13073-14"                                                                      
# [7] "Q:\\WOD\\Labo_2014\\0."                                                          
# [8] "LIMS\\I-13073-14_20140414e\\I-13073-14_20140414e_XTAB.csv"                                                                        

###########################################################################################################

projectName <- whichargs[6]
outputfile <- whichargs[7]
username <- whichargs[8] #zou met user moeten overeenkomen
#sink(file = "Q:/BMK/PIETER/Prelog.log", append=T)
  print(projectName)
  print(outputfile)
  print(username)
#sink()

#if(substring(outputfile, nchar(outputfile - 3), nchar(outputfile)) != ".csv"){
#    outputfile
#}



#outputfile2 <- NOG AAN TE VULLEN


#sink(file = paste(projectName, ".log", sep = ""))

#sink(file = "Q:/BMK/PIETER/Prelog.log", append=T)
print("\n\nstarting part 2")
print(whichargs)
print(projectName)
print(outputfile)
#sink()


#############################################################################################

### SELECT DATA IN DATABASE


filetoread <- paste("sqlquery_gen_xtab_data_", username, ".sql", sep = "")
print(filetoread)
print(getwd())
print(file.path(getwd(), filetoread))

#sink(file = "Q:/BMK/PIETER/Prelog.log", append=T)
sqlstring <- readLines(con = filetoread)
print(sqlstring)
#sink()

#This check does not work on the server!!!!, so we must assume that the first line (utf-coding) must be eliminated
# if(sqlstring[1] == "﻿"){
#     sqlstring <- paste(sqlstring[-1], collapse = "\n")
# } else {
#     sqlstring <- paste(sqlstring, collapse = "\n")
# }

#sink(file = "Q:/BMK/PIETER/Prelog.log", append=T)
sqlstring <- paste(sqlstring[-1], collapse = "\n")


cat("adjusted:\n",sqlstring,"\n\n")
#sink()


### STRUCTURE COLUMN NAMES
#sink(file = "Q:/BMK/PIETER/Prelog.log", append=T)
descriptivestring <- readLines(con = paste("sqlquery_gen_xtab_desc_", username, ".sql", sep = ""))
# if(descriptivestring[1] == "﻿"){
#     descriptivestring <- paste(descriptivestring[-1], collapse = "\n")
# } else {
#     descriptivestring <- paste(descriptivestring, collapse = "\n")
# }
descriptivestring <- paste(descriptivestring[-1], collapse = "\n")
cat("descriptivestring:\n\n")
print(descriptivestring)
#sink()

cat(descriptivestring, "\n\n")

### DO THE DATABASE ACTION

#sink(file = "Q:/BMK/PIETER/Prelog.log", append=T)
print("Now start data read")
#sink()

cat(conn, "\n\n")
Data <- sqlQuery(conn, sqlstring, stringsAsFactors = FALSE)
AnaData <- sqlQuery(conn, descriptivestring, stringsAsFactors = FALSE)
odbcClose(conn)

#sink(file = "Q:/BMK/PIETER/Prelog.log", append=T)
cat("Analysedata\n")
print(head(AnaData))


cat('Data Dimensions\n------------------------')
print(dim(Data))
print(dim(AnaData))

#sink()

for (i in 1 : ncol(Data)){
    Data[,i] <- as.character(Data[,i])
}

### MAKE THE CROSSTABLE

cat("---------------\nThe Basic Crosstab\n---------------------\n")

#Data$Analysestring <- paste(Data$ANALYSIS, Data$NAME, Data$tRep, Data$rRep, sep = "_")
#DataCast <- cast(Data, PROJECT + TEXT_ID + SAMPLE_ID + PRODUCT_GRADE + SAMPLING_POINT + LOGIN_DATE + C_REMARK ~ Analysestring, fun = max, value = "FORMATTED_ENTRY", na.rm=T )

DataCast <- cast(Data, PROJECT + TEXT_ID + SAMPLE_ID + PRODUCT_GRADE + SAMPLING_POINT + LOGIN_DATE + C_REMARK ~ ANALYSIS + NAME + tRep + rRep, fun = max, value = "FORMATTED_ENTRY", na.rm=T )
#DataCast$SAMPLE_NUMBER <- as.character(DataCast$SAMPLE_NUMBER) #otherwise generation of NA values possible

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

