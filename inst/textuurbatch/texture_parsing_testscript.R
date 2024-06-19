## LD Texture_processor COULTER files from LAB
## Programmed by Pieter Verschelde 9/06/2022
## adapted by Bruno De Vos

### IMPORTANT
### !!! Be sure to have VPN connection to link to LIMS system !!!



library(dplyr)
library(jsonlite)
library(tidyverse) #package met veel datafunctionaliteit
library(DBI) #package voor DB communicatie
library(readxl)

# Download/update met laatste versie 

remotes::install_github('inbo/inbolims')
library(inbolims) #package die de verwerking van de textuurfiles regelt
getwd() #gewoon om te tonen in welke werkdirectory je zit

## CENTRAL LOOP

## load raw filenames in folder  "C:/R/IN/LDTEX/" voor labproject V-22V057 (Cmon)

source_path <- "tests/testdata/"
source_pattern <- "sample"
target_path <- "tests/testdata/result/"

list_fn <- list.files(path = source_path,
                      pattern = source_pattern,
                      full.names = TRUE)
n_list_fn <- length(list_fn)

## Loop to process all files serially   ####
for (i in 1:n_list_fn) {
  filename <- list_fn[i]
  
  #parse de file naar een geldige R dataset
  textuur_parsed <- parse_texture_content(filename, delim = "\t")
  View(textuur_parsed)
  
  #interpreteer de dataset tot een inhoudelijk bruikbaar formaat
  textuur_interpreted <- interpret_texture_content(textuur_parsed)
  View(textuur_interpreted)
  
  #maak een connectie met het LIMS datawarehouse
  conn <- lims_connect() #connect to dwh
  textuur_linked <- link_labo_id(conn, textuur_interpreted)
  dim(textuur_linked)
  
  #schrijf de files weg in /R/OUT/LDTEX/
  write_texture_files(target_path, textuur_linked)
  
}   # loop end

#### Process files and save to CSV and json ####


# listFNOUT<-list.files(path="C:/R_scripts/_GIT_REPO/Cmon/out/LDTEX/2023/deel2", pattern=".csv", full.names = TRUE)
# nlist<-length(listFNOUT)
# 
# 
# for (j in 1:nlist) {
#   TEMPfile<-read.csv2(listFNOUT[j]) 
#   dim(TEMPfile)
#   UNIfile<-distinct(TEMPfile)     ## remove all duplicate rows
#   dim(UNIfile)
#   # write output csv file
#   write.csv2(UNIfile,listFNOUT[j], row.names = FALSE)
#   
#   # run function to convert to json and write in same directory
#   TEX_CSV2JSON(listFNOUT[j])
# }
# 









