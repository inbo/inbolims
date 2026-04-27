## LD Texture_processor COULTER files from LAB
## Programmed by Pieter Verschelde 9/06/2022
## adapted by Bruno De Vos
## readapted by Pieter Verschelde 28/08/2024

### IMPORTANT
### !!! Be sure to have VPN connection to link to LIMS system !!!

# load necessary libraries

library(dplyr)
library(jsonlite)
library(tidyverse)
library(DBI)
library(readxl)

# Download/update inbolims (core texture parsing functionalities)
remotes::install_github("inbo/inbolims")
library(inbolims)
getwd()

filename_1 <-
  file.path(
    "debugdata",
    "V-26V057-21_Ruwe Data LD_1_werkt_wel.txt"
  )

filename_2 <-
  file.path(
    "debugdata",
    "V-26V057-21_Ruwe Data LD_2_werkt_niet.txt"
  )

# output path
target_dir_1 <- "./output/f1"
target_dir_2 <- "./output/f2"
dir.create(target_dir_1)
dir.create(target_dir_2)

# db connection
conn <- lims_connect() # connect to dwh to link lab id

# main loop parsing (file1)
tparse_1 <- parse_texture_content(filename_1, delim = "\t")
tinterp_1 <- interpret_texture_content(tparse_1)
tlinked_1 <- link_labo_id(conn, tinterp_1)
write_texture_files(target_dir_1, tlinked_1)


# main loop parsing (file2)
tparse_2 <- parse_texture_content(filename_2, delim = "\t")
tinterp_2 <- interpret_texture_content(tparse_2)
tlinked_2 <- link_labo_id(conn, tinterp_2)
write_texture_files(target_dir_2, tlinked_2)

### json conversie


# file list ophalen

files_list_out_1 <-
  list.files(target_dir_1,
    pattern = ".csv",
    full.names = TRUE
  )

# Voor de eerste file de csv naar json convertern

for (j in 1:length(files_list_out_1)) {
  tmp <- read.csv2(files_list_out_1[j])
  tmp_uni <- distinct(tmp) # remove all duplicate rows
  write.csv2(tmp_uni, files_list_out_1[j], row.names = FALSE)
  tex_csv_2_json(files_list_out_1[j])
}
