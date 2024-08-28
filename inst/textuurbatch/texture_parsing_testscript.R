## LD Texture_processor COULTER files from LAB
## Programmed by Pieter Verschelde 9/06/2022
## adapted by Bruno De Vos
## readapted by Pieter Verschelde 28/08/2024

### IMPORTANT
### !!! Be sure to have VPN connection to link to LIMS system !!!

#load necessary libraries

library(dplyr)
library(jsonlite)
library(tidyverse)
library(DBI)
library(readxl)

# Download/update inbolims (core texture parsing functionalities)
remotes::install_github("inbo/inbolims")
library(inbolims)
getwd()

# get input files
file_input_path <- "."
files_list <- list.files(file_input_path,
                         pattern = "V-24V057",
                         full.names = TRUE)
n_files <- length(files_list)

#output path
target_dir <- "./output"
dir.create(target_dir)

#db connection
conn <- lims_connect() #connect to dwh to link lab id

# main loop parsing
for (i in 1:n_files) {
  filename <- files_list[i]
  print(filename)
  textuur_parsed <- parse_texture_content(filename, delim = "\t")
  textuur_interpreted <- interpret_texture_content(textuur_parsed)
  conn <- lims_connect() #connect to dwh
  textuur_linked <- link_labo_id(conn, textuur_interpreted)
  write_texture_files(target_dir, textuur_linked)
}

#conversion output to json
files_list_out <- list.files(target_dir, pattern = ".csv", full.names = TRUE)
n_files_out <- length(files_list_out)

for (j in 1:n_files_out) {
  tmp <- read.csv2(files_list_out[j])
  tmp_uni <- distinct(tmp) #remove all duplicate rows
  write.csv2(tmp_uni, files_list_out[j], row.names = FALSE)
  tex_csv_2_json(files_list_out[j])
}
