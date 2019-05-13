###################################################
###########   QC CHARTS       #####################
###################################################

### >>> CONFIGURE R SESSION ###
### ----------------------- ###

#setwd("C:/GIT_PROJECTS/inbolims/workdir") #om te testen
#set call_id to a value

library(inbolims)
library(dplyr)
library(ggplot2)
library(DBI)
library(odbc)
library(xtable)
library(plotly)

args <- commandArgs()
is_test <- length(args) < 5
if (is_test) {
  creds <- inbolims::read_db_credentials()
}  

dsn <- ifelse(is_test, creds$dsn, args[7])
uid <- ifelse(is_test, creds$uid, args[8])
pwd <- ifelse(is_test, creds$pwd, args[9])
callid <- ifelse(is_test, call_id, args[10])

conn <- inbolims::lims_db_connect(uid = uid, pwd = pwd)
params <- inbolims::read_db_arguments(conn, callid)

analyse <- 
  params %>% 
  filter(ARG_NAME == "ANALYSIS_NAME") %>% 
  pull(VALUE)

components <- 
  params %>% 
  filter(ARG_NAME == "COMP_NAMES") %>% 
  pull(VALUE) %>% 
  strsplit(split = ",") %>% 
  unlist()

batch <-  #batch nodig om controlestalen te zoeken
  params %>% 
  filter(ARG_NAME == "BATCH") %>% 
  pull(VALUE)


dfResultaten <- select_control_samples(conn, 30, batch, analyse, components)

html_qc_report(dfResultaten)

