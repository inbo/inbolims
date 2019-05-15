###################################################
###########   QC CHARTS       #####################
###################################################


### >>> Configure R session 

library(dplyr)
library(ggplot2)
library(inbolims)

args <- inbolims::prepare_session()
conn <- inbolims::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
params <- inbolims::read_db_arguments(conn, args["call_id"])


### >>> Declare variables

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

num <- 
  params %>% 
  filter(ARG_NAME == "NUM") %>% 
  pull(VALUE)


### >>> Execute Core Code

#Haal de resultaten uit de databank
dfResultaten <- inbolims::select_control_samples(conn, num, batch, analyse, components)

#Maak het HTML rapport
inbolims::html_qc_report(dfResultaten)

