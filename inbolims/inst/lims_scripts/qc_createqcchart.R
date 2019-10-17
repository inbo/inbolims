###################################################
###########   QC CHARTS       #####################
###################################################


### >>> Configure R session 

library(dplyr)
library(ggplot2)
library(inbolims)

#args <- inbolims::prepare_session(call_id = 324) #voorbeeld. Deze regel code manueel uitvoeren om te  testen met de juiste call_id
args <- inbolims::prepare_session()
conn <- inbolims::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
params <- inbolims::read_db_arguments(conn, args["call_id"])

#indien volledig manueel, dus ook als je niet kan testen vanuit een gekende call_id
# params <- data.frame(ARG_NAME = c("BATCH", "ANALYSIS", "COMP_NAMES", "NUM"),
#                      VALUE = c("IC_AN-190430-1", "IC_ANIONEN", "PO4,SO4", 30))


### >>> Declare variables

analyse <- 
  params %>% 
  filter(ARG_NAME == "ANALYSIS") %>% 
  pull(VALUE)

components <- 
  params %>% 
  filter(ARG_NAME == "COMP_NAMES") %>% 
  pull(VALUE) %>% 
  strsplit(split = ",") %>% 
  unlist() %>%
  convert_to_simple_ascii()

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

