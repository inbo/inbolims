############################################################################
# PLAATLAYOUT GENEREREN VOR CRYSTAL REPORT
############################################################################

### >>> Configure R session 

library(dplyr)
library(inbolims)

args <- inbolims::prepare_session()
conn <- inbolims::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
params <- inbolims::read_db_arguments(conn, args["call_id"])

### >>> Data inlezen

dfDesignOrig <- DBI::dbReadTable(conn, "C_DNA_RUN_REPORT")

### >>> Brondata klaarzetten

dfDesign <- inbolims::gen_plate_setup_source(dfDesignOrig)


### >>> Resultatendataset aanmaken

dfResult <- inbolims::gen_plate_create_report(dfDesign, Capilar = LETTERS[1:8], Lane = 1:12)


### >>> Invullen Resultatendataset

DBI::dbGetQuery(conn, "delete from C_DNA_RUN_REPORT_RESULTS")
check <- DBI::dbWriteTable(conn, "C_DNA_RUN_REPORT_RESULTS", dfResultWide, append = TRUE)
write.csv2(file = "platereport.csv", dfResultWide)
