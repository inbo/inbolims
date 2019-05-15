###################################################
###########   Randomise Plates ####################
###################################################

### >>> Configure R session 

library(dplyr)
library(inbolims)

args <- inbolims::prepare_session()
conn <- inbolims::limsdb_connect(uid = args["uid"], pwd = args["pwd"])
params <- inbolims::read_db_arguments(conn, args["call_id"])


### >>> Lees data in

DNA <- DBI::dbReadTable(conn = conn, name = "C_DNA_EXTRACTION")

### >>> Genereer de plaatdata

dfPlates <- inbolims::gen_ms_create_plates(DNA)

### >>> Schrijf de data weg in de databank

DBI::dbGetQuery(conn, "delete from C_DNA_EXTRACTION")
for (i in 1:nrow(dfPlates)) {
  DBI::dbGetQuery(conn, dfPlates$sql[i])
}

DBI::dbDisconnect(conn)

