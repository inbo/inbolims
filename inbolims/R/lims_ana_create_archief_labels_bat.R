
#' Creëer het .bat bestand waarin R de opdracht krijgt de archieflabels Access DB in te vullen
#'
#' @param bat_file the name including path where the .bat file should be created
#' @param db_location the name including path to the database for the printable raport
#' @param R_location the path to the R-32 bit version.  (paste0(R.home, "/bin/i386/Rscript.exe")) is often OK
#' @param Rscript_location the name including path where the .R script is that the .bat file will execute
#'
#' @return in het gekozen Access bestand wordt de tabel tblTemp ingevuld. De View QueryEtikettenPieter zal deze data dan aanspreken en als bron dienen voor een voorgedefinieerd Access rapportje.
#' @export
#'
#' @examples
#' \dontrun{
#' db_locatie <- system.file("extdata", "Archief_Labels_voorbeeldDB.accdb", package = "inbolims")
#' R_locatie <- paste0(R.home, "/bin/i386/Rscript.exe")
#' bat_bestand <- "Archieflabels.bat"
#' Rscript <- system.file("extdata", "Archief_Labels.R", package = "inbolims")
#' create_archief_labels_bat(bat_bestand, db_locatie, R_locatie, Rscript)
#' }
create_archief_labels_bat <- function(bat_file, db_location, R_location, Rscript_location){
  cat(paste0("TIMEOUT /T 1\n", 
             "Set /p Input=Plak de URL:",
             "\n\n",
             '"', R_location, '"',  ' "', Rscript_location, '"', " %Input%", ' "', db_location, '"',  
             "\n",
             "TIMEOUT /T 5"
             ), 
      file = bat_file)
}

