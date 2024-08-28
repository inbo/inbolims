#' Converteer textuurfiles naar JSON
#'
#' Converteer de bestanden die uit het textuurtoestel komen naar een json-file
#'
#' @param fullfilename path of the file to be converted
#'
#' @return jsonfile
#' @importFrom jsonlite toJSON
#' @importFrom utils read.csv2
#' @export
#'
tex_csv_2_json <- function(fullfilename) {
  # read file
  textuur_csv <- read.csv2(fullfilename)

  # extract observation_date from filename
  nc <- nchar(fullfilename)
  obsdate <- substr(fullfilename, nc - 13, nc - 4)
  # making a list for json data export
  s_id <- unique(textuur_csv$FieldSampleID)
  lab_sample_code <- unique(textuur_csv$sample)
  observation_date <- obsdate
  analyse_variabele <- "FRAC.0.2000\u00B5m.ld.c0"
  metingen <- textuur_csv[, c(3:6)]

  textuur_list <- list(
    s_id = s_id,
    lab_sample_code = lab_sample_code,
    observation_date = observation_date,
    analyse_variabele = analyse_variabele,
    metingen = metingen
  )

  # convert to json in compact format
  textuur_json <- toJSON(textuur_list, pretty = FALSE) ###  INBOdem readin
  textuur_json
  # write output in same folder but with json extension
  fullfilename
  basefilename <- gsub("\\.csv$", "", fullfilename) ## remove extension
  jsonfilename <- paste0(basefilename, ".json")

  write(textuur_json, jsonfilename)
  return(textuur_json)
}
