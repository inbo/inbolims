#' Parste texture output file to R readable format
#'
#' @param filename path to the output file to be parsed
#' @param delim delimiter of the file
#' @param verbose moet output getoond worden tijdens de uitvoering?
#'
#' @return parsed dataset with clear column names and data
#' @import magrittr
#' @importFrom readr read_delim
#' @importFrom dplyr lead
#' @export
parse_texture_content <- function(filename, delim = "\t", verbose = TRUE) {
  header <- readLines(con = filename, n = 7) # lees de 6 headerrijen + 1 datarij
  textuur <- read_delim(
    file = filename,
    delim = "\t",
    skip = 6,
    col_names = FALSE
  )

  headersplitted <- unlist(strsplit(header[3], split = "\t"))
  headersplitted[1] <- "lower_bound" # niet nodig maar voor duidelijkheid

  samplename_pos <- seq(2, length(headersplitted), by = 3)
  samples <- gsub("\\.\\$av", "", headersplitted[samplename_pos])
  last_underscores <- sapply(gregexpr("\\_", samples), max)
  samples <- substring(samples, 1, last_underscores - 1)

  header_names <- rep("", length(headersplitted)) # init lege rijen
  header_names[1] <- "lower_boundary"
  header_names[samplename_pos] <- paste(samples, "value", sep = "___")
  header_names[samplename_pos + 1] <- paste(samples, "LCL1S", sep = "___")
  header_names[samplename_pos + 2] <- paste(samples, "UCL1S", sep = "___")

  colnames(textuur) <- header_names

  textuur <- textuur %>%
    mutate(upper_boundary = c(lead(.data$lower_boundary, 1)))
  if (verbose) {
    cat(
      "\nOK\nDataset geimporteerd: \n",
      "dimensies: ",
      nrow(textuur), "x", ncol(textuur), "\n"
    )
  }
  return(textuur)
}


##############


#' Interpreteer de geparste textuurfile
#'
#' @param textuurdata geparste textuurdata
#' @param verbose moet output getoond worden tijdens de routine
#' @param digits aantal digits voor de waarde en sd
#' @import dplyr
#' @importFrom tidyr pivot_longer separate
#' @import magrittr
#'
#' @return tidy dataset met alle gegevens
#' @export
#'
interprate_texture_content <-
  function(textuurdata, verbose = TRUE, digits = 3) {
  textuur_long <- textuurdata %>%
    pivot_longer(cols = -ends_with("boundary")) %>%
    separate(.data$name, into = c("sample", "param"), sep = "___")

  textuur_wide <- textuur_long %>%
    pivot_wider(
      id_cols = c(
        .data$lower_boundary,
        .data$upper_boundary,
        .data$sample
      ),
      names_from = .data$param,
      values_from = .data$value
    ) %>%
    mutate(
      value = round(.data$value, digits),
      sd = round(.data$UCL1S - .data$value, digits),
      lower_boundary = round(.data$lower_boundary, 2),
      upper_boundary = round(.data$upper_boundary, 2)
    ) %>%
    select(c(1:4, 7, 5, 6))

  if (verbose) {
    cat(
      "\nOK\ndataset geinterpreteerd:\n",
      "dimensies: ", nrow(textuur_wide), "x", ncol(textuur_wide), "\n",
      "aantal stalen: ", length(unique(textuur_wide %>% pull(sample))), "\n"
    )
  }
  return(textuur_wide)
}

####################

#' Link het labo_id met het externe id
#'
#' @param conn db connectie
#' @param data dataset waarvan het labo_id gelinkt moet worden aan het extern id
#' @param labo_id_col naam van de kolom die het labo_id bevat
#' @param extern_id_col naam van de kolom waar je het externe id in wil
#' @importFrom DBI dbGetQuery
#'
#' @return dezelfde dataset met een extra kolom die het extern staalid bevat
#' @export
#' @import dplyr
#'
link_labo_id <- function(conn,
                         data,
                         labo_id_col = "sample",
                         extern_id_col = "FieldSampleID") {
  data$sample <- data[[labo_id_col]]

  # aanpassing omdat soms ---testnummer aan de sample gehangen wordt
  found_indices <- unlist(gregexpr("---", data$sample))
  last_index <- ifelse(found_indices < 0, nchar(data$sample), found_indices - 1)
  data$sample <- substring(data$sample, 1, last_index)

  qry <- paste0(
    "select sample = LabSampleID, \n",
    extern_id_col, " = FieldSampleID,  \n",
    "datum = AnalysisDate\n",
    " from dimSample s inner join factResult r ",
    "on s.SampleKey = r.SampleKey \n",
    " where s.LabSampleID in ",
    "('", paste(unique(data %>% pull(sample)),
      collapse = "','"
    ),
    "') and LimsAnalysisName like 'TEXTUUR_LD%' \n"
  )

  linktable <- dbGetQuery(conn, qry)

  returndata <- data %>%
    left_join(linktable, by = c("sample" = "sample"))

  if (labo_id_col != "sample") {
    returndata <- returndata %>% select(-sample)
  }

  returndata
}

###################

#' Converteer datumvector of POSIXct vector naar text in formaat yyyy-mm-dd
#'
#' @param x vector met datums (character)
#' @param na_result resultaat indien geen geldige datum
#'
#' @return character vector with datums
#' @export
#'
date_as_text <- function(x, na_result = "NODATE") {
  x[is.na(x) | substring(x, 1, 2) != "20"] <- NA
  x <- paste0(substring(x, 1, 10))
  x[is.na(x)] <- na_result
  x
}


###################


#' Schrijf textuurdata weg per staal in de target directory
#'
#' @param target_path locatie waar de files moeten terechtkomen
#' @param data textuurdata die weggeschreven moet worden
#' @param verbose toon output terwijl de routine loopt
#'
#' @return er wordt een file weggeschreven per aanwezig staal
#' @export
#' @importFrom dplyr filter select pull
#' @importFrom readr write_excel_csv2
#'
write_texture_files <- function(target_path, data, verbose = TRUE) {
  for (samp in unique(data %>% pull(.data$sample))) {
    tmp <- data %>%
      filter(.data$sample == samp) %>%
      transmute(
        .data$sample, .data$FieldSampleID,
        .data$lower_boundary, .data$upper_boundary,
        .data$value, .data$sd,
        .data$datum
      )
    if (any(is.na(tmp %>% pull(.data$FieldSampleID)))) {
      if (verbose) {
        print(paste0(samp, "niet weggeschreven want geen extern id"))
      }
      next
    }
    fn <- paste0(
      max(tmp %>% pull(.data$FieldSampleID)),
      "_",
      date_as_text(max(tmp %>% pull(.data$datum))),
      ".csv"
    )
    if (verbose) print(fn)
    write_excel_csv2(tmp %>% select(-.data$datum),
      file = file.path(target_path, fn)
    )
  }
  return()
}
