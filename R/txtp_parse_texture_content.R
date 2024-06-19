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
#' @importFrom stringr str_split_1
#' @export
parse_texture_content <- function(filename, delim = "\t", verbose = TRUE) {
  header <- readLines(con = filename, n = 7) # lees de 6 headerrijen + 1 datarij
  textuur <- read_delim(
    file = filename,
    delim = "\t",
    skip = 6,
    col_names = FALSE
  )
  # formaat verschilt als er 1 staal is of als er meerdere zijn

  # MEERDERE STALEN
  if (substring(header[3], 1, 7) != "Channel") {
    headersplitted <- str_split_1(header[3], pattern = "\t")
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

    # ENKEL 1 STAAL
  } else {
    filenamerecord <- str_split_1(header[2], pattern = "\t")
    sample <- gsub("\\.\\$av", "", filenamerecord[2])
    last_underscores <- sapply(gregexpr("\\_", sample), max)
    sample <- substring(sample, 1, last_underscores - 1)

    header_names <- str_split_1(header[3], pattern = "\t")
    header_names[1] <- "lower_boundary"
    header_names[2] <- paste(sample, "value", sep = "___")
    header_names[3] <- paste(sample, "LCL1S", sep = "___")
    header_names[4] <- paste(sample, "UCL1S", sep = "___")
  }

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
