library(readr)
test_that("texture parsing", {
  d1m <- suppressWarnings(
    read_tsv("../testdata/multiple_samples.txt",
      n_max = 6,
      col_names = FALSE
    )
  )
  d1 <- read_tsv("../testdata/multiple_samples.txt",
    col_names = FALSE,
    skip = 6
  )
  expect_gt(nrow(d1), 1)

  d2m <- suppressWarnings(
    read_tsv("../testdata/single_sample.txt",
      n_max = 6,
      col_names = FALSE
    )
  )
  d2 <- read_tsv("../testdata/single_sample.txt",
    col_names = FALSE,
    skip = 6
  )
  expect_gt(nrow(d2), 1)

  source_path <- "../testdata/"
  source_pattern <- "sample"
  target_path <- "../testdata/result/"

  list_fn <- list.files(
    path = source_path,
    pattern = source_pattern,
    full.names = TRUE
  )
  n_list_fn <- length(list_fn)

  ## Loop to process all files serially   ####
  for (i in 1:n_list_fn) {
    filename <- list_fn[i]

    # parse de file naar een geldige R dataset
    textuur_parsed <- parse_texture_content(filename, delim = "\t")

    # interpreteer de dataset tot een inhoudelijk bruikbaar formaat
    textuur_interpreted <- interpret_texture_content(textuur_parsed)

    # maak een connectie met het LIMS datawarehouse
    conn <- lims_connect() # connect to dwh
    textuur_linked <- link_labo_id(conn, textuur_interpreted)
    dim(textuur_linked)

    # schrijf de files weg in /R/OUT/LDTEX/
    write_texture_files(target_path, textuur_linked)
  }

  # importeer nieuw gegenereerde files
  single_new <-
    read_csv2(
      file.path(
        target_path,
        "CMON_P44652fb_laag_10_30_MG_2023_2023-09-29.csv"
      )
    )


  multiple_new <-
    read_csv2(
      file.path(
        target_path,
        "CMON_P1d22877_laag_0_10_MG_2023_2023-09-25.csv"
      )
    )

  # importeer eerder gegenereerde files
  single_old <-
    read_csv2(
      file.path(
        target_path,
        "single_sample_output.csv"
      )
    )
  multiple_old <-
    read_csv2(
      file.path(
        target_path,
        "multiple_sample_first_output.csv"
      )
    )

  # zijn ze gelijk?
  expect_equal(single_new, single_old)
  expect_equal(multiple_new, multiple_old)
})
