library(readr)
library(here)

test_that("texture parsing", {
  d1m <- suppressWarnings(
    read_tsv(
      here(
        "tests", "testthat", "fixtures",
        "source_texture_multiple_samples.txt"
      ),
      show_col_types = FALSE,
      n_max = 6,
      col_names = FALSE
    )
  )
  expect_gt(nrow(d1m), 1)

  d2m <- suppressWarnings(
    read_tsv(
      here(
        "tests", "testthat", "fixtures",
        "source_texture_single_sample.txt"
      ),
      show_col_types = FALSE,
      n_max = 6,
      col_names = FALSE
    )
  )
  expect_gt(nrow(d2m), 1)

  source_path <- here("tests", "testthat", "fixtures")
  source_pattern <- "^source_texture.*sample.*\\.txt$"
  target_path <- here("tests", "testthat", "result")
  if (!dir.exists(target_path)) dir.create(target_path)

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
      ),
      show_col_types = FALSE
    )

  multiple_new <-
    read_csv2(
      file.path(
        target_path,
        "CMON_P1d22877_laag_0_10_MG_2023_2023-09-25.csv"
      ),
      show_col_types = FALSE
    )

  # importeer eerder gegenereerde files
  single_old <-
    read_csv2(
      file.path(
        source_path,
        "expected_texture_single_sample_output.csv"
      )
    )
  multiple_old <-
    read_csv2(
      file.path(
        source_path,
        "expected_texture_multiple_sample_first_output.csv"
      )
    )

  # zijn ze gelijk?
  expect_equal(single_new, single_old)
  expect_equal(multiple_new, multiple_old)
})
