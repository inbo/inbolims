
library(testthat)
library(inbolims)


#// creating golden standard datasets

create_fixtures <- FALSE
if (create_fixtures) {
  setwd(here::here("tests", "testthat", "fixtures"))
  conn <- lims_connect()
  staal_info <- lims_sample_information(conn, project = c("I-19W001-02")) |> 
    dplyr::filter(substring(LabSampleID, 1, 8) == "19-00010") |> 
    dplyr::arrange("Project", "LabSampleID")
  rapport_data <- read_lims_data(connection = conn, 
                                 project = c("I-19W001-02"), 
                                 sql_template = "default", 
                                 show_query = FALSE) |> 
    dplyr::filter(substring(LaboCode, 1, 8) == "19-00010") |> 
    dplyr::arrange(.data$LaboCode, .data$Sleutel)
  staaloverzicht <- lims_report_samples(rapport_data)
  kruistabel <- lims_report_xtab(rapport_data)
  lims_report_export(kruistabel, path = "expected_test_xtab.csv")
  saveRDS(staal_info, "expected_staal_info.rds")
  saveRDS(rapport_data, "expected_rapport_data.rds")
  saveRDS(kruistabel, "expected_kruistabel.rds")  
  saveRDS(staaloverzicht, "expected_staaloverzicht.rds") 
  setwd(here::here())
}

#// Test Suite for the LIMS Data Workflow ---

  test_that("LIMS data retrieval and reporting workflow matches expected output", {
    
    # 1. SKIP TEST IF NOT ON THE WORK NETWORK
    # This is the most critical part for your package checks.
    # `skip_if_not()` will halt execution of this test block if the condition is FALSE.
    skip_if_not(is_on_inbo_domain(), message = "Skipping LIMS tests: Not connected to the company Windows domain.")
    
    
    # 2. SETUP AND CLEANUP
    # `on.exit()` is a robust way to ensure cleanup code runs, even if tests fail.
    # This prevents leaving open connections or temporary files.
    connection <- NULL
    on.exit({
      # Disconnect from the database if the connection was successful.
      if (!is.null(connection) && DBI::dbIsValid(connection)) {
        DBI::dbDisconnect(connection)
      }
      # Delete any files created during the test.
      if (file.exists("test.csv")) file.remove("test.csv")
      if (file.exists("test_xtab.csv")) file.remove("test_xtab.csv")
    })
    
    
    # 3. RUN THE WORKFLOW AND TEST EACH STEP
    
    # Test lims_connect()
    connection <- tryCatch(
      lims_connect(),
      error = function(e) {
        fail(paste("lims_connect() failed with an error:", e$message))
        return(NULL)
      }
    )
    expect_true(DBI::dbIsValid(connection), info = "The database connection should be valid.")
    
    # Test lims_sample_information() against the saved "golden" file
    staal_info <- lims_sample_information(connection, project = c("I-19W001-02")) |> 
      dplyr::filter(substring(LabSampleID, 1, 8) == "19-00010") |> 
      dplyr::arrange(Project, LabSampleID)
    expected_staal_info <- readRDS("fixtures/expected_staal_info.rds")
    expect_equal(staal_info, expected_staal_info, info = "Sample information does not match the expected output.")
    
    # Test read_lims_data() against the saved "golden" file
    rapport_data <- read_lims_data(connection = connection, 
                                   project = c("I-19W001-02"), 
                                   sql_template = "default", 
                                   show_query = FALSE) |> 
      dplyr::filter(substring(LaboCode, 1, 8) == "19-00010") |> 
      dplyr::arrange(.data$LaboCode, .data$Sleutel)
    expected_rapport_data <- readRDS("fixtures/expected_rapport_data.rds")
    expect_equal(rapport_data, expected_rapport_data, info = "Rapport data does not match the expected output.")
    print(getwd())
    saveRDS(rapport_data, "rapport_data.RDS")
    saveRDS(expected_rapport_data, "expected_data.RDS")
    
    # Test lims_report_samples()
    # This function's output is derived from rapport_data, so if the input is correct,
    # we can just check the object type here, or create another .rds file if needed.
    staaloverzicht <- lims_report_samples(rapport_data)
    expected_staaloverzicht <- readRDS("fixtures/expected_staaloverzicht.rds")
    expect_s3_class(staaloverzicht, "data.frame")
    expect_equal(staaloverzicht, expected_staaloverzicht, info = "Sample Overview does not match the expected output")
    
    # Test lims_report_xtab() against the saved "golden" file
    kruistabel <- lims_report_xtab(rapport_data)
    expected_kruistabel <- readRDS("fixtures/expected_kruistabel.rds")
    expect_equal(kruistabel, expected_kruistabel, info = "Kruistabel does not match the expected output.")
    
    # Test lims_report_export() for both file creation and content
    lims_report_export(kruistabel, path = "test_xtab.csv")
    expect_true(file.exists("test_xtab.csv"), info = "Exported crosstab file should exist.")
    
    # Compare the content of the created CSV with the expected CSV
    created_xtab_csv <- read.csv("test_xtab.csv")
    expected_xtab_csv <- read.csv("fixtures/expected_test_xtab.csv")
    expect_equal(created_xtab_csv, expected_xtab_csv, info = "Exported xtab CSV content does not match expected.")
    
    # Test the second export for file existence
    lims_report_export(rapport_data, path = "test.csv")
    expect_true(file.exists("test.csv"), info = "Exported report data file should exist.")
  })
