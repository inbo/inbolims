library(testthat)
library(inbolims)

# This helper function checks if we are inside GitHub Actions
is_github_actions <- function() {
  Sys.getenv("GITHUB_ACTIONS") == "true"
}
if (!is_github_actions()) {
  test_check("inbolims")
}
