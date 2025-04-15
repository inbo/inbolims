.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Zorg dat je op het INBO netwerk zit op een vestiging of via vpn.\n",
    "Eenvoudig gebruik van de code:\n\n",
    "  connection <- lims_connect()\n",
    "  rapport_data <- read_lims_data(connection, c(\"I-19W001-02\"))\n",
    "  kruistabel <- lims_report_xtab(rapport_data)\n\n",
    "To suppress this message, use 'suppressPackageStartupMessages(library(inbolims))'.\n"
  )
}