

readCreds <- function(file = "dbcredentials.txt", additional_vars = NULL){
  cn <- read.table(file, stringsAsFactors = FALSE)
  arglist <- c("", "", "", "", "", cn[1,1], cn[2,1], cn[3,1])
  if (!is.null(additional_vars)) {
    for (i in 1:length(additional_vars)) {
      arglist[8 + i] <- additional_vars[i]
    }
  }
  arglist
}

