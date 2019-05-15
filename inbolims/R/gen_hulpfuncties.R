
#####################################################################

#' Converteer een UTF bestand naar een ASCII bestand (nodig voor ABI toestel)
#'
#' @param filename relatiever verwijziging naar locatiepad bestand
#' @param verbose  toon tussentijdse output
#'
#' @return
#' overwrites the UTF-8 file as an ASCII file
#' @export
#'
#' @examples
#' options.old <- options()
#' options(encoding = "UTF-8")
#' filename <- "test_abi_acii.txt"
#' cat("this is an UTF file\n", file = filename) #open met Notepad++ of zo om encoding te zien
#' convert_ABI_to_ASCII(filename)
#' options(options.old)
# convert_ABI_to_ASCII <- function(file, verbose = FALSE, old_method = FALSE){
#   Data <- readLines(file)
#   options.old <- options()
#   options(encoding = "ASCII")
#   if (!old_method) {
#     Data <- iconv(Data, from = "", to = "ASCII")   
#     writeLines(Data, file)
#   } else {
#     Data <- iconv(Data, from = "", to = "ASCII")   
#     substr <- substring(Data[1],1,4)
#     if(substr != 3500 ){
#       if (verbose){
#         print("Modify")
#         print(Data[1])
#       }
#       Data[1] <- substring(Data[1],4,)
#       
#       if (verbose) print(Data[1])
#     }
#     writeLines(Data, file)
#     options(options.old)
#   }
#   invisible()
# }

############################################





