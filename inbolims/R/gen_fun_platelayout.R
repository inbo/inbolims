
#' Brondata voor plaatopvulschema klaarzetten
#'
#' @param data dataset die uit de tabel C_DNA_RUN_REPORT komt
#'
#' @return design dataset voor de plaatlayout
#' @import dplyr
#' @importFrom dplyr do mutate filter transmute arrange select
#' @importFrom tidyr spread
#' @importFrom rlang .data
#' @export
gen_plate_setup_source <- function(data){

  dfDesign <- 
    data %>%
    dplyr::transmute(Replicate = .data$ID, 
                     Specimen = as.character(ifelse(.data$Specimen == "leeg", NA, .data$Specimen)),
                     Plate = factor(.data$Plate, levels = sort(unique(.data$Plate))),
                     Plate_Seq = as.numeric(.data$Plate),
                     Capilar = LETTERS[.data$Capilar],
                     .data$Lane, 
                     SampleType = ifelse(is.na(.data$SampleType) | .data$SampleType == "", "N", 
                                         as.character(.data$SampleType)), 
                     .data$SampleNumber,
                     .data$MilliQ,
                     .data$DNA,
                     .data$ParentSample, 
                     .data$ParentAliquot, 
                     ParentAliquotBis = ifelse(.data$ParentAliquot == 0, .data$SampleNumber, .data$ParentAliquot),
                     Location = paste0(.data$Plate_Seq, .data$Capilar, .data$Lane)) %>%
    dplyr::arrange(.data$Plate, .data$Lane, .data$Capilar)
  
  allsamps <- dfDesign %>% select(.data$SampleNumber, .data$MilliQ, .data$DNA)
  
  dfDesign <- 
    bind_cols(
      select(dfDesign, -.data$MilliQ, -.data$DNA),
      dfDesign %>%
        mutate(Nr = row_number()) %>%
        rowwise() %>%
        do({
          if (.data$SampleType == "SUBSAMPLE") {
            pid <- which(allsamps$SampleNumber == data$ParentAliquot)
            if (length(pid)) {
              mqparent <- allsamps[pid, "MilliQ"]
              dnaparent <- allsamps[pid, "DNA"]  
              rv <- data.frame(MilliQ = mqparent, DNA = dnaparent)
            } else {
              rv <- data.frame(MilliQ = .data$MilliQ, DNA = .data$DNA)        
            }
          } else {
            rv <- data.frame(MilliQ = .data$MilliQ, DNA = .data$DNA)   
          }
          rv
        }))
  dfDesign
}

##########################################################


#' Maak een plaatlayout rapport aan
#'
#' @param data datasset komende uit gen_plate_setup_source
#' @param Capilar de codes van de plaatkolommen, typisch A tot F
#' @param Lane de nummers van de plaatrijen, typisch 1 tot 12
#' @import dplyr
#' @importFrom dplyr do mutate filter transmute arrange select
#' @importFrom tidyr spread
#' @importFrom rlang .data
#'
#' @return dataset klaar om te exporteren naar excel of naar de C_DNA_RUN_REPORT_RESULTS tabel
#' @export
gen_plate_create_report <- function(data,  Capilar = LETTERS[1:8], Lane = 1:12){
  
  dfResult <- 
    expand.grid(Plate = levels(data$Plate),
                Capilar = Capilar,
                Lane = Lane,
                Ixno    = 1:8, #8 locaties om iets te schrijven per cel
                value = NA,
                stringsAsFactors = F) %>%
    arrange(.data$Plate, .data$Lane, .data$Capilar, .data$Ixno) %>%
    mutate(Plate = factor(.data$Plate, levels = levels(data$Plate)),
           PlateSeq = as.numeric(.data$Plate),
           Capilar = as.character(.data$Capilar), 
           Samen = ifelse(.data$Ixno %in% 1:2, .data$Ixno, 
                          ifelse(.data$Ixno %in% 3:4, 3, 
                                 ifelse(.data$Ixno %in% 5:6, 4, 5))))
  
  for (i in 1:nrow(data)) {
    tmp <- as.data.frame(data[i, , drop = FALSE])
    #zou telkens 8 elementen moeten weergeven
    selectie <- which(dfResult$PlateSeq == tmp$Plate_Seq & dfResult$Capilar == tmp$Capilar & dfResult$Lane == tmp$Lane)
    dfResult[selectie[1],"value"] <- tmp$Specimen
    dfResult[selectie[7],"value"] <- paste("M", tmp$MilliQ)
    dfResult[selectie[8],"value"] <- paste("D", tmp$DNA)
    
    if (tmp$SampleType == "SUBSAMPLE") {
      specimen_parent <- data$Specimen[tmp$ParentAliquot == data$SampleNumber]
      if (length(specimen_parent == 1)) {
        dfResult[selectie[2], "value"] <- paste0("(",specimen_parent, ")")      
      }
    }
    corresp <- which(data$ParentAliquotBis == tmp$ParentAliquotBis)
    corresp <- corresp[corresp != i]
    if (length(corresp)) {
      locs <- paste0(data[corresp, "Plate_Seq"], data[corresp, "Capilar"], sprintf("%02d",data[corresp, "Lane"]))
      for (k in 1:max(4,length(locs))) {
        dfResult[selectie[2 + k], "value"] <- locs[k]
      }
    }
  }
  
  dfResult <- 
    dfResult %>% 
    arrange(.data$PlateSeq, .data$Capilar, .data$Lane, .data$Ixno) %>%
    mutate(Lane = paste0("Lane", sprintf("%02d", .data$Lane))) %>%
    group_by(.data$Plate, .data$PlateSeq, .data$Capilar, .data$Lane, .data$Samen) %>%
    mutate(value = ifelse(is.na(.data$value), "", .data$value)) %>%
    summarize(value = paste(.data$value, collapse = "  " ))
  
   dfResultWide <- 
     dfResult %>% 
     ungroup() %>%
     select(.data$Plate, .data$Capilar, .data$Lane, .data$Samen, .data$value) %>%
     tidyr::spread(key = .data$Lane, value = .data$value)  %>%
     transmute(ID = 1:n(), .data$Plate, .data$Capilar, 
               .data$Lane01, .data$Lane02, .data$Lane03, .data$Lane04,
               .data$Lane05, .data$Lane06, .data$Lane07, .data$Lane08,
               .data$Lane09, .data$Lane10, .data$Lane11, .data$Lane12)
  
  dfResultWide
}
