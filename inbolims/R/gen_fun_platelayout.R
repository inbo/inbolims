
#' Brondata voor plaatopvulschema klaarzetten
#'
#' @param data dataset die uit de tabel C_DNA_RUN_REPORT komt
#'
#' @return design dataset voor de plaatlayout
#' @import dplyr
#' @importFrom dplyr do mutate filter transmute arrange select
#' @importFrom tidyr spread
#' @export
gen_plate_setup_source <- function(data){
  Capilar <- DNA <- ID <- Ixno <- Lane <- MilliQ <- ParentAliquot <- ParentSample <- NULL
  Plate <- Plate_Seq <- SampleNumber <- SampleType <- Specimen <- NULL
  
  dfDesign <- 
    data %>%
    dplyr::transmute(Replicate = ID, 
                     Specimen = as.character(ifelse(Specimen == "leeg", NA, Specimen)),
                     Plate = factor(Plate, levels = sort(unique(Plate))),
                     Plate_Seq = as.numeric(Plate),
                     Capilar = LETTERS[Capilar],
                     Lane, 
                     SampleType = ifelse(is.na(SampleType) | SampleType == "", "N", as.character(SampleType)), 
                     SampleNumber,
                     MilliQ,
                     DNA,
                     ParentSample, 
                     ParentAliquot, 
                     ParentAliquotBis = ifelse(ParentAliquot == 0, SampleNumber, ParentAliquot),
                     Location = paste0(Plate_Seq, Capilar, Lane)) %>%
    dplyr::arrange(Plate, Lane, Capilar)
  
  allsamps <- dfDesign %>% select(SampleNumber, MilliQ, DNA)
  
  dfDesign <- 
    bind_cols(
      select(dfDesign, -MilliQ, -DNA),
      dfDesign %>%
        mutate(Nr = row_number()) %>%
        rowwise() %>%
        do({
          if (.[["SampleType"]] == "SUBSAMPLE") {
            pid <- which(allsamps$SampleNumber == .[["ParentAliquot"]])
            if (length(pid)) {
              mqparent <- allsamps[pid, "MilliQ"]
              dnaparent <- allsamps[pid, "DNA"]  
              rv <- data.frame(MilliQ = mqparent, DNA = dnaparent)
            } else {
              rv <- data.frame(MilliQ = .[["MilliQ"]], DNA = .[["DNA"]])        
            }
          } else {
            rv <- data.frame(MilliQ = .[["MilliQ"]], DNA = .[["DNA"]])   
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
#'
#' @return dataset klaar om te exporteren naar excel of naar de C_DNA_RUN_REPORT_RESULTS tabel
#' @export
gen_plate_create_report <- function(data,  Capilar = LETTERS[1:8], Lane = 1:12){
  
  Capilar <- Ixno <- DNA <- ID <- Lane <- Lane01 <- Lane02 <- Lane03 <- Lane04 <- Lane05 <- Lane06 <- NULL
  Lane07 <- Lane08 <- Lane09 <- Lane10 <- Lane11 <- Lane12 <- MilliQ <- ParentAliquot <- NULL
  ParentSample <- Plate <- PlateSeq <- Plate_Seq <- Samen <- SampleNumber <- SampleType <- Specimen <- value <- NULL
  
  dfResult <- 
    expand.grid(Plate = levels(data$Plate),
                Capilar = LETTERS[1:8],
                Lane = 1:12,
                Ixno    = 1:8, #8 locaties om iets te schrijven per cel
                value = NA,
                stringsAsFactors = F) %>%
    arrange(Plate, Lane, Capilar, Ixno) %>%
    mutate(Plate = factor(Plate, levels = levels(data$Plate)),
           PlateSeq = as.numeric(Plate),
           Capilar = as.character(Capilar), 
           Samen = ifelse(Ixno %in% 1:2, Ixno, 
                          ifelse(Ixno %in% 3:4, 3, 
                                 ifelse(Ixno %in% 5:6, 4, 5))))
  
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
    arrange(PlateSeq, Capilar, Lane, Ixno) %>%
    mutate(Lane = paste0("Lane", sprintf("%02d", Lane))) %>%
    group_by(Plate, PlateSeq, Capilar, Lane, Samen) %>%
    mutate(value = ifelse(is.na(value), "", value)) %>%
    summarize(value = paste(value, collapse = "  " ))
  
  dfResultWide <- 
    dfResult %>% 
    ungroup() %>%
    select(Plate, Capilar, Lane, Samen, value) %>%
    tidyr::spread(key = Lane, value = value) %>%
    transmute(ID = 1:nrow(.), Plate, Capilar, Lane01, Lane02, Lane03, Lane04, Lane05, Lane06, Lane07, Lane08, Lane09, Lane10, Lane11, Lane12)
  
  dfResultWide
}
