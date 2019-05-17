
#' Randomiseer data, kies substalen, en maak de platen aan
#'
#' @param DNA dataset met DNA_RUN_ID (1 cijfer die voor dit project gelijk blijft), SAMPLE_NUMBER (de sample_numbers uit LIMS), SAMPLE_TYPE (N = normaal sample, Q = QCmethod)
#' @param ... argumenten die doorgegeven worden aan de functie \link{randomiseCapilar}
#' @importFrom dplyr filter transmute mutate arrange bind_rows bind_cols left_join inner_join n pull
#'
#' @return dataset met naast de velden in DNA, de extra velden PLATE_SEQ (nummer van de plaat), PLATE_POSITION (positie op de plaat) en REP_SAMPLE_NUMBER (staalnummer waarvan dit een substaal is). Deze dataset moet per definitie meer rijen bevatten dan DNA, aangezien er replicates gelogd worden, en nieuwe controlestalen en blanco's indien de stalen over meerdere platen worden verdeeld
#' @export
#'
gen_ms_create_plates <- function(DNA, ...) {
  
  run_id <- DNA$DNA_RUN_ID[1]
  
  Capilar <- Group <- Lane <- Orig <- Plate <- QCsample <- Replicate <- SAMPLE_NUMBER <- SAMPLE_TYPE <- SampleNumber <- Specimen <- non_QC <- NULL
  
  #De originele staalnummers (die teruggelinkt zullen worden aan de tabel)
  
  unique_non_QC_orig <- 
    DNA %>% 
    dplyr::filter(SAMPLE_TYPE == "N") %>% 
    dplyr::transmute(non_QC = as.numeric(as.character(SAMPLE_NUMBER))) %>% 
    dplyr::arrange(non_QC)
  
  #defintie van bestaande QC stalen
  QC_samplenumber <- 
    DNA %>% 
    dplyr::filter(SAMPLE_TYPE == "Q") %>% 
    dplyr::transmute(QCsample = SAMPLE_NUMBER) %>%
    dplyr::arrange(QCsample) %>% 
    dplyr::pull(QCsample)
  
  if (length(QC_samplenumber) == 0) {
    QC_samplenumber <- -1
  }
  
  QCdata <- data.frame(Capilar = c("E", "F"), Line = c(5, 5), ID = c(0, QC_samplenumber), Type = c("B", "Q"))
  
  #Voer de randomisatiefunctie randomiseCapilar van Thierry uit
  #nCapilar, Lines, rReplicates, minReplicates, fillPlate, firstLabID, prefix via ... ingevuld
  datasetlist <- 
    inbolims::randomiseCapilar(
      Specimens = DNA$SAMPLE_NUMBER[DNA$SAMPLE_TYPE == "N"],
      Group = rep("N", sum(DNA$SAMPLE_TYPE == "N")),
      QC = QCdata
    )
  
  #Converteer datasetlist die uit randomiseCapilar komt naar een data.frame
  dataset <- 
    dplyr::inner_join(datasetlist$Specimens, datasetlist$Replicates, by = "Specimen") %>%
    dplyr::arrange(Plate, Lane, Capilar) %>% 
    dplyr::mutate(Group = ifelse(duplicated(Specimen) & Group == "N", "S", as.character(Group)),
                  Specimen = as.numeric(as.character(Specimen)),
                  Replicate = 1:n(),
                  Specimen_tmp = Specimen)
  
  #overview of non_QC_samples after
  unique_non_QC_after <- 
    dataset %>% 
    dplyr::filter(Group == "N") %>% 
    dplyr::transmute(Specimen = as.numeric(as.character(Specimen)))
  
  #Bind the original samples before and after together (Orig an After must be different)
  dfLink <- 
    dplyr::bind_cols(unique_non_QC_orig, unique_non_QC_after) %>% 
    dplyr::select(Orig = non_QC, After = Specimen) %>% 
    dplyr::bind_rows(data.frame(Orig = QC_samplenumber, After = QC_samplenumber))
  
  dataset <- 
    dataset %>%
    dplyr::left_join(dfLink, by = c("Specimen" = "After")) %>%
    dplyr::mutate(SampleNumber = ifelse(is.na(Orig) & Group == "B", 0, Orig)) %>%
    dplyr::arrange(SampleNumber, Plate, Lane, Capilar) %>% #check if N always before S per sample
    dplyr::mutate(sql = paste0("insert into C_DNA_EXTRACTION ",
                               "(DNA_ID, DNA_RUN_ID, SAMPLE_NUMBER, SAMPLE_TYPE, PLATE_SEQ, PLATE_POSITION)",
                               " values (",
                               Replicate, ",",
                               run_id, ",",
                               SampleNumber, ",",
                               "'", Group, "',", 
                               Plate, ",", 
                               "'",Capilar, Lane, "')")) %>%
    dplyr::filter(Group != "X")
  
  dataset
}
