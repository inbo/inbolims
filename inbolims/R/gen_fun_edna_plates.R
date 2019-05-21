#' Zet projectstalen klaar voor op de plaat
#'
#' @param data datset met projectstalen. Bevat minstens de kolom DNA_ID, DNA_RUN_ID, SAMPLE_NUMBER, REP_SAMPLE_NUMBER
#' @param rep_fractie welk aandeel van de stalen (tussen 0 en 1) wordt gebruikt om substalen te maken
#' @param plate_size grootte van de plaat. Typisch 96
#' @param column_size lengte van de plaatkolommen. Typisch 8 voor een 96-well plaat
#' @param n_qcm aantal qc methods per plaat
#' @param n_blank aantal blanco's per plaat
#' @param max_reps maximaal aantal keer dat een staal gerepliceerd mag worden in de plaat. Dus elk van de rep_fractie stalen kan maximaal max_reps keer als substaal voorkomen. In het totaal komt het staal dus maximaal  1 + max_reps keer voor op de plaat
#' @param min_filled hoeveel elementen van een plaat moeten gevuld worden, om een nieuwe plaat waard te zijn. Indien de overflow van de vorige plaat minder dan 24 is, wordt het aantal substalen gereduceerd, zodat geen nieuwe plaat nodig is.
#' @importFrom rlang .data
#' @import dplyr
#' @return dataset die klaargezet is om in de functie set_edna_plate_positions te gebruiken
#' @export
get_edna_plate_data <- function(data, rep_fractie = 0.10, plate_size = 96, column_size = 8, n_qcm = 1, n_blank = 3, 
                                max_reps = 2, min_filled = 24) {
  
  ### >>> staalkeuze 
  
  free_size <- plate_size - n_qcm - n_blank #vrije staalposities per plaat
  rep_keuze <- sample(1:nrow(data), size = ceiling(nrow(data) * rep_fractie) + 1) #(replicatekeuze + 1reservestaal)
  
  dfReps1 <- 
    data %>% 
    slice(rep_keuze[-length(rep_keuze)])
  
  dfReserve <- 
    data %>%
    slice(rep_keuze[length(rep_keuze)])
  
  dfReps1 <- data[rep_keuze[-length(rep_keuze)], ] #1 reservestaal toegevoegd
  dfReserve <- data[rep_keuze[length(rep_keuze)], , drop = FALSE] 
  
  #Herhaal deze stalen tot aan max_reps (indien 2: origineel + 1 rep  (rep1) + 1 rep (rep2))
  dfReps2 <- NULL
  for (k in 2:max_reps) {
    dfReps2 <- bind_rows(dfReps2, dfReps1)
  }
  
  ### >>> staalkeuze relatief aan plaatvulling
  
  #Kijk hoeveel stalen er zijn, en hoeveel platen nodig zijn
  max_proj_samps <- nrow(data) + nrow(dfReps1) + nrow(dfReps2)
  max_plates <- ceiling(max_proj_samps / free_size)
  max_samples <- max_proj_samps + max_plates * (n_qcm + n_blank)
  max_reps2 <- nrow(dfReps2)
  
  #Hoeveel overschot hebben we na het vullen van de platen
  plate_overflow <- max_samples %% plate_size #let op, bevat ook de controlestalen (dus direct bv 5 ipv 1)
  
  #Wat doen we als er overflow is. Hou rekening dat de controlestalen QC en blank ook moeten in rekening gebracht worden
  if (plate_overflow > 0 & plate_overflow < min_filled) {
    if (nrow(dfReps2) >= (plate_overflow - n_qcm - n_blank)) { #er verdwijnt immers een plaat dus ook de controles
      dfReps2 <- dfReps2[sample(1:nrow(dfReps2), size = nrow(dfReps2) - (plate_overflow - n_qcm - n_blank)), , drop = FALSE] 
    } else {
      #do nothing
    }
  }
  
  #Hoeveel platen zouden we nodig hebben zonder de QCM en blanks
  n_plates_tmp <- ceiling((nrow(data) + nrow(dfReps1) + nrow(dfReps2)) / (plate_size - n_qcm - n_blank))
  
  #Wanneer er overflow is binnen een kolom, gooi enkele stalen uit dfReps2 weg, of als er 7 zijn voeg het reservestaal toe
  col_overflow <- (nrow(data) + nrow(dfReps1) + nrow(dfReps2) + (plate_size - free_size) * n_plates_tmp) %% column_size
  if (col_overflow > 0 & col_overflow < 7) {
    if (nrow(dfReps2) >= col_overflow) {
      dfReps2 <- dfReps2[sample(1:nrow(dfReps2), size = nrow(dfReps2) - col_overflow), , drop = FALSE]
    }
  } else if (col_overflow == 7) {
    dfReps2 <- rbind(dfReps2, dfReserve) #indien 1 staal tekort gebruik het reservestaal
  } else {
    #do nothing
  }
  
  ### >>> Data randomiseren
  
  dfReps <- rbind(dfReps1, dfReps2)
  dfReps$origsample <- dfReps$sample
  dfReps$sample <- -9 #om aan te duiden dat dit een replicate is
  dfPlateFillNoOrder <- rbind(data, dfReps) #zou een veelvoud van 92 en een rest van 4 bij %%8 moeten zijn bij volle platen
  
  #sorteer de stalen willekeurig
  #waar er gaten zijn komen de replicatiestalen, de andere stalen worden oplopend gesorteerd voor het labogemak
  volgorde <- sample(1:nrow(dfPlateFillNoOrder))
  volgorde[volgorde %in%  (1:nrow(data))] <- 1:nrow(data)
  
  dfPlateFill <- 
    dfPlateFillNoOrder %>% 
    mutate(type = ifelse(.data$origsample > 0, "D", "N")) %>%
    slice(volgorde)
  
  n_plates <- ceiling(nrow(dfPlateFill) / free_size)
  
  dfPlateFill
  
}

#################################################


#' Data verder klaarzettten voor export naar lims platen
#'
#' @param data dataset komende uit de functie get_edna_plate_data
#' @param n_plates aantal platen die gevuld moeten worden
#' @param plate_size grootte van de plaat (standaard 96)
#' @param column_size  aantal lanes (standaard 8 voor een 96 wells plate)
#' @param qcm_pos posities van de qc_methods
#' @param n_blank aantal blanco's per plaat
#' @importFrom rlang .data
#' @import dplyr
#'
#' @return dataset die kan weggeschreven worden in C_DNA_EXTRACTION waar Lims dan verder mee aan de slag kan
#' @export
set_edna_plate_positions <- function(data, n_plates, plate_size = 96, column_size = 8, qcm_pos = 38, n_blank = 3) {
  
  ### >>> Maak lege plaat aan en vul de posities van de controlestalen (Q en B) in

  dfPlates <- 
    expand.grid(Plate = 1:n_plates, Position = 1:plate_size, type = NA, sample = NA, origsample = NA) %>%
    arrange(.data$Plate, .data$Position) %>%
    mutate(type = ifelse(.data$Position %in% qcm_pos, "Q", .data$type))
  for (i in 1:n_plates) {
    blank_candi <- 1:plate_size
    blank_pos <- sample(blank_candi[-qcm_pos], size = n_blank)
    dfPlates <- 
      dfPlates %>%
      mutate(type = ifelse(.data$Position %in% blank_pos & .data$Plate == i, "B", .data$type))
  }
  
  ### >>> Splits de plaat op in referentiestalen en echte stalen
  
  dfPlateRefs <- dfPlates %>%
    filter(!is.na(.data$type)) %>% 
    mutate(sample = ifelse(.data$type == "B", -1, 
                           ifelse(.data$type == "Q", -2, NA)))
  
  dfPlateSamps <- dfPlates %>% 
    filter(is.na(.data$type)) %>% 
    select(.data$Plate, .data$Position) %>%
    slice(1:nrow(data)) %>% 
    bind_cols(data)
  
  dfPlateAll <- dfPlateRefs %>%
    bind_rows(dfPlateSamps) %>% 
    arrange(.data$Plate, .data$Position)
  
  #De laatste plaat moet nog eens gerandomiseerd worden
  dfPlateLast  <- filter(dfPlateAll, .data$Plate == max(.data$Plate))
  dfPlateAllmin1 <- filter(dfPlateAll, .data$Plate < max(.data$Plate))
  
  ### >>> Randomiseer de laatste plaat
  
  #indien de plaat minder plaatsen bevat dan waar de QC method staat, randomiseer alle staaltypes
  if (nrow(dfPlateLast) < qcm_pos[length(qcm_pos)]) {
    whi_normal <- which(dfPlateLast$type %in% c("N", "D"))
    whi_qc <- which(dfPlateLast$type %in% c("Q", "B"))
    randomisation <- sample(1:nrow(dfPlateLast))
    dfPlateLastR <- dfPlateLast %>%
      slice(randomisation) %>% 
      mutate(Position = 1:nrow(dfPlateLast))
    
    #sorteer de normale stalen volgens staalnummer zoals in dfPlateLast
    dfPlateLastR$sample[dfPlateLastR$type %in% c("N","D")] <- dfPlateLast$sample[whi_normal]
    dfPlateLastR$origsample[dfPlateLastR$type %in% c("N","D")]  <- dfPlateLast$origsample[whi_normal]
    dfPlateLastR$type[dfPlateLastR$type %in% c("N","D")]  <- dfPlateLast$type[whi_normal]
    dfPlateAll <- bind_rows(dfPlateAllmin1, dfPlateLastR)
    
    #indien de QC method niet op de laatste positie staat, omdat de plaat verder gevuld is dan de QCM positie 
    #dan worden enkel de blanco's gerandomiseerd
  } else {
    blank_pos <- dfPlateLast %>% filter(.data$type == "B" & Position > qcm_pos[length(qcm_pos)]) %>% pull(.data$Position)
    
    #indien er blanco's voorkomen nadat de QC method voorkomt
    if (length(blank_pos) > 0) {
      dfPlateLastR <- dfPlateLast %>% mutate(Position = 1:nrow(dfPlateLast))
      whi_normal <- which(dfPlateLast$type %in% c("N", "D"))
      whi_qc <- which(dfPlateLast$type %in% "Q")
      if (length(whi_qc)) {
        randomisation <- sample((1:nrow(dfPlateLast))[-whi_qc])
      } else {
        randomisation  <- sample(1:nrow(dfPlateLast))
      }
      dfPlateLastR$sample[-whi_qc] <- dfPlateLast$sample[randomisation]
      dfPlateLastR$origsample[-whi_qc] <- dfPlateLast$origsample[randomisation]
      dfPlateLastR$type[-whi_qc] <- dfPlateLast$type[randomisation]
      
      dfPlateLastR$sample[dfPlateLastR$type %in% c("N","D")] <- dfPlateLast$sample[whi_normal]
      dfPlateLastR$origsample[dfPlateLastR$type %in% c("N","D")]  <- dfPlateLast$origsample[whi_normal]
      dfPlateLastR$type[dfPlateLastR$type %in% c("N","D")]  <- dfPlateLast$type[whi_normal]
      
      dfPlateAll <- bind_rows(dfPlateAllmin1, dfPlateLastR)  
      
      #indien alle blanco's voor de QC method komen, is extra randomisatie niet meer nodig  
    } else {
      dfPlateAll <- bind_rows(dfPlateAllmin1, dfPlateLast)
    }
  }
  dfPlateAll
}


