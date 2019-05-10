

lims_shewhart <- function(x, ...){
  UseMethod("lims_shewhart")
}

lims_shewhart.lims_data <- function(x, ...){
  lims_shewhart.default(x=x$NUMERIC_ENTRY, ...)

}

lims_shewhart.default <- function(x, center = NULL, sd = NULL, rules=lims_shewhart_rules, ...){
  if (is.null(center)) center <- mean(x)
  if (is.null(sd)) sd <- sd(x)

  plotdata <- lims_shewhart_rules(x, center, sd, runs = c(1,9,6,14,3,5,15,8))
  plotdata <- cbind(Nr = 1:length(x), plotdata)
  plotdata <- as.data.frame(plotdata)
  plotdata$RVIOL <- as.character(paste0(plotdata$rule01,plotdata$rule02,plotdata$rule03,plotdata$rule04,
                           plotdata$rule05,plotdata$rule06,plotdata$rule07,plotdata$rule08))

  plotdata
}


gg_lims_shewhart <- function(plotdata, red_rules = 1:2, orange_rules = 3:6, yellow_rules = 7:8, 
                             labels = 1:length(x), ...){
  if (!all(red_rules, orange_rules, yellow_rules) %in% 1:8) stop("chosen rules not valid, should be a vector of integers between 1 and 8")
  if (length(red_rules)){
    plotdata$RR <- as.numeric(rowSums(plotdata[, paste0("rule", sprintf("%02d",red_rules))])>0)
  } else {
    plotdata$RR <- 0
  }
  
  if (length(orange_rules)){
    plotdata$OR <- as.numeric(rowSums(plotdata[, paste0("rule", sprintf("%02d",orange_rules))])>0)
  } else {
    plotdata$OR <- 0
  }
  
  if (length(yellow_rules)){
    plotdata$YR <- as.numeric(rowSums(plotdata[, paste0("rule", sprintf("%02d",yellow_rules))])>0)
  } else {
    plotdata$YR <- 0
  }
  plotdata$color <- ifelse(plotdata$RR, "red", ifelse(plotdata$OR, "orange", ifelse(plotdata$YR, "yellow", "black")))
  plotdata
  
  
  g<- 
  ggplot(plotdata, aes(x = Nr, y = value)) +
  geom_line(aes(y = lcl_1s), lty=2, color = "gold") + geom_line(aes(y = ucl_1s), lty = 2, color = "gold") +
  geom_line(aes(y = lcl_2s), lty=2, color = "orange") + geom_line(aes(y = ucl_2s), lty = 2, color = "orange") +
  geom_line(aes(y = lcl_3s), lty=2, color = "red") + geom_line(aes(y = ucl_3s), lty = 2, color = "red") +
  geom_line(aes(y=center)) +
  geom_line(lty = 3) +
  geom_point(data = subset(plotdata, color == "black"), color = "darkgreen") +
  geom_point(data = subset(plotdata, color == "red"), color = "red", size = rel(3)) +
  geom_point(data = subset(plotdata, color == "orange"), color = "orange", size = rel(2)) +
  geom_point(data = subset(plotdata, color == "yellow"), color = "yellow") +
  xlab("") + ylab("M1")
  
  g
}


#Rule1: point outside 3sigma
qcc_rule01 <- function(x, lcl_3s, ucl_3s, run = 1){
  if (run != 1) stop("run for shewhart rule 01 must be exactly 1")
  x <= lcl_3s | x >= ucl_3s
}

#Rule2: 9 consecutive points on the same side of the center
qcc_rule02 <- function(x, center, run = 9){
  if (length(x) < run){
    return(rep(FALSE, length(x)))
  } else {
    centered_x <- x - center
    base_data <- zoo::rollapply(centered_x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y) abs(sum(sign(y), na.rm = TRUE)) >= run))    
  }
}

#Rule3: 6 consecutive points steadily increasing or decreasing
qcc_rule03 <- function(x, run = 6){
  if (length(x) <= run){
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run+1, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y) abs(sum(sign(c(tail(y, -1) - head(y, -1))), na.rm = TRUE)) >= (run)))    
  }
}

#Rule4: 14 consecutive points alternating up and down
qcc_rule04 <- function(x, run = 14){
  if (length(x) <= run){
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run+1, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    apply(base_data, 1, FUN = function(z) {
      tmp <- sign(tail(z, -1) - head(z, -1))
      tmp[is.na(tmp)] <- 0
      return(all(tmp == rep(c(-1,1),100)[1:run]) | (all(tmp == rep(c(1,-1),100)[1:run])))    
    })
  }
}

#Rule5: 2 out of 3 consecutive points more than 2 sigma in the same direction
qcc_rule05 <- function(x, lcl_2s, ucl_2s, run = 3){
  if (length(x) < run){
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y){
      v_upp <- sum(y >= ucl_2s, na.rm = TRUE)
      v_low <- sum(y <= lcl_2s, na.rm = TRUE)
      (v_low >= run-1) | (v_upp >= run-1)
    }) )   
  }
}

#Rule6: 4 out of 5 consecutive points are more than 1 sigma from the center line in the same direction
qcc_rule06 <- function(x, lcl_1s, ucl_1s, run = 5){
  if (length(x) < run){
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y){
      v_upp <- sum(y >= ucl_1s, na.rm = TRUE)
      v_low <- sum(y <= lcl_1s, na.rm = TRUE)
      (v_low >= run-1) | (v_upp >= run-1)
    }))   
  }
}

#Rule7: 15 consecutive points are within 1 sigma of the center line
qcc_rule07 <- function(x, lcl_1s, ucl_1s, run = 15){
  if (length(x) < run){
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y){
      sum(y <= ucl_1s & y >= lcl_1s, na.rm = TRUE) >= run
    }))    
  }
}

#Rule8: 8 consecutive points on either side of the center line not within 1 sigma
qcc_rule08 <- function(x, lcl_1s, ucl_1s, run = 8){
  if (length(x) < run){
    return(rep(FALSE, length(x)))
  } else {
    base_data <- zoo::rollapply(x, run, FUN = function(x)x, fill = c(NA, NA, NA), align = "right", partial = FALSE)
    return(apply(base_data, 1, function(y){
      sum((y >= ucl_1s) | (y <= lcl_1s), na.rm = TRUE) >= run
    }))   
  }

}

lims_shewhart_rules <- function(x, center, sd, runs = c(1,9,6,14,3,5,15,8), ...){
  lcl_3s <- center - 3*sd
  lcl_2s <- center - 2*sd
  lcl_1s <- center - 1*sd
  ucl_1s <- center + 1*sd
  ucl_2s <- center + 2*sd
  ucl_3s <- center + 3*sd
  rule01 <- qcc_rule01(x, lcl_3s, ucl_3s, run = runs[1])
  rule02 <- qcc_rule02(x, center = center, run = runs[2])
  rule03 <- qcc_rule03(x, run = runs[3])
  rule04 <- qcc_rule04(x, run = runs[4])
  rule05 <- qcc_rule05(x, lcl_2s, ucl_2s, run = runs[5])
  rule06 <- qcc_rule06(x, lcl_1s, ucl_1s, run = runs[6])
  rule07 <- qcc_rule07(x, lcl_1s, ucl_1s, run = runs[7])
  rule08 <- qcc_rule08(x, lcl_1s, ucl_1s, run = runs[8])
  cbind(value=x, lcl_3s, lcl_2s, lcl_1s, center, ucl_1s, ucl_2s, ucl_3s,
        rule01, rule02, rule03, rule04, rule05, rule06, rule07, rule08)
}

