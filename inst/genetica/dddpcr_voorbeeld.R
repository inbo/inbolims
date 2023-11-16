library(tidyverse)
library(inbolims)

#3 voorbeelden

## voorbeeld 1
df_gmk <- read_csv("_NOT_GIT_/LOD_GMK new (Quentin)_22022019.csv")

ddPCR_qcplot(dfGMK, estim_formula = "connect")

## voorbeeld 2
df_gmk <- read_csv("_NOT_GIT_/LOD_Stonefly_27052019bis.csv",
                   na = c("", "No Call")) %>%
  filter(!is.na(CopiesPer20uLWell))

ddPCR_qcplot(dfGMK, estim_formula = "loess")

## voorbeeld 3
df_gmk <- read_csv("_NOT_GIT_/LOD_GMK oud (KWR)_22022019.csv",
                   na = c("", "No Call")) %>%
  filter(!is.na(CopiesPer20uLWell))

ddPCR_qcplot(df_gmk, estim_formula = "sigmoid")
