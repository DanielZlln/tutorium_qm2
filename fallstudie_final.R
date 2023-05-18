library(tidyverse)
library(purrr)
library(corrplot)
library(corrgram)

source("ackerland.R")
source("beschaeftigte.R")
source("bevoelkerung.R")
source("bodenflaeche.R")
source("wahlbeteiligung.R")


df_list <- list(bodenflaeche, ackerland, beschaeftigte, bevoeklerung, wahlbeteiligung)

fallstudie <- Reduce(function(x,y) merge(x,y, by = c("ID","Kreis"), all = TRUE),
       df_list)

neu_fallstudie <- fallstudie

fallstudie <- for(i in 1:ncol(fallstudie)) {
  if (colnames(fallstudie[i]) == "Kreis" || colnames(fallstudie[i]) == "ID") {
    neu_fallstudie[,i] <- fallstudie[,i]
  } else {
    neu_fallstudie[,i] <- as.numeric(fallstudie[,i]) 
  }
}

fallstudie_num <- neu_fallstudie %>% 
  select(where(is.numeric))

exclude_na <- function(df) {
  for (i in 1:ncol(df)) {
    if (anyNA(df[[i]]) & is.numeric(df[[i]])) {
      for (j in 1:length(df[,i])) {
        if (is.na(df[j,i])) {
          df[j,i] <- mean(df[,i], na.rm = T) 
        }
      }
    }
  }
  return(df)
}


