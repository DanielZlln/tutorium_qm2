library(tidyverse)
library(purrr)
library(corrplot)
library(corrgram)

source("ackerland.R")
source("beschaeftigte.R")
source("bevoelkerung.R")
source("bodenflaeche.R")
source("wahlbeteiligung.R")
source("tourismus.R")
source("siedlungsfläche.R")


df_list <- list(bodenflaeche, siedlungsflaeche, ackerland, beschaeftigte, bevoeklerung, 
                wahlbeteiligung, tourismus)

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

