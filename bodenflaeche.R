library(readr)
library(readxl)
library(dplyr)

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

bodenflaeche <- read_xlsx("33111-01-02-4.xlsx", skip = 1)
bodenflaeche <- bodenflaeche[-4,]
bodenflaeche <- bodenflaeche[-1,]
bodenflaeche <- bodenflaeche[-3,]
bodenflaeche[2,3] <- "Fläche Insgesamt"
bodenflaeche[2,4] <- "Siedlung"
bodenflaeche[2,5] <- "Verkehr"
bodenflaeche[2,6] <- "Vegetation"
bodenflaeche[2,14] <- "Gewässer"
bodenflaeche <- bodenflaeche[-1,]
bodenflaeche <- bodenflaeche[-2,]
bodenflaeche <- bodenflaeche[1:539,]
bodenflaeche <- header.true(bodenflaeche)

names(bodenflaeche)[1] <- "ID"
names(bodenflaeche)[2] <- "Kreis"
bodenflaeche[1,1] <- '0'
#bodenflaeche[,1] <- sapply(bodenflaeche[,1],as.numeric)

bodenflaeche$ID <- ifelse(nchar(bodenflaeche$ID) > 5, 
                          substring(bodenflaeche$ID, 0, 5), 
                          bodenflaeche$ID)
