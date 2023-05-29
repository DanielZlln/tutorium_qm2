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
bodenflaeche[2,3] <- "Fläche insgesamt"
bodenflaeche[2,4] <- "Siedlung"
bodenflaeche[2,5] <- "Verkehr"
bodenflaeche[2,6] <- "Vegetationsfläche"
bodenflaeche[2,14] <- "Gewässer"
bodenflaeche <- bodenflaeche[-1,]
bodenflaeche <- bodenflaeche[-2,]
bodenflaeche <- bodenflaeche[1:539,]
bodenflaeche <- header.true(bodenflaeche)

bodenflaeche <- bodenflaeche[, -7:-13]
bodenflaeche <- bodenflaeche[,1:7]

names(bodenflaeche)[1] <- "ID"
names(bodenflaeche)[2] <- "Kreis"

