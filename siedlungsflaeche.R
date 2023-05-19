library(readr)
library(readxl)
library(dplyr)

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

siedlungsflaeche <- read_xlsx("33111-02-01-4.xlsx", skip = 3)
siedlungsflaeche <- header.true(siedlungsflaeche)
siedlungsflaeche <- siedlungsflaeche[,-7]
siedlungsflaeche <- siedlungsflaeche[,-8]

names(siedlungsflaeche)[12] <- "Grünanlage"

siedlungsflaeche <- siedlungsflaeche[-1:-3,]

names(siedlungsflaeche)[1] <- "ID"
names(siedlungsflaeche)[2] <- "Kreis"

siedlungsflaeche <- siedlungsflaeche[,-3]

names(siedlungsflaeche)[3] <- "Siedlungsfläche Gesamt"

siedlungsflaeche <- siedlungsflaeche[1:538,]
