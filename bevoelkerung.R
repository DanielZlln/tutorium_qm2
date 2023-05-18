library(readr)
library(readxl)
library(dplyr)

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

bevoeklerung <- read_xlsx("12411-01-01-4.xlsx", skip = 3)

bevoeklerung <- bevoeklerung[-1,]

bevoeklerung <- header.true(bevoeklerung)

names(bevoeklerung)[2] <- "Kreis"
names(bevoeklerung)[1] <- "ID"
names(bevoeklerung)[3] <- "Insgesamt Bevölkerung"

bevoeklerung <- bevoeklerung[1:538,]