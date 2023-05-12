library(readr)
library(readxl)
library(dplyr)

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

ackerland <- read_xlsx("41141-02-02-4.xlsx", skip = 3)
ackerland <- header.true(ackerland)
ackerland <- ackerland[-2,]
ackerland <- ackerland[-1,]
names(ackerland)[1] <- "ID"
names(ackerland)[2] <- "Kreis"
ackerland <- ackerland[1:538,]
