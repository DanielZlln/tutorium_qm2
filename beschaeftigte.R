library(readr)
library(readxl)
library(dplyr)

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

beschaeftigte <- read_xlsx("52111-01-02-4.xlsx", skip = 3)
beschaeftigte <- header.true(beschaeftigte)
beschaeftigte <- beschaeftigte[-2,]
beschaeftigte <- beschaeftigte[-1,]
beschaeftigte <- beschaeftigte[1:538,]

names(beschaeftigte)[1] <- "ID"
names(beschaeftigte)[2] <- "Kreis"
names(beschaeftigte)[3] <- "Niederlassungen"
