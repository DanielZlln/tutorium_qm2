library(readr)
library(readxl)
library(dplyr)

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

wahlbeteiligung <- read_xlsx("14111-01-04-4.xlsx", skip = 4)
#wahlbeteiligung <- header.true(wahlbeteiligung)


names(wahlbeteiligung)[1] <- "ID"
names(wahlbeteiligung)[2] <- "Kreis"
names(wahlbeteiligung)[3] <- "Wahlberechtigte"
names(wahlbeteiligung)[4] <- "Wahlbeteiligung"
names(wahlbeteiligung)[5] <- "Gültige Zweitstimmen"
wahlbeteiligung <- wahlbeteiligung[-2,]
wahlbeteiligung <- wahlbeteiligung[-1,]
wahlbeteiligung <- wahlbeteiligung[1:538,]
