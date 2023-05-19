library(readr)
library(readxl)
library(dplyr)

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

touri_herk <- read_xlsx("45412-03-02-4-b.xlsx", skip = 6)

names(touri_herk)[1] <- "ID"
names(touri_herk)[2] <- "Kreis"
names(touri_herk)[3] <- "Insg. Gästeankünfte"
names(touri_herk)[4] <- "GA Inland"
names(touri_herk)[5] <- "GA Ausland"
names(touri_herk)[6] <- "Insg. Gästeübernachtungen"
names(touri_herk)[7] <- "GÜ Inland"
names(touri_herk)[8] <- "GÜ Ausland"

touri_herk <- touri_herk[1:489,]

touri_schlaf <- read_xlsx("45412-01-03-4.xlsx", skip = 2)
touri_schlaf <- touri_schlaf[,1:4]
touri_schlaf <- touri_schlaf[-2,]
touri_schlaf <- touri_schlaf[-1,]

names(touri_schlaf)[1] <- "ID"
names(touri_schlaf)[2] <- "Kreis"

touri_schlaf <- touri_schlaf[1:538,]

tourismus <- merge(x = touri_schlaf, y = touri_herk,
                   by = c("ID", "Kreis"), all.x = T)