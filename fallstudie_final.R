library(tidyverse)
library(purrr)
library(corrplot)
library(corrgram)
library(recipes)
library(corrgram)
library(corrplot)
library(VIM)
library(WriteXLS)


source("ackerland.R")
source("beschaeftigte.R")
source("bevoelkerung.R")
source("bodenflaeche.R")
source("wahlbeteiligung.R")
source("tourismus.R")
source("siedlungsflaeche.R")

# Merge der Datensätze
df_list <- list(bodenflaeche, siedlungsflaeche, ackerland, beschaeftigte, bevoeklerung, 
                wahlbeteiligung, tourismus)

df <- Reduce(function(x,y) merge(x,y, by = c("ID","Kreis"), all = TRUE),
                     df_list)

# Umwandlung der Datentypen in numerisch
for(i in 1:ncol(df)) {
  if (colnames(df[i]) == "Kreis" || colnames(df[i]) == "ID") {
    df[,i] <- df[,i]
  } else {
    df[,i] <- as.numeric(df[,i]) 
  }
}

#Entfernen von männlich/weiblich, weil es in der Faktoranalyse zu Problemen führt
df <- df[, !colnames(df) %in% c("männlich", "weiblich")]


#Fehlende Werte ersetzen durch KNN-Imputation
imputed_data <- kNN(df)

df <- imputed_data[1:69]
imputation_indicators <- imputed_data[70:138]


# Umwandeln von absoluten Zahlen in Anteile
calculate_shares <- function(df, base_column_name, next_n_columns) {
  base_column <- which(colnames(df) == base_column_name)
  for (i in 1:next_n_columns) {
    df[[base_column + i]] <- df[[base_column + i]] / df[[base_column]] * 100
  }
  return(df)
}

df <- calculate_shares(df, "Fläche Insgesamt", 15)
df <- calculate_shares(df, "Siedlungsfläche Gesamt", 9)
df <- calculate_shares(df, "Ackerland insgesamt", 16)
df <- calculate_shares(df, "Niederlassungen", 4)
df <- calculate_shares(df, "Gültige Zweitstimmen", 7)
df <- calculate_shares(df, "Insg. Gästeankünfte", 2)
df <- calculate_shares(df, "Insg. Gästeübernachtungen", 2)

#Export der Datensätze

data <- df
save(data, imputation_indicators, file = "data.RData")

# == Analyseteil ==

load("data.RData")
df <- data

# ID und Kreise raus für numerische Analysen
df <- subset(df, select = -ID)
df <- subset(df, select = -Kreis)

#Basics
summary(df)

corrplot(cor(df), tl.cex = 0.5)


# Factor Analysis
fa <- factanal(df,factors = 10, scores = "Bartlett", lower = 0.03)

loadings <- t(as.matrix(fa$loadings))
corrplot(loadings, 
         method = "circle",
         tl.col="black")

# PCA
PC <- prcomp(df, 
             center = TRUE, 
             scale. = TRUE)

PC.var <- PC$sdev^2/ sum(PC$sdev^2)
barplot(PC.var/sum(PC.var), xlab = "HK", 
        ylab="Anteil erklärte Varinaz")


# K-Means
n = 10

centers <- 0

for (i in 1:n){
  
  km <- kmeans(df, centers = i, nstart = 10, iter.max = 30)
  
  agg_mean <- aggregate(df, by = list(km$cluster), mean)
  
  centers <- rbind(centers, data.frame(Solution = i, Cluster = 1:i,agg_mean[,-1]))
} 

#Export for Cluster-Viz
WriteXLS(centers, ExcelFileName = "centers.xlsx", SheetNames = NULL, row.names = FALSE, col.names = TRUE)

