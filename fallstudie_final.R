library(tidyverse)
library(purrr)
library(corrplot)
library(corrgram)
library(recipes)
library(corrgram)
library(corrplot)

source("ackerland.R")
source("beschaeftigte.R")
source("bevoelkerung.R")
source("bodenflaeche.R")
source("wahlbeteiligung.R")
source("tourismus.R")
source("siedlungsflaeche.R")


df_list <- list(bodenflaeche, siedlungsflaeche, ackerland, beschaeftigte, bevoeklerung, 
                wahlbeteiligung, tourismus)

fallstudie <- Reduce(function(x,y) merge(x,y, by = c("ID","Kreis"), all = TRUE),
       df_list)

for(i in 1:ncol(fallstudie)) {
  if (colnames(fallstudie[i]) == "Kreis" || colnames(fallstudie[i]) == "ID") {
    fallstudie[,i] <- fallstudie[,i]
  } else {
    fallstudie[,i] <- as.numeric(fallstudie[,i]) 
  }
}

str(fallstudie)

rec <- recipe(~., data = fallstudie)
?step_impute_knn

ratio_recipe <- rec %>% 
  step_impute_knn(all_predictors(), neighbors = 3)

ratio_recipe_2 <- prep(ratio_recipe)

fallstudie <- bake(ratio_recipe_2, new_data = NULL)

summary(fallstudie)

fallstudie_num <- fallstudie[sapply(fallstudie, is.numeric)]

sapply(fallstudie_num, class)

cor_matrix <- cor(fallstudie_num)

corrgram(cor_matrix)
corrplot(cor_matrix, order = "hclust")
