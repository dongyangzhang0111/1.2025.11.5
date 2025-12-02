
library(tidyverse)
library(broom)
library(readxl)
library(ggplot2)
library(dplyr)

data <- read_excel("C:/Users/Rrasia0059/Desktop/yintao7.xlsx")

y_vars <- c("OHdG","OHG","HNEMA","Alla","CEL","diY","OHGua","oTyrosine") 
x_vars <- c("LNAl","LNV","LNMg","LNLi","LNCr","LNFe","LNMn","LNCo","LNNi","LNMo","LNRb","LNSn","LNSr","LNCe","LNCu","LNAs","LNAg","LNCd","LNBa","LNSe","LNZn","LNTl","LNHg","LNPb") 
covars <- c("smoke","alc","age","edu","buyun","BMI","day")  

model_results <- list()
summary_results <- list()
tidy_results <- list()


for (y in y_vars) {

  if (y == "") next
  
  for (x in x_vars) {

    formula <- as.formula(paste(y, "~", x, "+", paste(covars, collapse = "+")))
    

    model_name <- paste(y, "by", x, sep = "_")
    model_results[[model_name]] <- lm(formula, data = data, na.action = na.exclude)
    

    summary_results[[model_name]] <- summary(model_results[[model_name]])

    tidy_results[[model_name]] <- tidy(model_results[[model_name]], conf.int = TRUE, conf.level = 0.95)
  }
}


print(summary_results[[1]])


all_tidy_results <- bind_rows(tidy_results, .id = "model")
head(all_tidy_results)

View(all_tidy_results)
write.csv(all_tidy_results,file = "linear model8.csv")
