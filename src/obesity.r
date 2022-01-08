# load required libraries
library(tree)
library(shiny)
library(shinyWidgets)
library(bslib)

# set working directory
# setwd("C:/Users/User/obesity-estimation/src")

# load data
obesity_data <- read.csv("../data/ObesityDataSet_raw_and_data_sinthetic_edited_randomised.csv", header = TRUE, sep = ",", fill = TRUE, stringsAsFactors = TRUE)

# convert variables to correct types
variables <- c("FCVC", "NCP", "CH2O", "FAF", "TUE")

for (v in variables) {
  
  # round off synthetic data
  obesity_data[, v] <- as.integer(obesity_data[, v])
  
  # convert back to factor
  obesity_data[, v] <- as.factor(obesity_data[, v])
  
}

obesity_data[, "Age"] <- as.integer(obesity_data[, "Age"])
obesity_data[, "ObesityCode"] <- as.factor(obesity_data[, "ObesityCode"])

# show summary of data
summary(obesity_data)

# the splitting doesnt work bcs the synthetic data all at the end... solution: randomise rows in excel
obesity_data.train <- obesity_data[1:1477,]
obesity_data.test <- obesity_data[1478:2111,]

# calculate model
dtree <- tree(ObesityCode ~ Gender + Age + FAVC + FCVC + NCP + CAEC + CH2O + FAF + TUE + CALC, data = obesity_data)

# tune model 
## k subject to change, hable uses 5 normally
tuning <- cv.tree(dtree, K=5)

# find optimal number of leaves
t <- which.min(tuning$dev)
number_of_leaves <- tuning$size[t]

# output model
model <- prune.tree(dtree, best=number_of_leaves)

# run shiny app
runApp("app.r")
