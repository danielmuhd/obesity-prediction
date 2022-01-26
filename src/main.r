# load required libraries
library(caret)
library(caTools)
library(rpart)
library(rpart.plot)
library(shiny)
library(shinyWidgets)
library(bslib)

# ensure reproducible split in dataset
set.seed(42)

# set working directory
# setwd("C:/Users/User/obesity-prediction/src")

# load data
df <- read.csv("../data/ObesityDataSet_raw_and_data_sinthetic_edited.csv", 
               header = TRUE, sep = ",", fill = TRUE, stringsAsFactors = TRUE)

# rename variables to increase readability
renamed_variables <- c("familyHistoryWithOverweight", "highCalories", "veggies", 
                       "mainMeals", "betweenMeals", "SMOKE", "water", "SCC", 
                       "exercise", "screens", "alcohol", "transport")

for (i in 5:16) {
  names(df)[i] = renamed_variables[i - 4]
}

# convert variables to correct types
variables <- c("veggies", "water", "exercise", "screens")

for (v in variables) {

  # round off synthetic data
  df[, v] <- as.integer(df[, v])

  # convert back to factor
  df[, v] <- as.factor(df[, v])

}

df[, "atRisk"] <- as.factor(df[, "atRisk"])

# show summary of data
summary(df)

# split data into training and test sets
sample_split <- sample.split(Y = df$atRisk, SplitRatio = 0.7)
df.train <- subset(x = df, sample_split == TRUE)
df.test <- subset(x = df, sample_split == FALSE)

# calculate model and prune excess leaves
fit <- rpart(atRisk ~ familyHistoryWithOverweight + highCalories + veggies + 
               water + exercise + screens + alcohol, 
             data = df.train, method = "class")

pruned <- prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# rpart.plot(fit, type = 3, uniform = TRUE, box.palette = "Blues", digits = 1, fallen.leaves = TRUE, varlen = 0, faclen = 0, extra = 0, tweak = 1.15)
# rpart.plot(pruned, type = 3, uniform = TRUE, box.palette = "Blues", digits = 1, fallen.leaves = TRUE, varlen = 0, faclen = 0, extra = 0, tweak = 1.15)

# # calculate accuracy of model
# X <- df.test[,c("familyHistoryWithOverweight", "highCalories","veggies","mainMeals","betweenMeals","water","exercise","screens","alcohol")]
# pred <- predict(fit, X, type = "class")
# c <- confusionMatrix(df.test$atRisk, pred)
#
# # get factor with maximum probability
# colnames(pred)[apply(pred, 1, which.max)][1]
# 
# # get table of matches and accuracy value
# c$table
# c$overall[1]

# run shiny app
runApp("app.r")
