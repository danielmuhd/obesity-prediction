obesity_data <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv", header = TRUE, sep = ",", fill = TRUE, stringsAsFactors = TRUE)

summary(obesity_data)
str(obesity_data)


obesity_data[, "FCVC"] <- as.integer(obesity_data[, "FCVC"])
obesity_data[, "NCP"] <- as.integer(obesity_data[, "NCP"])
obesity_data[, "CH2O"] <- as.integer(obesity_data[, "CH2O"])
obesity_data[, "FAF"] <- as.integer(obesity_data[, "FAF"])
obesity_data[, "TUE"] <- as.integer(obesity_data[, "TUE"])

obesity_data[, "FCVC"] <- as.factor(obesity_data[, "FCVC"])
obesity_data[, "NCP"] <- as.factor(obesity_data[, "NCP"])
obesity_data[, "CH2O"] <- as.factor(obesity_data[, "CH2O"])
obesity_data[, "FAF"] <- as.factor(obesity_data[, "FAF"])
obesity_data[, "TUE"] <- as.factor(obesity_data[, "TUE"])

# model <- glm(NObeyesdad ~ Gender + Age + Height + Weight + family_history_with_overweight + FAVC + FCVC + NCP + CAEC + SMOKE + CH2O + SCC + FAF + TUE + CALC + MTRANS, data = obesity_data, binomial(link = "logit"))
# model <- glm(NObeyesdad ~ Gender + Age + Height + Weight + FAVC + FCVC + NCP + CAEC + CH2O + FAF + TUE + CALC, data = obesity_data, binomial(link = "logit"))
model <- glm(NObeyesdad ~ Gender + Age + Height + Weight, data = obesity_data, binomial(link = "logit"))

model

y <- obesity_data[,"NObeyesdad"]
pred <- model$fitted.values
coefs <- model$coefficients
mad <- mean(abs(y - pred))
mad