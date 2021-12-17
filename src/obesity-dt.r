obesity_data <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv", header = TRUE, sep = ",", fill = TRUE, stringsAsFactors = TRUE)

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

summary(obesity_data)
str(obesity_data)

obesity_data.train <- obesity_data[1:1477,]
obesity_data.test <- obesity_data[1478:2111,]


library(tree)


Baum <- tree(NObeyesdad ~ Gender + Age + Height + Weight + family_history_with_overweight + FAVC + FCVC + NCP + CAEC + SMOKE + CH2O + SCC + FAF + TUE + CALC + MTRANS, data = obesity_data.train)
tuning <- cv.tree(Baum, K=10)
# plot(tuning)

# Der Baum mit der optimalen Anzahl an Endknoten:

t <- which.min(tuning$dev)
Anzahl.Endknoten <- tuning$size[t]
Anzahl.Endknoten

model <- prune.tree(Baum,best=Anzahl.Endknoten)
# plot(model)
# text(model)

# Berechnen von Prognosen

# Berechnung der Prognosen f?r die Personen aus dem Datensatz:

# Auswahl aller Input-Variablen und Speicherung unter 'X'
X <- obesity_data[,c("Gender", "Age","Height","Weight","family_history_with_overweight","FAVC","FCVC","NCP","CAEC","SMOKE","CH2O","SCC","FAF","TUE","CALC","MTRANS")]

# Berechnung der Prognoseergebnisse:
predict(model,X)


# Berechnung der Prognoseg?te:

prognosen <- predict(model,X)
prognosen

# Umrechnen der Wahrscheinlichkeiten in 0/1-Prognosen
prognosen <- round(prognosen[])
prognosen

w <- which(prognosen=="1",arr.ind=TRUE)
prognosen[w] <- names(prognosen)[w[,"col"]]

y <- obesity_data.test[,"NObeyesdad"]
y

# Aufstellen der Matrix
A <- matrix(0,ncol=2,nrow=2)
colnames(A) <- c("Real: positiv", "Real: negativ") 
rownames(A) <- c("Prognose: positiv", "Prognose: negativ") 
A[1,1] <- sum(ifelse(y == 0 & prognosen == 0, 1,0))
A[1,2] <- sum(ifelse(y == 1 & prognosen == 0, 1,0))
A[2,1] <- sum(ifelse(y == 0 & prognosen == 1, 1,0))
A[2,2] <- sum(ifelse(y == 1 & prognosen == 1, 1,0))

A