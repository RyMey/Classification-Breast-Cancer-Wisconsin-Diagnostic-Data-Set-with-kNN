# G64164008
# Rya Meyvriska
# Praktikum 2

# 1. Mempersiapkan data
dt <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", sep = ',')
dt <- dt[,-1] #menghilangkan fitur v1, karena isinya hanya ID

# 2. Split data
dtClassB <- dt[which(dt$V2=='B'),]
dtClassM <- dt[which(dt$V2=='M'),]

# 3. Cek Range
summary(dt)

# 4. Normalisasi data
normalize <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}
normalizedDtB <- as.data.frame(lapply(dtClassB[2:31], normalize))
normalizedDtM <- as.data.frame(lapply(dtClassM[2:31], normalize))

# 5. Ambil training data (70%) dan testing data (30%)
# Ambil 70% data dari kelas B
nClassB <- nrow(normalizedDtB)
boundClassB <- ceiling(nClassB * 0.7)
trainingDtClassB <- normalizedDtB[1:boundClassB, ]
testingDtClassB <- normalizedDtB[(boundClassB + 1):nClassB, ]
trainingLabelsClassB <- dtClassB[1:boundClassB, 1]
testingLabelsClassB <- dtClassB[(boundClassB + 1):nClassB, 1]

# Ambil 70% data dari kelas M
nClassM <- nrow(normalizedDtM)
boundClassM <- ceiling(nClassM * 0.7)
trainingDtClassM <- normalizedDtM[1:boundClassM, ]
testingDtClassM <- normalizedDtM[(boundClassM + 1):nClassM, ]
trainingLabelsClassM <- dtClassM[1:boundClassM, 1]
testingLabelsClassM <- dtClassM[(boundClassM + 1):nClassM, 1]

# Satukan dalam satu data
trainingDt <- rbind(trainingDtClassB[,], trainingDtClassM[,])
testingDt <- rbind(testingDtClassB[,], testingDtClassM[,])
trainingLabels <- c(as.character(trainingLabelsClassB[]), as.character(trainingLabelsClassM[]))
trainingLabels <- as.factor(trainingLabels)
testingLabels <- c(as.character(testingLabelsClassB[]), as.character(testingLabelsClassM[]))
testingLabels <- as.factor(testingLabels)

# Undersampling pada trainingDt
library(ROSE) #Random OverUnder Sampling Example
dtUnder <- cbind(trainingLabels,trainingDt)
trainingDtOver <- ovun.sample(trainingLabels~., data = dtUnder, method = "under", N = 298)$data
trainingDtOverLabel <- trainingDtOver$trainingLabels
trainingDtOver <- trainingDtOver[,-1]

# 6.Membuat prediksi dengan kNN
library(class)
startK <- ceiling(sqrt(nrow(trainingDt)))-3
predictionLabels <- knn(train = trainingDtOver, test = testingDt, cl = trainingDtOverLabel, k = startK)

# 7. Evaluasi
# Tabel Hasil
confusionMatrix <- table(predictionLabels,testingLabels)
accuracy <- (confusionMatrix[1,1] + confusionMatrix [2,2]) / (confusionMatrix[1,1] + confusionMatrix[1,2] + confusionMatrix[2,1] +confusionMatrix[2,2])
recall <- (confusionMatrix[1,1]) / (confusionMatrix[1,1] + confusionMatrix[1,2])
specificity <- (confusionMatrix [2,2]) / (confusionMatrix[2,1] +confusionMatrix[2,2])
precision <- (confusionMatrix[1,1]) / (confusionMatrix[1,1] + confusionMatrix[2,1])
# List Data Evaluasi
evaluasiList <- list(confusionMatrix = confusionMatrix, accuracy = accuracy , recall = recall, specificity = specificity, precision = precision)
evaluasiList

# 8.Beberapa Plot
library(lattice)
xyplot(V10 ~ V28, dt, groups = dt$V2, pch= 20)
xyplot(V7 ~ V26, dt, groups = dt$V2, pch= 20)
xyplot(V4 ~ V23, dt, groups = dt$V2, pch= 20)
xyplot(V10 ~ V29, dt, groups = dt$V2, pch= 20)
