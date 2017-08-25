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

# 6.Membuat prediksi dengan kNN
library(class)
startK <- ceiling(sqrt(nrow(trainingDt))) - 1
predictionLabels <- knn(train = trainingDt, test = testingDt, cl = trainingLabels, k = startK)

# 7. Evaluasi
# Tabel Hasil
result <- table(predictionLabels,testingLabels)
# Tabel Kebenaran
truthMatrix <- matrix(c(round(105/111, digit = 2),round(6/111, digit = 2),
                    round(2/59, digit = 2), round(57/59, digit = 2)), 
                    ncol = 2, nrow = 2, byrow = TRUE)
colnames(truthMatrix) <- c("Actually B","Actually M")
rownames(truthMatrix) <- c("Predict B","Predict M")
truthMatrix <- as.table(truthMatrix)
# Persentase Kebenaran dan Persentase Error
percentageOfTruth <- (truthMatrix[1,1] + truthMatrix [2,2]) / (truthMatrix[1,1] + truthMatrix[1,2] + truthMatrix[2,1] +truthMatrix[2,2])
percentageOfError <- (truthMatrix[1,2] + truthMatrix [2,1]) / (truthMatrix[1,1] + truthMatrix[1,2] + truthMatrix[2,1] +truthMatrix[2,2])
# List Data Evaluasi
percentageList <- list(result = result, truthMatrixInPercent = truthMatrix , percentageOfTruth = percentageOfTruth, percentageOfError = percentageOfError)
percentageList

# 7.Beberapa Plot
library(lattice)
xyplot(V10 ~ V28, dt, groups = dt$V2, pch= 20)
xyplot(V7 ~ V26, dt, groups = dt$V2, pch= 20)
xyplot(V4 ~ V23, dt, groups = dt$V2, pch= 20)
xyplot(V10 ~ V29, dt, groups = dt$V2, pch= 20)