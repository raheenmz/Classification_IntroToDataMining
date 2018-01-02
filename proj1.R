classify <- function() {
  
  # Loading packages
  init() 
  
  # Reads data from excel
  dataSet <- readData("./data.xlsx")
  
  # set seed value for randomzation
  seedValue <- getSeedValue()

  # splits data into training and testing set  
  c(trainSet, testSet)%<-%splitDataset(dataSet,seedValue)
  
  ### KNN
  fitKNN <- myKNN(trainSet) 
  confMAtrixKNN <- myKNNPredict(fitKNN, testSet) 
  c(accuracyKNN,precisionKNN,recallKNN,fMeasureKNN)%<-%calculateStats(confMAtrixKNN)
  printResults("KNN", confMAtrixKNN, accuracyKNN, precisionKNN, recallKNN, fMeasureKNN)
  
    
  ### Ripper
  fitRIPPER <- myRIPPER(trainSet)
  confMAtrixRIPPER <- myRIPPERPredict(fitRIPPER, testSet)
  c(accuracyRIPPER,precisionRIPPER,recallRIPPER,fMeasureRIPPER)%<-%calculateStats(confMAtrixRIPPER)
  printResults("RIPPER", confMAtrixRIPPER, accuracyRIPPER,precisionRIPPER,recallRIPPER,fMeasureRIPPER)  
  
  ### C4.5
  fitC45 <- myC45(trainSet)
  confMAtrixC45 <- myC45Predict(fitC45, testSet)
  c(accuracyC45,precisionC45,recallC45,fMeasureC45)%<-%calculateStats(confMAtrixC45)    
  printResults("C4.5", confMAtrixC45, accuracyC45,precisionC45,recallC45,fMeasureC45)  
  
    
  ## svm
  fitSVM <- mySVM(trainSet)
  confMAtrixSVM <- mySVMPredict(fitSVM, testSet)
  c(accuracySVM,precisionSVM,recallSVM,fMeasureSVM)%<-%calculateStats(confMAtrixSVM)    
  printResults("SVM",confMAtrixSVM, accuracySVM,precisionSVM,recallSVM,fMeasureSVM)  
  
}


# genric function for printing results 
printResults <- function (model, confMAtrix, accuracy,precision,recall,fMeasure) {
  cat("\n\n***************************")
  cat("\nRESULTS FOR ", model) 
  cat("\n***************************")
  cat("\nConfuson Matrx:")
  print(as.table(confMAtrix))
  cat("\nAccuracy: ", accuracy)
  cat("\nPrecision(by class in order {Africa,Asia,Europe,North America,Oceania,South America}) : ",precision)
  cat("\nRecall(by class in order {Africa,Asia,Europe,North America,Oceania,South America}): ",recall)
  cat("\nF- Measure(by class in order {Africa,Asia,Europe,North America,Oceania,South America}): ",fMeasure)
}

# gets seed value from user

getSeedValue <- function() {
  seedValue <- readline(prompt = "Enter seed value: ")
  return (as.numeric(seedValue))
}


init <- function() {
  #install.packages("rJava")
  #install.packages("RWeka")
  #install.packages("readxl")
  #install.packages("caret")
  #install.packages("e1071")
  #install.packages("zeallot")
  
    
  if (!(require("RWeka")))
    print("Error in loading RWeka package!!! Please check if installed correctly...", quote=FALSE)
  else
    print("Loaded RWeka package", quote=FALSE)
  
  if (!(require("readxl")))
    print("Error in loading readxl package!!! Please check if installed correctly...", quote=FALSE)
  else
    print("Loaded readxl package", quote=FALSE)
  
  if (!(require("caret")))
    print("Error in loading caret package!!! Please check if installed correctly...", quote=FALSE)
  else
    print("Loaded caret package", quote=FALSE)
  
  if (!(require("e1071")))
    print("Error in loading e1071 package!!! Please check if installed correctly...", quote=FALSE)
  else
    print("Loaded e1071 package", quote=FALSE)
  
  if (!(require("zeallot")))
    print("Error in loading zeallot package!!! Please check if installed correctly...", quote=FALSE)
  else
    print("Loaded zeallot package", quote=FALSE)
}

## reads data from excel
readData <- function(pathLink) {
  dataSet <- read_excel(pathLink)
  print("Uploaded Data Successfully", quote=FALSE)
  return(dataSet)
}


## splits data into tranng and test set
splitDataset <- function(dataSet, seedValue) {
  #Setting seed
  set.seed(seedValue)
  
  # Factoring Continent Column
  dataSet$Continent <- factor(dataSet$Continent)
  
  # Removing Entity Column
  entityColName <- grep("Entity",names(dataSet))
  dataSet <- dataSet[,-entityColName]
  
  # Dividing into train and test set 
  dataSet[,"index"] <- ifelse(runif(nrow(dataSet))<0.8,1,0)
  trainSet <- dataSet[dataSet$index==1,]
  testSet <- dataSet[dataSet$index==0,]
  indexColName <- grep("index",names(trainSet))
  trainSet <- trainSet[,-indexColName]
  testSet <- testSet[,-indexColName]
  cat("Data split into train and test set")
  cat("\nSize of training set: " , nrow(trainSet))
  cat("\nSize of testing set: " , nrow(testSet))
  return(list(trainSet, testSet))
} 

## Evaluating Results

calculateStats <- function(confMAtrix) {
  accuracy <- confMAtrix$overall['Accuracy']
  compTable <- as.table(confMAtrix)
  
  precision = numeric() 
  for (i in 1:ncol(compTable)) {
    precision <- c(precision,compTable[i,i]/sum(compTable[i,]))
  }
  
  recall = numeric() 
  for (i in 1:ncol(compTable)) {
    recall <- c(recall,compTable[i,i]/sum(compTable[,i]))
  }
  
  fMeasure = (2 * recall * precision)/(recall+precision)
  
  return(list(accuracy,precision,recall,fMeasure))
} 

##### knn Classification Methods ######

myKNN <- function(trainSet) {
  tuning <- trainControl(method = "repeatedcv",number = 10,repeats = 5)
  fit <- train(trainSet[,1:4], trainSet$Continent, method='knn', trControl = tuning)
  return (fit)
}

myKNNPredict <- function(fit, testSet) {
  predictions<-predict(fit, testSet[,1:4])
  confMAtrix <- confusionMatrix(predictions, testSet$Continent)
  return (confMAtrix)
}

##### RiPPER Classification Methods ######

myRIPPER <- function(trainSet) {
  tuning <- trainControl(method = "repeatedcv",number = 10,repeats = 5)
  fit <- train(trainSet[,1:4], trainSet$Continent,method = "JRip", trControl = tuning, preProcess = c("center","scale"))
  return (fit)
}

myRIPPERPredict <- function(fit, testSet) {
  predictions <- predict(fit, testSet[,1:4])
  confMAtrix <- confusionMatrix(predictions, testSet$Continent)
  return (confMAtrix)
}


##### C4.5 Classification Methods ######

myC45 <- function(trainSet) {
  fit <- J48(Continent ~., data = trainSet, control = Weka_control(R = TRUE, N= 10))
  return (fit)
}

myC45Predict <- function(fit, testSet) {
  predictions <- predict(fit, testSet)
  compTable <- table(predictions, testSet$Continent)
  confMAtrix <- confusionMatrix(compTable)
  return (confMAtrix)
}

##### SVM Classification Methods ######

mySVM <- function(trainSet) {
  fit <- svm(Continent~ ., data=trainSet, method="C-classification", kernel="radial", type = "C")
  optCost <- tune.svm(Continent~., data = trainSet, gamma = 2^(-1:1), cost = 2^(2:4))
  tunedFit <- svm(Continent~ ., data=trainSet, method="C-classification", kernel="radial", type = "C", cost=optCost$best.parameters$cost, optCost$best.parameters$gamma)
  return (tunedFit)
}

mySVMPredict <- function(fit, testSet) {
  predictions <- predict(fit, testSet)
  confMAtrix <- confusionMatrix(predictions, testSet$Continent)
  return (confMAtrix);
}

### Calling classification

classify()
