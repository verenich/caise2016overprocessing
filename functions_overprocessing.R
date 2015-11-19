#CHECK WHETHER ALL PACKAGES ARE INSTALLED. IF NOT, INSTALL MISSING
required_packages <- c('randomForest','rpart','ggplot2','reshape','MASS','ROCR',
                       'rpart.plot','gtools')
for(pkg in required_packages){
  if(pkg %in% rownames(installed.packages()) == FALSE) {
    install.packages(pkg)
  }
}

library(randomForest)
library(rpart)
library(ggplot2)
library(reshape)
library(MASS)
library(ROCR)
library(rpart.plot)
library(e1071)
library(gtools)

####DATA PREPROCESSING functions####

#dataset-specific preprocessing
preprocessBondora<-function(dat=Bon, koActivities){
  #factorize targets, reverse the labels for some checks - for semantics (0 - check not passed, 1 - passed)
  dat$PostFundingCancellation = factor(dat$PostFundingCancellation,labels = c(1,0))
  dat$IdCancellation = factor(dat$IdCancellation, labels=c(1,0))
  dat$CreditDecision = factor(dat$CreditDecision, labels=c(0,1))
  
  #tweak predictors
  Breaks = c(0, 20, 30, 40, 50, 60, 70, 80) #age groups
  dat$Age = cut(dat$Age, breaks = Breaks)
  levels(dat$Age) = seq(1:length(Breaks))
  
  dat$Gender = factor(dat$Gender)
  dat$NewCreditCustomer = factor(dat$NewCreditCustomer)
  dat$language_code = factor(dat$language_code)
  dat$UseOfLoan = factor(dat$UseOfLoan)
  dat$ApplicationType = factor(dat$ApplicationType)
  dat$education_id = factor(dat$education_id)
  dat$marital_status_id = factor(dat$marital_status_id)
  dat$employment_status_id = factor(dat$employment_status_id)
  dat$occupation_area = factor(dat$occupation_area)
  dat$home_ownership_type_id = factor(dat$home_ownership_type_id)
  
  M = which(dat$nr_of_dependants == "")
  dat = dat[-M,]
  M = which(levels(dat$nr_of_dependants)=="10Plus")
  levels(dat$nr_of_dependants)[M] = "11"
  dat$nr_of_dependants = as.numeric(as.character(dat$nr_of_dependants))
  
  dat = generateConstTime(dat, koActivities)
  
  return (dat)
}


preprocessEnvpermit <- function(dat, koActivities) {
  dat$lifecycle.transition = NULL
  dat$Variant = NULL
  dat$concept.instance = NULL
  dat$Activity = NULL
  
  dat$concept.name = as.character(dat$concept.name)
  
  dat$whichfailed = "unknown"
  
  caselen = table(dat$Case.ID)
  j=1
  for (i in 1:nrow(dat)) {
    if (dat$Resource[i] == "End") {
      pos = caselen[j]-1
      dat$whichfailed[i-pos] = dat$concept.name[i-1]
      j=j+1
    }
  }
  
  foo = which(dat$whichfailed == "unknown" | dat$whichfailed == "Confirmation of receipt")
  dat = dat[-foo,]
  
  dat$T02 = 1
  dat$T06 = 1
  dat$T10 = 1
  
  for (i in 1:ncol(dat)) {
    if(is.factor(dat[,i]) == TRUE)
      dat[,i]=droplevels(dat[,i])
  }
  
  for (k in 1:nrow(dat)) {
    if (dat$whichfailed[k] == "T10 Determine necessity to stop indication") dat$T10[k] = 0
    if (dat$whichfailed[k] == "T02 Check confirmation of receipt") dat$T02[k] = 0
    if (dat$whichfailed[k] == "T06 Determine necessity of stop advice") dat$T06[k] = 0
  }
  
  dat$whichfailed = NULL
  dat$T02 = as.factor(dat$T02)
  dat$T06 = as.factor(dat$T06)
  dat$T10 = as.factor(dat$T10)
  
  dat = generateConstTime(dat, koActivities)
  return(dat)
}


#generic preprocessing
# clean the NA data, filter the usefulFeatures under a threshold and returns the cleaned numeric features
preprocessData<-function(data, koActivities, usefulFeatures, numFeatures){
  koIndexes = match(koActivities, colnames(data))
  invalid = which(is.na(data[,koIndexes]), arr.ind=TRUE)[,1]
  if(length(invalid) > 0) data=data[-invalid,]
  
  usefulFeaturesIndexes = c()
  for (i in 1:length(usefulFeatures)) {
    usefulFeatureIndex = which(names(data)==usefulFeatures[i])
    if((sum(is.na(data[,usefulFeatureIndex])))/nrow(data) < 0.1) #exclude features where > 10% of values are NA's
    {usefulFeaturesIndexes = c(usefulFeaturesIndexes,usefulFeatureIndex)}
  }
  dataFiltered = data[,usefulFeaturesIndexes]
  
  numFeaturesIndexes = c()
  for (i in 1:length(numFeatures)) {
    numFeatureIndex = which(names(dataFiltered)==numFeatures[i])
    if((sum(is.na(dataFiltered[,numFeatureIndex])))/nrow(dataFiltered) < 0.1) #exclude features where > 10% of values are NA's
    {numFeaturesIndexes = c(numFeaturesIndexes,numFeatureIndex)}
  }
  
  dataFiltered = na.omit(dataFiltered)
  
  for (i in 1:ncol(dataFiltered)) {
    if(is.factor(dataFiltered[,i]) == TRUE)
      dataFiltered[,i]=droplevels(dataFiltered[,i])
  }
  
  return (list(dataFiltered=dataFiltered, numFeaturesIndexes=numFeaturesIndexes))
  
}

####AUXILIARY functions####
#generate costant processing times
generateConstTime <- function(data, koActivities){
  tempData = mapply(function(x) {
    curr_time = t(apply(data, 1, function(y)return (1)))
    #print(nrow(curr_time))
    return (curr_time)
  }, paste(koActivities, "_time", sep=""))
  newData = cbind(data, tempData) 
  return (newData)
}

#generate exponential processing times
generateExpTime <- function(data, koActivitiesDistribution, numFeaturesIndexes){
  numericData = data[,numFeaturesIndexes]
  N= nrow(numericData)
  
  numericData$time = apply(koActivities, function(x) {
    koDistribution = koActivitiesDistribution[x]
    return (koDistribution[1]*exp(data[,2]*koDistribution[2]*runif(N,koDistribution[3],koDistribution[4])) + koDistribution[5]*exp(data[,3]*koDistribution[6]*runif(N,koDistribution[7],koDistribution[8])))
  })
  
  return (numericData)
}

generateTrainingAndTesting <- function(dataFiltered){
  testingId = sample(1:nrow(dataFiltered),round(0.2*nrow(dataFiltered)),replace=F)
  testData = dataFiltered[testingId,] # testing 
  trainingData = dataFiltered[-testingId,] # training
  
  return (list(trainingData=trainingData, testData=testData))
}

removeTimeColumns<-function(data){
  timeColumnIndexes = c()
  for (i in 1:length(colnames(data))) {
    colname = colnames(data)[i]
    if (substring(colname, (nchar(colname)-4), nchar(colname))=="_time")
      timeColumnIndexes=c(timeColumnIndexes, i) 
  }
  data = data[,-timeColumnIndexes]
  return (data)
}


####functions for MACHINE LEARNING model training####

runRF <- function(dat,testratio= 0.2,checktype,method="none") {
  tgt = which(colnames(dat) == checktype)
  
   
  dat_bal <- dat
  #if(method != "none") {dat_bal <- ovun.sample(reformulate(colnames(dat)[-tgt],response = colnames(dat)[tgt]), data=dat, p=0.5, seed=1, method="over")$data}
   
  testid = sample(1:nrow(dat_bal),round(testratio*nrow(dat_bal)),replace = F)
  testc = dat_bal[testid,]
  trainc = dat_bal[-testid,]
  
  rf <- randomForest(x=trainc[,1:(ncol(dat_bal)-3)], y = trainc[,tgt], ntree = 100, importance = TRUE,do.trace=FALSE)
  
  predicted <- predict(rf, testc[,-tgt], type = "response")
  tt = table(pred=predicted, actual=testc[,tgt])
  err = 1 - sum(diag(tt))/sum(tt)
  
  prob1 <- predict(rf, testc[,-tgt], type = "prob")
  predd <- prediction(prob1[,2], testc[,tgt])
  #AUC = as.numeric(performance(predd, measure = "auc", x.measure = "cutoff")@y.values)
  
  AUC = -1
  result = tryCatch({
    AUC = as.numeric(performance(predd, measure = "auc", x.measure = "cutoff")@y.values)
  }, warning = function(w) {
    # log the warning or take other action here
  }, error = function(e) {
    # log the error or take other action here
  }, finally = {
    # this will execute no matter what else happened
  })
  
  return(list(rf = rf, AUC = AUC, imp = rf$importance, tt = tt, err = err, pred_bin = predicted, pred = prob1[,2]))
}

# 
# runRF <- function(dat,testratio= 0.2,checktype,method="none") {
#   tgt = which(colnames(dat) == checktype)
#   
#   #if(method != "none") {dat_bal <- ovun.sample(reformulate(colnames(dat)[-tgt],response = colnames(dat)[tgt]), data=dat, p=0.5, seed=1, method="over")$data}
#   
#   testid = sample(1:nrow(dat),round(testratio*nrow(dat)),replace = F)
#   testc = dat[testid,]
#   trainc = dat[-testid,]
#   
#   formul = reformulate(colnames(dat)[1:(ncol(dat)-3)],response = colnames(dat)[tgt])
#   rf <- randomForest(formul, ntree = 100, importance = TRUE,do.trace=FALSE)
#   
#   predicted <- predict(rf, testc[,-tgt], type = "response")
#   tt = table(pred=predicted, actual=testc[,tgt])
#   err = 1 - sum(diag(tt))/sum(tt)
#   
#   prob1 <- predict(rf, testc[,-tgt], type = "prob")
#   predd <- prediction(prob1[,2], testc[,tgt])
#   AUC = as.numeric(performance(predd, measure = "auc", x.measure = "cutoff")@y.values)
#   
#   return(list(rf = rf, AUC = AUC, imp = rf$importance, tt = tt, err = err, pred_bin = predicted, pred = prob1[,2]))
# }
# 
runSVM <- function() {
  warning("to be implemented")
}

#predict processing times
runRFtime <- function(testratio=0.2,data,numFeaturesIndexes) {
  indexes=c(numFeaturesIndexes,which (colnames(data)=="time"))
  data = data[,indexes]
  
  testid = sample(1:nrow(data),round(testratio*nrow(data)),replace = F)
  testc = data[testid,]
  trainc = data[-testid,]
  
  #mod = glm(time ~ ., data = trainc, family = Gamma(link='log'))
  mod = glm(time ~ ., data = trainc)
  predicted <- predict(mod, testc[,-ncol(testc)], type = "response")
  RMSE = sqrt(mean((testc[,ncol(testc)]-predicted)^2))
  
  return(list(mod = mod, pred = predicted, RMSE = RMSE))
}


####functions for COMPUTING PROCESSING AND OVERPROCESSING####
computeRejectProbability <- function(trainingData, testData, koActivities){
  noTimeTrainingData = removeTimeColumns(data = trainingData)
  RFResults = mapply(function(x) runRF(dat=noTimeTrainingData, checktype = x), koActivities)
  
  RFResults = rbind(RFResults, koActivities)
  
  # predict reject probability for each task
  predRP = apply(RFResults, 2, function(x) { 
    index = which(colnames(testData)==x$koActivities)
    pred = predict(x$rf, testData[, -index], type="prob")
    return (pred[,2])
  })
  
  return (predRP)
}

computePredictedTime <- function(trainingData, testData, numFeaturesIndexes, koActivities) {
  noTimeTrainingData = removeTimeColumns(data = trainingData)
  RFTResults = mapply(
    function(x){
      index = which(colnames(trainingData)==x)
      specialTrainingData = cbind(noTimeTrainingData,trainingData[,index])
      colnames(specialTrainingData)=c(colnames(noTimeTrainingData),"time")
      return (runRFtime(data = specialTrainingData, numFeaturesIndexes = numFeaturesIndexes))
    }, paste(koActivities,"_time", sep="")
  )
  
  
  # predict time for each task
  predT = apply(RFTResults, 2, function(x) {
    index = which(colnames(testData) == colnames(x))
    pred = predict(x$mod, testData[,numFeaturesIndexes], type = "response")
    return (pred)
  })
  
  return (predT)
}

computePermutations <- function(rejectProbability, predictedTimes, testData, koActivities) {
  permutations = permutations(length(koActivities), length(koActivities), koActivities)
  rownames(permutations)=c(1:nrow(permutations))
  
  results = apply(permutations, 1, function(x) {
    index = which(apply(t(permutations) == x,2,all))
    total = 0
    for(i in 1:length(x)){
      check = x[i]
      checkTime = paste(check,"_time", sep="")
      checkIndexRP = which(colnames(rejectProbability)==check)
      checkIndexPT = which(colnames(predictedTimes)==checkTime)
      current = predictedTimes[,checkIndexPT]*rejectProbability[,checkIndexRP]
      if (i>1){
        for(j in 2:i-1){
          passedCheck = x[j]
          passedCheckIndexRP = which(colnames(rejectProbability)==passedCheck)
          passedRP = (1-rejectProbability[,passedCheckIndexRP])
          current = current * passedRP
        }
      }
      total = total+current
    }
    return (total)
  })
  
  
  return (list(perm=results, order=permutations))
}

computeCheckNumber <- function(permutations, order, testData, koActivities) {
  best_index = which(colnames(permutations)=="best")
  name_index = which(colnames(permutations)=="name")
  permutations$checks = apply(permutations, 1, function(x) {
    best_order = order[as.numeric(x[best_index]),]
    #print(best_order)
    testDataIndex=which(as.numeric(row.names(testData))==as.numeric(x[name_index]))
    counter=0
    for (check in best_order) {
      counter=counter+1
      if(testData[testDataIndex,which(colnames(testData)==check)]==0){
        break;
      }
    }
    return (counter)})
  permutations$minimum_check_num = apply(permutations, 1, function(x) {
    testDataIndex=which(as.numeric(row.names(testData))==as.numeric(x[name_index]))
    PCCounterArray = mapply(function(y){
      if(testData[testDataIndex,which(colnames(testData)==y)]==1){
        CurrPCCounter = 1;
      } else CurrPCCounter =0;
      return (CurrPCCounter)
    }
    ,koActivities) 
    #print(PCCounterArray)
    PCCounter = ifelse (all(PCCounterArray==1), sum(PCCounterArray), 1)
    return (PCCounter)})  
  
  return (permutations)
}

printOutput<-function(i, permutations, order, fileOutputPath){
  
  globalTable = cbind(
    permutations$best, order[permutations$best,],permutations$checks, permutations$minimum_check_num, (
      permutations$checks - permutations$minimum_check_num
    )
  )
  row.names(globalTable) = permutations$name
  columns = c(
    "Permutation Number", mapply(function(x)
      sprintf("KOActivity_%i",x), seq(1, length(koActivities))), "Number of checks to be executed according to the suggestion", "Minimum Check Number (Ideal Processing)", "Check Overprocessing"
  )
  colnames(globalTable) = columns
  intermediateTable = globalTable[,-1]
  
  toPrint = intermediateTable[order(as.numeric(rownames(intermediateTable))),]
  print(table(permutations$checks))
  print(table(permutations$minimum_check_num))
  print(table(
    permutations$checks - permutations$minimum_check_num
  ))
  cleanedFileName = substring(fileOutputPath,1,nchar(fileOutputPath) - 4)
  fileName = paste("output_",cleanedFileName,"_",i, ".csv", sep = "")
  write.table (
    toPrint, file = fileName, append = FALSE, sep = ",", col.names = TRUE, row.names = TRUE,quote = FALSE
  )
}

#compute best permutation
# remember to add cross validation
#input: input csv file path, knockout activities, output csv file path
#output: csv file containing:
# - output1 -> ordered list of testing traces + ordered knock out activities + number of checks to be executed according to the suggestion + number of negative checks (if there is one)
# - output final -> how many ordering 1,2, ... 6 + how many 1 check, 2 checks, 3 checks, overprocessing

computeBestPermutation <-
  function(fileInputPath, fileOutputPath, koActivities, usefulFeatures, numFeatures, n) {
    print("reading in data file")
    inputData = read.csv(fileInputPath,header = TRUE,sep = ",")
    print("data preprocessing")
    if(fileInputPath=="Bondora.csv") {prepreProcessedData=preprocessBondora(inputData, koActivities=koActivities)}
    if(fileInputPath=="Envpermit.csv") {prepreProcessedData=preprocessEnvpermit(inputData, koActivities=koActivities)}
    
    preprocessedData = preprocessData(
      data=prepreProcessedData, koActivities = koActivities, usefulFeatures = usefulFeatures, numFeatures = numFeatures
    )
    
    tableChecks=vector(mode="numeric", length=length(koActivities))
    tableMinimumChecks=vector(mode="numeric", length = 2)
    tableOverprocessing=vector(mode="numeric", length = length(koActivities))
    
    for (i in c(1:n)) {
      data_gen = generateTrainingAndTesting(dataFiltered = preprocessedData$dataFiltered)
      
      print("computing reject probabilities")
      rejectPb_gen = computeRejectProbability (
        trainingData = data_gen$trainingData, testData = data_gen$testData, koActivities = koActivities
      )
      
      print("computing processing time")
      if(fileInputPath=="Bondora.csv") {
        predictedTime_gen = computePredictedTime (
        trainingData = data_gen$trainingData, testData = data_gen$testData, numFeaturesIndexes =
          preprocessedData$numFeaturesIndexes, koActivities = koActivities)
      }
      
      if(fileInputPath=="Envpermit.csv") { # do not predict, just copy all ones as PTs
        predictedTime_gen = data_gen$testData
        predictedTime_gen = predictedTime_gen[(ncol(predictedTime_gen)-length(koActivities)+1):ncol(predictedTime_gen)]
      }
      
      print("computing permutations")
      permutationsData_gen = computePermutations(
        rejectProbability = rejectPb_gen, predictedTimes = predictedTime_gen, testData = data_gen$testData, koActivities = koActivities
      )
      
      permutations = permutationsData_gen$perm
      order = permutationsData_gen$order
      
      best = apply(permutations, 1, function(x)
        which.min(x))
      permutations = as.data.frame(cbind (permutations,best))
      
      table(permutations$best)
      permutations$name = row.names(permutations)
      newPermutations = computeCheckNumber(
        permutations=permutations, order = order, testData=data_gen$testData, koActivities = koActivities
      )  
      
      printOutput(i=i, permutations=newPermutations, order=order, fileOutputPath = fileOutputPath)
      tableChecks=tableChecks+table(newPermutations$checks)
      tableMinimumChecks=tableMinimumChecks+table(newPermutations$minimum_check_num) 
      tableOverprocessing=tableOverprocessing+table(
        newPermutations$checks - newPermutations$minimum_check_num
      )
      
    }
    cleanedFileName = substring(fileOutputPath,1,nchar(fileOutputPath) - 4)
    fileNameGeneral = paste("output_",cleanedFileName,"_general", ".txt", sep = "")
    tableChecks=tableChecks/n
    tableMinimumChecks= tableMinimumChecks/n
    tableOverprocessing=tableOverprocessing/n
    
    write (
      "***** STATISTICS ******* ", file = fileNameGeneral, append = FALSE, sep = ""
    )
    cat(
      c("1 check", "2 checks", "3 checks", "\n"), file = fileNameGeneral, append = TRUE, sep = "\t"
    )     
    cat(
      tableChecks, file = fileNameGeneral, append = TRUE, sep = "\t\t"
    ) 
    write(
      c("\n","PROCESSING"), file = fileNameGeneral, append = TRUE, sep = "\t"
    )       
    cat(
      c("1 check", "3 checks", "\n"), file = fileNameGeneral, append = TRUE, sep = "\t"
    )       
    cat(
      tableMinimumChecks, file = fileNameGeneral, append = TRUE, sep = "\t\t"
    )
    write(
      c("\n","OVERPROCESSING"), file = fileNameGeneral, append = TRUE, sep = "\t"
    )       
    cat(
      c("1 check", "2 checks", "3 checks", "\n"), file = fileNameGeneral, append = TRUE, sep = "\t"
    )     
    write(
      tableOverprocessing, file = fileNameGeneral, append = TRUE, sep = "\t"
    )
    
  }

