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
runRF <- function(dat,testratio= 0.05,checktype,method="none") {
  tgt = which(colnames(dat) == checktype)
  
  #if(method != "none") {dat_bal <- ovun.sample(reformulate(colnames(dat)[-tgt],response = colnames(dat)[tgt]), data=dat, p=0.5, seed=1, method="over")$data}
  
  testid = sample(1:nrow(dat),round(testratio*nrow(dat)),replace = F)
  testc = dat[testid,]
  trainc = dat[-testid,]
  
  formul = reformulate(colnames(dat)[1:(ncol(dat)-3)],response = colnames(dat)[tgt])
  print(formul)
  model <- randomForest(formul, data = dat, ntree = 100, importance = TRUE,do.trace=FALSE)
  
  predicted <- predict(model, testc[,-tgt], type = "response")
  tt = table(pred=predicted, actual=testc[,tgt])
  err = 1 - sum(diag(tt))/sum(tt)
  
  prob1 <- predict(model, testc[,-tgt], type = "prob")
  predd <- prediction(prob1[,2], testc[,tgt])
  #AUC = as.numeric(performance(predd, measure = "auc", x.measure = "cutoff")@y.values)
  
  return(list(model = model, imp = model$importance, tt = tt, err = err, pred_bin = predicted, pred = prob1[,2]))
}

runDT <- function(dat,testratio= 0.05,checktype,method) {
  tgt = which(colnames(dat) == checktype)
  
  #if(method != "none") {dat_bal <- ovun.sample(reformulate(colnames(dat)[-tgt],response = colnames(dat)[tgt]), data=dat, p=0.5, seed=1, method="over")$data}
  
  testid = sample(1:nrow(dat),round(testratio*nrow(dat)),replace = F)
  testc = dat[testid,]
  trainc = dat[-testid,]
  
  formul = reformulate(colnames(dat)[1:(ncol(dat)-3)],response = colnames(dat)[tgt])
  obj2 = tune.rpart(formul, data = trainc, cp = c(0.01,0.1,0.2,0.5,0.8))
  model <- rpart(form2, data = trainc, cp = obj2$best.parameters[1])
  
  predicted <- predict(model, testc[,-tgt], type = "class")
  tt = table(pred=predicted, actual=testc[,tgt])
  err = 1 - sum(diag(tt))/sum(tt)
  
  prob1 <- predict(model, testc[,-tgt], type = "prob")
  predd <- prediction(prob1[,2], testc[,tgt])
  #AUC = as.numeric(performance(predd, measure = "auc", x.measure = "cutoff")@y.values)
  
  return(list(model = model, tt = tt, err = err, pred_bin = predicted, pred = prob1[,2]))
}

runSVM <- function(dat,testratio= 0.05,checktype,method) {
  tgt = which(colnames(dat) == checktype)
  
  testid = sample(1:nrow(dat),round(testratio*nrow(dat)),replace = F)
  testc = dat[testid,]
  trainc = dat[-testid,]
  
  formul = reformulate(colnames(dat)[1:(ncol(dat)-3)],response = colnames(dat)[tgt])
  model <- svm(formul, data = trainc, probability = TRUE, kernel = "radial")
  
  predicted <- predict(model, testc[,-tgt])
  tt = table(pred=predicted, actual=testc[,tgt])
  err = 1 - sum(diag(tt))/sum(tt)
  
  prob1 <- predict(model, testc[,-tgt], probability = TRUE)
  prob_values <- attr(prob1, "probabilities")
  predd <- prediction(prob_values[,2], testc[,tgt])
  #AUC = as.numeric(performance(predd, measure = "auc", x.measure = "cutoff")@y.values)
  
  return(list(model = model, tt = tt, err = err, pred_bin = predicted, pred = prob1[,2]))
  
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
    pred = predict(x$model, testData[, -index], type="prob")
    return (pred[,1]) # !!!
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

computePermutations <- function(rejectProbability, koActivities, Order) {

  tmp = t(apply(rejectProbability, 1, function(x) {
    order(x,decreasing = TRUE)
  }
  ))
  
  tmp2 = apply(tmp,1:2, function(x) {
    x = colnames(rejectProbability)[x]
  }
  )
  
  best=rep(-1,nrow(tmp2))
  for (i in 1:nrow(tmp2)) {
    for (j in 1:nrow(Order)) {
      if (sum(tmp2[i,]==Order[j,])==length(koActivities)) best[i]=j
    }
  }
  return(best)
}

computePermutationsByFormula <- function(rejectProbability, koActivities, Order) {

  results = apply(Order, 1, function(x) {
    index = which(apply(t(Order) == x,2,all))
    total = 0
    for(i in 1:length(x)){
      check = x[i]
      checkIndexRP = which(colnames(rejectProbability)==check)
      current = rejectProbability[,checkIndexRP]
      if (i>1){
        for(j in 1:(i-1)){
          passedCheck = x[j]
          passedCheckIndexRP = which(colnames(rejectProbability)==passedCheck)
          passedRP = (1-rejectProbability[,passedCheckIndexRP])
          current = current * passedRP
        }
        current = current * i # !!!
      }
      total = total+current
    }
    return (total)
  })

  best = apply(results, 1, function(x)
    which.min(x))
  return(best)
}


computeMinCheckNumber <- function(permutations, order, testData, koActivities) {
  name_index = which(colnames(permutations)=="name")
  
  minimum_check_num = apply(permutations, 1, function(x) {
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
  
  return (minimum_check_num)
}


computeCheckNumber <- function(permutations, order, testData, koActivities, checkIndex) {
  name_index = which(colnames(permutations)=="name")
  
  check_num = apply(permutations, 1, function(x) {
  our_order = order[as.numeric(x[checkIndex]),]
  #print(best_order)
  testDataIndex=which(as.numeric(row.names(testData))==as.numeric(x[name_index]))
  counter=0
  for (check in our_order) {
    counter=counter+1
    if(testData[testDataIndex,which(colnames(testData)==check)]==0){
      break;
    }
  }
  return (counter)})

  return (check_num)
}


#compute best permutation
# remember to add cross validation
#input: input csv file path, knockout activities, output csv file path
#output: csv file containing:
# - output1 -> ordered list of testing traces + ordered knock out activities + number of checks to be executed according to the suggestion + number of negative checks (if there is one)
# - output final -> how many ordering 1,2, ... 6 + how many 1 check, 2 checks, 3 checks, overprocessing

computeBestPermutation <-
  function(fileInputPath, fileOutputPath, koActivities, usefulFeatures, numFeatures, disallowed_permutation = c(), n) {
    print("reading in data file")
    inputData = read.csv(fileInputPath,header = TRUE,sep = ",")
    print("data preprocessing")
    if(fileInputPath=="Bondora.csv") {prepreProcessedData=preprocessBondora(inputData, koActivities=koActivities)}
    if(fileInputPath=="Envpermit.csv") {prepreProcessedData=preprocessEnvpermit(inputData, koActivities=koActivities)}
    
    preprocessedData = preprocessData(
      data=prepreProcessedData, koActivities = koActivities, usefulFeatures = usefulFeatures, numFeatures = numFeatures
    )
    
    for (r in c(1:n)) {
      data_gen = generateTrainingAndTesting(dataFiltered = preprocessedData$dataFiltered)
      
      print("computing reject probabilities")
      rejectPb_gen = computeRejectProbability (
        trainingData = data_gen$trainingData, testData = data_gen$testData, koActivities = koActivities
      )
      
      print("computing permutations")
      Order = permutations(length(koActivities), length(koActivities), koActivities)
      rownames(Order)=c(1:nrow(Order))

      if(length(disallowed_permutation) == 0) print("notice: all combinations of knock-out activities are allowed")
      
      if(length(disallowed_permutation) > 0) {
        disallowed_permutation_id = c()
        for (m in 1:nrow(disallowed_permutation)) {
          for (j in 1:nrow(Order)) {
          if (sum(disallowed_permutation[m,]==Order[j,])==length(koActivities)) {
            disallowed_permutation_id=c(disallowed_permutation_id, j)
          }
          }
        }
        print("disallowed permutations: ")
        print(Order[disallowed_permutation_id,])
        Order = Order[-disallowed_permutation_id,]
      }
      
      
      # Our model - order checks in decreasing probability of rejection
      our_best_perm = computePermutations(
        rejectProbability = rejectPb_gen, koActivities = koActivities, Order = Order
      )
      
      # Wil's model - constant probabilities of rejection "learnt" from the training set
      trainingData = data_gen$trainingData
      rejectPb_Wil = rejectPb_gen
      for (i in 1:ncol(rejectPb_Wil)) {
        foo = which(colnames(trainingData) == colnames(rejectPb_Wil)[i])
        rejectPb_Wil[,i] = sum(trainingData[,foo]==0)/nrow(trainingData)
      }
      
      Wil_best_perm = computePermutations(
        rejectProbability = rejectPb_Wil, koActivities = koActivities, Order = Order
      )
      

      # random model
      rand_perm = sample(1:nrow(Order),nrow(data_gen$testData),replace = TRUE)
      
      newPermutations = as.data.frame(cbind (our_best_perm,Wil_best_perm,rand_perm))
      newPermutations$name = row.names(rejectPb_gen)
      
      print("computing number of checks")
      newPermutations$nr_checks_our = computeCheckNumber(
        permutations=newPermutations, order = Order, testData=data_gen$testData, koActivities = koActivities,
        checkIndex = which(colnames(newPermutations)=="our_best_perm"))  
      
      newPermutations$nr_checks_Wil = computeCheckNumber(
        permutations=newPermutations, order = Order, testData=data_gen$testData, koActivities = koActivities,
        checkIndex = which(colnames(newPermutations)=="Wil_best_perm"))  
      
      newPermutations$nr_checks_rand = computeCheckNumber(
        permutations=newPermutations, order = Order, testData=data_gen$testData, koActivities = koActivities,
        checkIndex = which(colnames(newPermutations)=="rand_perm"))  
      
      newPermutations$minimum_check_number = computeMinCheckNumber(
        permutations=newPermutations, order = Order, testData=data_gen$testData, koActivities = koActivities)  
      
      row.names(newPermutations) = newPermutations$name
      newPermutations$name <- NULL
      toPrint = newPermutations[order(as.numeric(rownames(newPermutations))),]
      cleanedFileName = substring(fileOutputPath,1,nchar(fileOutputPath) - 4)
      fileName = paste("output_",cleanedFileName,"_",r, ".csv", sep = "")
      write.table (
        toPrint, file = fileName, append = FALSE, sep = ",", col.names = TRUE, row.names = TRUE,quote = FALSE
      )
      
      fileNameOrder = paste("output_",cleanedFileName,"_permutations", ".csv", sep = "")
      write.table (
        Order, file = fileNameOrder, append = FALSE, sep = ",", col.names = FALSE, row.names = FALSE,quote = FALSE
      )
      
    }

  }

