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

generateTrainingAndTesting <- function(dataFiltered, testratio = 0.2){
  testingId = sample(1:nrow(dataFiltered),round(testratio*nrow(dataFiltered)),replace=F)
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


####functions for TRAINING CLASSIFIERS to predict reject probabilities####
trainClassifiers <- function(data, checktype, learner, sampling_method) {
  dat = removeTimeColumns(data)
  tgt = which(colnames(dat) == checktype)  
  
  if(sampling_method == "over") {
    class_balance = table(dat[,tgt]) 
    MM = which.min(class_balance)
    diff = floor(class_balance[-MM]/class_balance[MM])
    smallid = which(dat[,tgt] == names(class_balance[MM]))
    small = dat[smallid,]
    extra = do.call("rbind", replicate(diff, small, simplify = FALSE))
    dat = rbind(dat, extra)
    print ("applied oversampling, resulting class distribution is: ")
    print(table(dat[,tgt]))
  }
  
  if(sampling_method == "under") {
    class_balance = table(dat[,tgt]) 
    MM = which.min(class_balance)
    small_id = which(dat[,tgt] == names(class_balance[MM]))
    large_id = which(dat[,tgt] == names(class_balance[-MM]))
    if(length(large_id)/ length(small_id) >= 2) large_id_reduced = sample(large_id, 2*length(small_id), replace = FALSE)
    if(length(large_id)/ length(small_id) < 2) large_id_reduced = sample(large_id, 1.25*length(small_id), replace = FALSE)
    all_ids = c(small_id,large_id_reduced)
    dat = dat[all_ids,]
    print ("applied undersampling, resulting class distribution is: ")
    print(table(dat[,tgt]))
  }
  
  if(sampling_method == "none") {
    print ("no sampling applied, target class distribution is: ")
    print(table(dat[,tgt]))
  }
  
  formul = reformulate(colnames(dat)[1:(ncol(dat)-length(koActivities))],response = colnames(dat)[tgt])
  if(learner == "svm") {
    obj = tune(svm, formul, data = dat, kernel = "radial", ranges = list(cost = 10^(0:2),gamma = 10^(-2:-1)))
    #print(obj$best.parameters)
    
    class.weights = NULL
    if(sampling_method == "none") {
      wts <- 100 / table(dat[,tgt])
      class.weights = wts
    }
    
    model <- svm(formul, data = dat, probability = TRUE, kernel = "radial",
                 cost=obj$best.parameters[1],gamma=obj$best.parameters[2],
                 class.weights=class.weights)
    
  }
  
  if(learner == "rf") {
    model <- randomForest(formul, data = dat, ntree = 80, importance = FALSE,do.trace=FALSE)
  }
  
  if(learner == "tree") {
    model <- rpart(formul, data = dat, cp = 0.001)
  }
  
  cleanedFileName = substring(fileInputPath,1,nchar(fileInputPath) - 4)
  saveRDS(model, file=sprintf("learners/%s_%s_%s_%s.Rds", cleanedFileName, checktype, learner, sampling_method))
  
}


#predict processing times
# runRFtime <- function(data,numFeaturesIndexes) {
#   indexes=c(numFeaturesIndexes,which (colnames(data)=="time"))
#   data = data[,indexes]
#   
#   testid = sample(1:nrow(data),round(0.2*nrow(data)),replace = F)
#   testc = data[testid,]
#   trainc = data[-testid,]
#   
#   #mod = glm(time ~ ., data = trainc, family = Gamma(link='log'))
#   mod = glm(time ~ ., data = trainc)
#   predicted <- predict(mod, testc[,-ncol(testc)], type = "response")
#   RMSE = sqrt(mean((testc[,ncol(testc)]-predicted)^2))
#   
#   return(list(mod = mod, pred = predicted, RMSE = RMSE))
# }

loadClassifiers <- function(checktype) {
  cleanedFileName = substring(fileInputPath,1,nchar(fileInputPath) - 4)
  model = readRDS(file = sprintf("learners/%s_%s_%s_%s.Rds",
                                 cleanedFileName,checktype,learner,sampling_method))
  return(list(model=model))
}


####functions for COMPUTING PROCESSING AND OVERPROCESSING####
computeRejectProbability <- function(testData, koActivities){
  #noTimeTrainingData = removeTimeColumns(data = trainingData)
  RFResults = mapply(function(x) loadClassifiers(checktype = x), koActivities)
  
  RFResults = rbind(RFResults, koActivities)
  
  # predict reject probability for each task
  predRP = apply(RFResults, 2, function(x) { 
    index = which(colnames(testData)==x$koActivities)
    if (learner == "svm") {
      pred = predict(x$RFResults, testData[, -index], probability = TRUE)
      prob <- attr(pred, "probabilities")
    }
    
    if(learner == "rf" | learner == "tree") {
      prob = predict(x$RFResults, testData[, -index], type="prob")
    }
    return(prob[,1])
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
  results = matrix(-1,nrow = nrow(rejectProbability),ncol=nrow(Order))
  rownames(results) = rownames((rejectProbability))
  colnames(results) = 1:nrow(Order)
  
  for (k in 1:nrow(rejectProbability)) {
    for (jj in 1:nrow(Order)) {
      perm = Order[jj,]
      total = 0
      for(i in 1:length(perm)){
        check = perm[i]
        checkIndexRP = which(colnames(rejectProbability)==check)
        current = ifelse(i==length(perm), 1, rejectProbability[k,checkIndexRP])
        if (i>1){
          for(j in 1:(i-1)){
            passedCheck = perm[j]
            passedCheckIndexRP = which(colnames(rejectProbability)==passedCheck)
            passedRP = (1-rejectProbability[k,passedCheckIndexRP])
            current = current * passedRP
          }
          current = current * i
        }
        total = total+current
        
      }
      results[k,jj] = total
    }
  }
  
#   results = apply(Order, 1, function(x) {
#     index = which(apply(t(Order) == x,2,all))
#     total = 0
#     for(i in 1:length(x)){
#       check = x[i]
#       checkIndexRP = which(colnames(rejectProbability)==check)
#       current = rejectProbability[,checkIndexRP]
#       if (i>1){
#         for(j in 1:(i-1)){
#           passedCheck = x[j]
#           passedCheckIndexRP = which(colnames(rejectProbability)==passedCheck)
#           passedRP = (1-rejectProbability[,passedCheckIndexRP])
#           current = current * passedRP
#         }
#         current = current * i
#       }
#       total = total+current
#     }
#     return (total)
#   })

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
  function(fileInputPath, koActivities, usefulFeatures, numFeatures,
           disallowed_permutation = c(), n=1) {
    print("**************")
    ptm <- proc.time()
    inputData = read.csv(fileInputPath,header = TRUE,sep = ",")
    if(fileInputPath=="Bondora.csv") {prepreProcessedData=preprocessBondora(inputData, koActivities=koActivities)}
    if(fileInputPath=="Envpermit.csv") {prepreProcessedData=preprocessEnvpermit(inputData, koActivities=koActivities)}
    preprocessedData = preprocessData(
      data=prepreProcessedData, koActivities = koActivities, usefulFeatures = usefulFeatures, numFeatures = numFeatures
    )
    ptm2 <- (proc.time() - ptm)[3]
    out = sprintf("%g samples read in and processed in %f seconds", nrow(preprocessedData$dataFiltered),ptm2)
    print(out)
    
    for (r in c(1:n)) {
      seed_nr=sample.int(1000000,1)
      set.seed(seed_nr)
      data_gen = generateTrainingAndTesting(dataFiltered = preprocessedData$dataFiltered)
      
      # retrain classifiers if needed
      ptm <- proc.time()
      for(checktype in koActivities) {
        trainClassifiers(data = data_gen$trainingData, checktype = checktype, learner, sampling_method)
      }
      ptm2 <- (proc.time() - ptm)[3]
      out = sprintf("%g samples used for training classifiers in %f seconds", nrow(data_gen$trainingData),ptm2)
      print(out)
      
      # computing reject probabilities
      ptm <- proc.time()
      rejectPb_gen = computeRejectProbability(testData = data_gen$testData, koActivities = koActivities)
      ptm2 <- (proc.time() - ptm)[3]
      out = sprintf("%g samples used to compute RP's in %f seconds", nrow(data_gen$testData),ptm2)
      print(out)
      colnames(rejectPb_gen)=koActivities
      
      # computing permutations
      ptm <- proc.time()
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
      our_best_perm = computePermutationsByFormula(
        rejectProbability = rejectPb_gen, koActivities = koActivities, Order = Order
      )
      ptm2 <- (proc.time() - ptm)[3]
      out = sprintf("%g samples used to compute permutations in %f seconds", nrow(data_gen$testData),ptm2)
      print(out)
      
      # Wil's model - constant probabilities of rejection "learnt" from the training set
      trainingData = data_gen$trainingData
      rejectPb_Wil = rejectPb_gen
      for (i in 1:ncol(rejectPb_Wil)) {
        foo = which(colnames(trainingData) == colnames(rejectPb_Wil)[i])
        rejectPb_Wil[,i] = sum(trainingData[,foo]==0)/nrow(trainingData)
      }
      
      Wil_best_perm = computePermutationsByFormula(
        rejectProbability = rejectPb_Wil, koActivities = koActivities, Order = Order
      )
      

      # random model
      rand_perm = sample(1:nrow(Order),nrow(data_gen$testData),replace = TRUE)
      
      newPermutations = as.data.frame(cbind (our_best_perm,Wil_best_perm,rand_perm))
      newPermutations$name = row.names(data_gen$testData)
      
      # computing number of checks when applying our, Wil and random ordering
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
      cleanedFileName = substring(fileInputPath,1,nchar(fileInputPath) - 4)
      fileName = sprintf("output/output_%s_%s_%s_%s.csv",cleanedFileName,learner,sampling_method,r)
      write.table (
        toPrint, file = fileName, append = FALSE, sep = ",", col.names = TRUE, row.names = TRUE,quote = FALSE
      )
      
      fileNameOrder = sprintf("output/output_%s_permutations.txt",cleanedFileName)
      write.table (
        Order, file = fileNameOrder, append = FALSE, sep = ",", col.names = FALSE, row.names = FALSE,quote = FALSE
      )
      
    }

  }

