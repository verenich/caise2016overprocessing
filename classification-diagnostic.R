rm(list=ls())


#PACKAGES AND SOURCE FILES
source('functions_overprocessing.R')
set.seed(40132)
koActivities=c("T02","T06","T10")

usefulFeatures = c("Resource","X.case..channel","X.case..department","X.case..group","X.case..responsible",
                   "org.group", koActivities,paste(koActivities,"_time",sep = ""))

numFeatures = c()

fileInputPath = "Envpermit.csv"
inputData = read.csv(fileInputPath,header = TRUE,sep = ",")
prepreProcessedData=preprocessEnvpermit(inputData, koActivities=koActivities)
preprocessedData = preprocessData(
  data=prepreProcessedData, koActivities = koActivities, usefulFeatures = usefulFeatures, numFeatures = numFeatures
)
data_gen = generateTrainingAndTesting(dataFiltered = preprocessedData$dataFiltered, testratio = 0.5)


learner="rf"
sampling_method = "under"
for (checktype in koActivities) {
  sprintf("training classifiers for method %s checktype %s",sampling_method,checktype)
  trainClassifiers(data = data_gen$trainingData, checktype = checktype,
                   learner=learner, sampling_method=sampling_method)
  cleanedFileName = substring(fileInputPath,1,nchar(fileInputPath) - 4)
  model = readRDS(file = sprintf("learners/%s_%s_%s_%s.Rds",
                                 cleanedFileName,checktype,learner,sampling_method))
  sprintf("applying classifiers for method %s checktype %s",sampling_method,checktype)
  testc = data_gen$testData
  tgt = which(colnames(testc) == checktype)
  if(learner == "rf") predicted <- predict(model, testc[,-tgt], type = "response")
  if(learner == "tree") predicted <- predict(model, testc[,-tgt], type = "class")
  tt = table(pred=predicted, actual=testc[,tgt])
  err = 1 - sum(diag(tt))/sum(tt)
  
  prob1 <- predict(model, testc[,-tgt], type = "prob")
  predd <- prediction(prob1[,2], testc[,tgt])
  
  AUC = as.numeric(performance(predd, measure = "auc", x.measure = "cutoff")@y.values)
  print(AUC)
  
  save(tt,err,AUC,file = sprintf("diag/performance_%s_%s_%s_%s.Rdata",
                                 cleanedFileName,checktype,learner,sampling_method))
  
  Roc = performance(predd, measure = "tpr", x.measure = "fpr")
  saveRDS(Roc,file = sprintf("diag/ROC_%s_%s_%s_%s.Rds",
                             cleanedFileName,checktype,learner,sampling_method))
  
}

save(data_gen, file= sprintf("diag/dataset_%s_%s_%s.Rdata",
                             cleanedFileName,learner,sampling_method))


####

rm(list=ls())

#PACKAGES AND SOURCE FILES
source('functions_overprocessing.R')

koActivities=c("IdCancellation", "PostFundingCancellation", "CreditDecision")

usefulFeatures = c("Age","Gender","Country","NewCreditCustomer","language_code","education_id",
                   "marital_status_id","nr_of_dependants","employment_status_id",
                   "Employment_Duration_Current_Employer","work_experience","occupation_area",
                   "home_ownership_type_id",
                   "income_from_principal_employer","income_total","TotalLiabilitiesBeforeLoan",
                   "TotalMonthlyLiabilities","DebtToIncome",
                   "AppliedAmountToIncome","LiabilitiesToIncome","NoOfPreviousApplications",
                   "AmountOfPreviousApplications",
                   "AppliedAmount","Interest","LoanDuration","UseOfLoan","ApplicationType",
                   koActivities,paste(koActivities,"_time",sep = ""))

numFeatures = c("AppliedAmount","Interest","LoanDuration","nr_of_dependants","income_from_principal_employer",
                "income_total",
                "TotalLiabilitiesBeforeLoan","TotalMonthlyLiabilities","DebtToIncome",
                "AppliedAmountToIncome","LiabilitiesToIncome","NoOfPreviousApplications",
                "AmountOfPreviousApplications")

fileInputPath = "Bondora.csv"
inputData = read.csv(fileInputPath,header = TRUE,sep = ",")
prepreProcessedData=preprocessBondora(inputData, koActivities=koActivities)
preprocessedData = preprocessData(
  data=prepreProcessedData, koActivities = koActivities, usefulFeatures = usefulFeatures, numFeatures = numFeatures
)
data_gen = generateTrainingAndTesting(dataFiltered = preprocessedData$dataFiltered)

learner="svm"
sampling_method = "under"
for (checktype in koActivities) {
  sprintf("training classifiers for method %s checktype %s",sampling_method,checktype)
  trainClassifiers(data = data_gen$trainingData, checktype = checktype,
		    learner=learner, sampling_method=sampling_method)
  cleanedFileName = substring(fileInputPath,1,nchar(fileInputPath) - 4)
  model = readRDS(file = sprintf("learners/%s_%s_%s_%s.Rds",
				  cleanedFileName,checktype,learner,sampling_method))
  sprintf("applying classifiers for method %s checktype %s",sampling_method,checktype)
  testc = data_gen$testData
  tgt = which(colnames(testc) == checktype)
  predicted <- predict(model, testc[,-tgt])
  tt = table(pred=predicted, actual=testc[,tgt])
  err = 1 - sum(diag(tt))/sum(tt)
  
  prob1 <- predict(model, testc[,-tgt], probability = TRUE)
  prob_values <- attr(prob1, "probabilities")
  predd <- prediction(prob_values[,2], testc[,tgt])
  
  AUC = as.numeric(performance(predd, measure = "auc", x.measure = "cutoff")@y.values)
    
  save(tt,err,AUC,file = sprintf("diag/performance_%s_%s_%s_%s.Rdata",
				  cleanedFileName,checktype,learner,sampling_method))
  Roc = performance(predd, measure = "tpr", x.measure = "fpr")
  saveRDS(Roc,file = sprintf("diag/ROC_%s_%s_%s_%s.Rds",
                              cleanedFileName,checktype,learner,sampling_method))
}

save(data_gen, file= sprintf("diag/dataset_%s_%s_%s.Rdata",
				  cleanedFileName,learner,sampling_method))

