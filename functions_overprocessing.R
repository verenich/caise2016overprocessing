#CHECK WHETHER ALL PACKAGES ARE INSTALLED. IF NOT, INSTALL MISSING
required_packages <- c('randomForest','rpart','ggplot2','reshape','MASS','ROCR',
                       'rpart.plot')
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


##--------------##
#functions for data preprocessing for specific datasets
process_bondora <- function(dat) {
  invalid = which(is.na(dat$CreditDecision) | is.na(dat$IdCancellation)| is.na(dat$PostFundingCancellation))
  dat = dat[-invalid,]
  
  useful_features = c("Age","Gender","Country","NewCreditCustomer","language_code","education_id",
                      "marital_status_id","nr_of_dependants","employment_status_id",
                      "Employment_Duration_Current_Employer","work_experience","occupation_area",
                      "home_ownership_type_id",
                      "income_from_principal_employer","income_total","TotalLiabilitiesBeforeLoan",
                      "TotalMonthlyLiabilities","DebtToIncome",
                      "AppliedAmountToIncome","LiabilitiesToIncome","NoOfPreviousApplications",
                      "AmountOfPreviousApplications",
                      "AppliedAmount","Interest","LoanDuration","UseOfLoan","ApplicationType",
                      "PostFundingCancellation","IdCancellation","CreditDecision")
  
  useful_features_id = c()
  for (i in 1:length(useful_features)) {
    foo = which(names(dat)==useful_features[i])
    if((sum(is.na(dat[,foo])))/nrow(dat) < 0.1) #exclude features where > 10% of values are NA's
    {useful_features_id = c(useful_features_id,foo)}
  }
  
  num_features = c("AppliedAmount","Interest","LoanDuration","nr_of_dependants","income_from_principal_employer",
                   "income_total",
                   "TotalLiabilitiesBeforeLoan","TotalMonthlyLiabilities","DebtToIncome",
                   "AppliedAmountToIncome","LiabilitiesToIncome","NoOfPreviousApplications",
                   "AmountOfPreviousApplications")
  
  dat = dat[,useful_features_id]
  
  num_features_id = c()
  for (i in 1:length(num_features)) {
    foo = which(names(dat)==num_features[i])
    if((sum(is.na(dat[,foo])))/nrow(dat) < 0.1) #exclude features where > 10% of values are NA's
    {num_features_id = c(num_features_id,foo)}
  }
  
  dat = na.omit(dat)
  
  #factorize targets, reverse the labels for some checks
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
  
  return(dat)
}

process_envpermit <- function(dat) {
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
  
  useful_features = c("Resource","X.case..channel","X.case..department","X.case..group","X.case..responsible","org.group","whichfailed")
  useful_features_id = c()
  for (i in 1:length(useful_features)) {
    foo = which(names(dat)==useful_features[i])
    if((sum(is.na(dat[,foo])))/nrow(dat) < 0.1) #exclude features where > 10% of values are NA's
    {useful_features_id = c(useful_features_id,foo)}
  }
  
  dat = dat[,useful_features_id]
  
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
  
  return(dat)
}

##--------------##
#functions for training models to predict the checks outcome
runRF <- function(dat=Bon,testratio= 0.2,checktype,method) {
  tgt = which(colnames(dat) == checktype)
  
  #oversampling
  print(table(dat[,tgt]))
  
  dat_bal <- dat
  if(method != "none") {dat_bal <- ovun.sample(reformulate(colnames(dat)[-tgt],response = colnames(dat)[tgt]), data=dat, p=0.5, seed=1, method="over")$data}
  print(reformulate(colnames(dat)[-tgt],response = colnames(dat)[tgt]))
  
  testid = sample(1:nrow(dat_bal),round(testratio*nrow(dat_bal)),replace = F)
  testc = dat_bal[testid,]
  trainc = dat_bal[-testid,]
  
  rf <- randomForest(x=trainc[,1:(ncol(dat_bal)-3)], y = trainc[,tgt], ntree = 100, importance = TRUE,do.trace=FALSE)
  
  predicted <- predict(rf, testc[,-tgt], type = "response")
  tt = table(pred=predicted, actual=testc[,tgt])
  err = 1 - sum(diag(tt))/sum(tt)
  
  prob1 <- predict(rf, testc[,-tgt], type = "prob")
  predd <- prediction(prob1[,2], testc[,tgt])
  AUC = as.numeric(performance(predd, measure = "auc", x.measure = "cutoff")@y.values)
  
  return(list(rf = rf, AUC = AUC, imp = rf$importance, tt = tt, err = err, pred_bin = predicted, pred = prob1[,2]))
}

runDT <- function(dat=Bon,testratio= 0.2,checktype,method) {
  tgt = which(colnames(dat) == checktype)
  
  #oversampling
  print(table(dat[,tgt]))
  form = reformulate(colnames(dat)[-tgt],response = colnames(dat)[tgt])
  ifelse(method == "none", {dat_bal <- dat}, {dat_bal <- ovun.sample(form, data=dat, p=0.5, seed=1, method=method)$dat})
  
  testid = sample(1:nrow(dat_bal),round(testratio*nrow(dat_bal)),replace = F)
  testc = dat_bal[testid,]
  trainc = dat_bal[-testid,]
  
  form2 = reformulate(colnames(dat)[1:(ncol(dat)-3)],response = colnames(dat)[tgt])
  obj2 = tune.rpart(form2, data = trainc, cp = c(0.01,0.1,0.2,0.5,0.8))
  tree <- rpart(form2, data = trainc, cp = obj2$best.parameters[1])
  
  predicted <- predict(tree, testc[,-tgt], type = "class")
  tt = table(pred=predicted, actual=testc[,tgt])
  err = 1 - sum(diag(tt))/sum(tt)
  
  prob1 <- predict(tree, testc[,-tgt], type = "prob")
  predd <- prediction(prob1[,2], testc[,tgt])
  AUC = as.numeric(performance(predd, measure = "auc", x.measure = "cutoff")@y.values)
  
  return(list(tree = tree, AUC = AUC, tt = tt, err = err, pred_bin = predicted, pred = prob1[,2]))
}

