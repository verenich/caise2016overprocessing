#SET THE PATH
#the path should be the same as with the source file and Data
path=getwd()
path

#PACKAGES AND SOURCE FILES
source('functions_overprocessing.R')

#used for partitioning the data
seed_nr <- 40952

#Read and pre-process the data
Bondora = read.csv("Bondora.csv",header = TRUE,sep = ",")
invalid = which(is.na(Bondora$CreditDecision) | is.na(Bondora$IdCancellation)| is.na(Bondora$PostFundingCancellation))
Bondora = Bondora[-invalid,]

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
  foo = which(names(Bondora)==useful_features[i])
  if((sum(is.na(Bondora[,foo])))/nrow(Bondora) < 0.1) #exclude features where > 10% of values are NA's
  {useful_features_id = c(useful_features_id,foo)}
}

num_features = c("AppliedAmount","Interest","LoanDuration","nr_of_dependants","income_from_principal_employer",
                 "income_total",
                 "TotalLiabilitiesBeforeLoan","TotalMonthlyLiabilities","DebtToIncome",
                 "AppliedAmountToIncome","LiabilitiesToIncome","NoOfPreviousApplications",
                 "AmountOfPreviousApplications")



Bon = Bondora[,useful_features_id]

num_features_id = c()
for (i in 1:length(num_features)) {
  foo = which(names(Bon)==num_features[i])
  if((sum(is.na(Bon[,foo])))/nrow(Bon) < 0.1) #exclude features where > 10% of values are NA's
  {num_features_id = c(num_features_id,foo)}
}

Bon = na.omit(Bon)

#factorize targets, reverse the labels for some checks
Bon$PostFundingCancellation = factor(Bon$PostFundingCancellation,labels = c(1,0))
Bon$IdCancellation = factor(Bon$IdCancellation, labels=c(1,0))
Bon$CreditDecision = factor(Bon$CreditDecision, labels=c(0,1))

#tweak predictors
Breaks = c(0, 20, 30, 40, 50, 60, 70, 80) #age groups
Bon$Age = cut(Bon$Age, breaks = Breaks)
levels(Bon$Age) = seq(1:length(Breaks))

Bon$Gender = factor(Bon$Gender)
Bon$NewCreditCustomer = factor(Bon$NewCreditCustomer)
Bon$language_code = factor(Bon$language_code)
Bon$UseOfLoan = factor(Bon$UseOfLoan)
Bon$ApplicationType = factor(Bon$ApplicationType)
Bon$education_id = factor(Bon$education_id)
Bon$marital_status_id = factor(Bon$marital_status_id)
Bon$employment_status_id = factor(Bon$employment_status_id)
Bon$occupation_area = factor(Bon$occupation_area)
Bon$home_ownership_type_id = factor(Bon$home_ownership_type_id)

M = which(Bon$nr_of_dependants == "")
Bon = Bon[-M,]
M = which(levels(Bon$nr_of_dependants)=="10Plus")
levels(Bon$nr_of_dependants)[M] = "11"
Bon$nr_of_dependants = as.numeric(as.character(Bon$nr_of_dependants))

iddev = sample(1:nrow(Bon),round(0.2*nrow(Bon)),replace=F)
devset = Bon[iddev,] # development set - put aside 
Bon = Bon[-iddev,] 

# datC <- ovun.sample(reformulate(colnames(Bon)[-30],response = colnames(Bon)[30]), data=dat, p=0.5, seed=1, method="over")$data

tmp1 = runRF(checktype = "CreditDecision",method = "none")
tmp2 = runRF(checktype = "IdCancellation", method = "none")
tmp3 = runRF(checktype = "PostFundingCancellation",method = "none")

# generate and predict processing times
runRFtime <- function(dat=Bon,testratio= 0.2,checktype) {
  print(dim(dat))
  dat = dat[,num_features_id]
  N= nrow(dat)
  
  if (checktype == "IdCancellation") {
    dat$resp = 0.2*exp(dat[,2]*0.5*runif(N,0.1,0.14)) + 0.2*exp(dat[,3]*0.5*runif(N,0.05,0.11))
    print(summary(dat$resp))
  }
  
  if (checktype == "Creditdecision") {
    dat$resp = 0.3*exp(dat[,2]*0.16*runif(N,0.28,0.4)) + 0.45*exp(dat[,3]*0.25*runif(N,0.3,0.4))
    print(summary(dat$resp))
  }
  
  if (checktype == "PostFundingCancellation") {
    dat$resp = 0.2*exp(dat[,2]*0.5*runif(N,0.08,0.17)) + 0.2*exp(dat[,3]*0.5*runif(N,0.15,0.2))
    print(summary(dat$resp))
  }
  
  testid = sample(1:nrow(dat),round(testratio*nrow(dat)),replace = F)
  testc = dat[testid,]
  trainc = dat[-testid,]
  
  
  #rf <- randomForest(resp ~ ., data = trainc, ntree = 50, importance = TRUE,do.trace=FALSE)
  
  mod = glm(resp ~ ., data = trainc, family = Gamma(link='log'))
  predicted <- predict(mod, testc[,-ncol(testc)], type = "response")
  RMSE = sqrt(mean((testc[,ncol(testc)]-predicted)^2))
  
  #predicted2 <- predict(rf, testc[,-ncol(testc)], type = "response")
  #RMSE2 = sqrt(mean((testc[,ncol(testc)]-predicted2)^2))
  
  return(list(mod = mod, pred = predicted, RMSE = RMSE))
}


tmp4 = runRFtime(dat= Bon, checktype = "IdCancellation")
tmp5 = runRFtime(dat= Bon, checktype = "Creditdecision")
tmp6 = runRFtime(dat= Bon, checktype = "PostFundingCancellation")

# predict reject Pr and processing times for each task
predRPC <- predict(tmp1$rf, devset[,-30], type = "prob")
predRPC <- predRPC[,2]
predRPI <- predict(tmp2$rf, devset[,-29], type = "prob")
predRPI <- predRPI[,2]
predRPP <- predict(tmp3$rf, devset[,-28], type = "prob")
predRPP <- predRPP[,2]

Devset = devset[,num_features_id]
predPTC <- predict(tmp5$mod, Devset, type = "response")
predPTI <- predict(tmp4$mod, Devset, type = "response")
predPTP <- predict(tmp6$mod, Devset, type = "response")

#perm1 (baseline) - I + C + P
devset$perm1 = predPTI*predRPI + predPTC*predRPC*(1-predRPI) + predPTP*predRPP*(1-predRPI)*(1-predRPC)

#perm2 (baseline) - I + P + C
devset$perm2 = predPTI*predRPI + predPTP*predRPP*(1-predRPI) + predPTC*predRPC*(1-predRPI)*(1-predRPP)

#perm3 (baseline) - C + I + P
devset$perm3 = predPTC*predRPC + predPTI*predRPI*(1-predRPC) + predPTP*predRPP*(1-predRPI)*(1-predRPC)

#perm4 (baseline) - C + P + I
devset$perm4 = predPTC*predRPC + predPTP*predRPP*(1-predRPC) + predPTI*predRPI*(1-predRPC)*(1-predRPP)

#perm5 (baseline) - P + C + I
devset$perm5 = predPTP*predRPP + predPTC*predRPC*(1-predRPP) + predPTI*predRPI*(1-predRPP)*(1-predRPC)

#perm6 (baseline) - P + I + C
devset$perm6 = predPTP*predRPP + predPTI*predRPI*(1-predRPP) + predPTC*predRPC*(1-predRPI)*(1-predRPP)

# finding best permutation
Perm = devset[,31:36]
Perm$best = apply(Perm, 1, function(x) which.min(x) )
table(Perm$best)
