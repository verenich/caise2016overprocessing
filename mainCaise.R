#SET THE PATH
#the path should be the same as with the source file and Data
path=getwd()
path

#PACKAGES AND SOURCE FILES
source('functions_overprocessing.R')

#used for partitioning the data
seed_nr <- 40952

#Read and pre-process the data
dset = read.csv("logs/Bondora.csv",header = TRUE,sep = ",")
dat = process_bondora(dset)

dset = read.csv("logs/env-permit.csv",header = TRUE, sep = ",")
dat = process_envpermit(dset)

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
