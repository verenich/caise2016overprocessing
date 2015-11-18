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


##--------------##

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

