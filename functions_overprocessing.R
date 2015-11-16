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