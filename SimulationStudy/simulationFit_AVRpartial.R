options(java.parameters = "-Xmx10g")
#load('PartialMEf_SimulatedDataAVR_Ommitted_1Yr.RData')
load('PartialMEf_SimulatedDataAVR_OmmittedWithin30Days.RData')
library(gglasso)
library(glmnet)
library(msgl)
library(xtable)
library(ROCR)
library(SuperLearner)
library(logistf)
library(kernlab)
library(bartMachine)
#library(dbarts)
library(xgboost)
library(nnet)

source('SLwrappersAVR.R')
#source('getGroupsFoldsAVR.R')
source('getGroupsFoldspartial.R')

cov.new = dimnames(X.sim)[[2]][-subID]

Y = Y.sim
X = X.sim[,cov.new]
cohort = 'AVR_Ommitted_30Days'

groupFolds = getGroupsFoldspartial(TT=0,X,Y)
groupIndicators = groupFolds$groupIndicators
X = X[,groupFolds$varNames]

##Specify a library of algorithms##
#groupIndicators = 1:dim(X)[2]
SL.glassoAVR = function(...){
    SL.glasso(...,groupid = groupIndicators)
}

SL.sparseglassoAVR = function(...){
    SL.sparseglasso(...,grouping = groupIndicators)
}

SL.glmnetNoStandardize = function(...){
    SL.glmnet(...,standardize=FALSE)
}

SL.nnet3 = function(...){
    SL.nnet(..., size = 3)
}

SL.nnet4 = function(...){
    SL.nnet(..., size = 4)
}

create.spgl = create.Learner("SL.sparseglassoAVR",
tune = list(alpha=c(0.15))) #,0.5,0.85)))


SL.library <- c("SL.mean","SL.glm",
               "SL.randomForest",
               "SL.glmnetNoStandardize","SL.glmnetT0",
               "SL.logistf","SL.glassoAVR",
                create.spgl$names,
               "SL.xgboost", "SL.nnet", "SL.nnet3",
               "SL.nnet4",
               "SL.ksvm", "SL.bartMachine")

#options(mc.cores = 3)
#Check how many parallel workers we are using (on macOS/Linux).
#getOption("mc.cores")

fit.data.SL<- CV.SuperLearner(Y=Y,X=X,
                 SL.library= SL.library,
                 family=binomial(),
                 method= "method.AUC",
                 cvControl = list(V = 5, stratifyCV = TRUE),
                 innerCvControl = list(list(V = 10, stratifyCV = TRUE)),
                 verbose=TRUE)

save(fit.data.SL,file=paste('AUC_FittedModel_Simulation',cohort,'.RData',sep=''))


    
