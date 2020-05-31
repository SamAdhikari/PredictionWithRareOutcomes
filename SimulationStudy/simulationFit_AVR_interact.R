### Code to fit Super Learner ensemble for simulation setting 2, AVR 30 Day mortality outcome
options(java.parameters = "-Xmx10g")

cohort = 'AVR_30Days_Interact'

#load R libraries
library(gglasso)
library(glmnet)
library(msgl)
library(xtable)
library(ROCR)
library(SuperLearner)
library(logistf)
library(kernlab)
library(bartMachine)
library(xgboost)
library(nnet)

#source internal functions
source('SLwrappersAVR.R')
source('getGroupsFoldspartial.R')

#load simulated dataset
load('SimulatedDataAVR_InteractWithin30Days.RData')

##prepare data for model fitting
Y = Y.sim

groupFolds = getGroupsFoldspartial(TT,X.sim,Y)
groupIndicators = groupFolds$groupIndicators
X = X.sim[,groupFolds$varNames]

##Specify a library of algorithms to includee in the ensemble
##
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
                tune = list(alpha=c(0.15, 0.5,0.85)))

SL.library <- c("SL.mean","SL.glm",
                "SL.randomForest",
                "SL.glmnetNoStandardize","SL.glmnetT0",
                "SL.logistf","SL.glassoAVR",
                create.spgl$names,
                "SL.xgboost", "SL.nnet", "SL.nnet3",
                "SL.nnet4",
                "SL.ksvm", "SL.bartMachine")




fit.data.SL<- CV.SuperLearner(Y=Y,X=X,
                      SL.library= SL.library,
                      family=binomial(),
                      method= "method.AUC",
                      cvControl = list(V = 5, stratifyCV = TRUE),
                      innerCvControl = list(list(V = 10, stratifyCV = TRUE)),
                      verbose=TRUE)


save(fit.data.SL,file=paste('AUC_FittedModel_Simulation',cohort,'.RData',sep=''))


    
