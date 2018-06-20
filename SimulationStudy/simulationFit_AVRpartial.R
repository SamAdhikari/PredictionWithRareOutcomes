load('SimulatedDataAVR_OmmittedWithin30Days.RData')
library(gglasso)
library(glmnet)
library(msgl)
library(xtable)
library(ROCR)
library(SuperLearner)
library(logistf)
source('SLwrappersAVR.R')
source('getGroupsFoldspartial.R')


##subID were used for data generation
##they will now be excluded from model fitting
cov.new = dimnames(X.sim)[[2]][-subID]

Y = Y.sim
X = X.sim[,cov.new]
cohort = 'AVR_Ommitted_30Day'

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

create.spgl = create.Learner("SL.sparseglassoAVR", tune = list(alpha=c(0.15,0.5,0.85)))
 SL.library <- c("SL.mean","SL.glm",
                 "SL.randomForest",
                 "SL.glmnetNoStandardize","SL.glmnetT0",
                 "SL.logistf", "SL.glassoAVR",
                create.spgl$names)

options(mc.cores = 5)
#Check how many parallel workers we are using (on macOS/Linux).
getOption("mc.cores")
fit.data.SL<- CV.SuperLearner(Y=Y,X=X,
                              SL.library= SL.library, 
                              family=binomial(),
                              method= "method.AUC",
                              cvControl = list(V = 5, stratifyCV = TRUE),
                              innerCvControl = list(list(V = 10, stratifyCV = TRUE)),
                              verbose=TRUE,
                              parallel='multicore')

save(fit.data.SL,file=paste('FittedModel_Simulation',cohort,'.RData',sep=''))


    