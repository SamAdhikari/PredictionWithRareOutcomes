##make sure the working directory is '/PredictionWithRareOutcomes'

#load required libraries
library(gglasso)
library(glmnet)
library(msgl)
library(xtable)
library(ROCR)
library(SuperLearner)
library(logistf)
library(randomForest)

#source r scripts
source('SLwrappersAVR.R')
source('SimulationStudy/getGroupsFoldspartial.R')

#load .Rdata file included in subfolder 'SimulatedData'
load('SimulatedData/SimulatedDataAVR_OmmittedWithin30Days.RData')

#specify cohort
cohort = 'AVR_Ommitted_30Day'

##subID were used for data generation
##they will now be excluded from model fitting
cov.new = dimnames(X.sim)[[2]][ -subID]
X = X.sim[ , cov.new]

Y = Y.sim

## create group indicators for group lasso and sparse group lasso algorithms
groupFolds = getGroupsFoldspartial(TT = 0, X, Y)
groupIndicators = groupFolds$groupIndicators
X = X[ , groupFolds$varNames]

##Specify a library of algorithms##

SL.glassoAVR = function(...){
    SL.glasso(..., groupid = groupIndicators)
}

SL.sparseglassoAVR = function(...){
    SL.sparseglasso(..., grouping = groupIndicators)
}

SL.glmnetNoStandardize = function(...){
    SL.glmnet(..., standardize = FALSE)
}

create.spgl = create.Learner("SL.sparseglassoAVR", tune = list(alpha = c(0.15, 0.5, 0.85)))


 SL.library <- c("SL.mean",
                 "SL.glm",
                 "SL.randomForest",
                 "SL.glmnetNoStandardize",
                 "SL.glmnetT0",
                 "SL.logistf",
                 "SL.glassoAVR",
                 create.spgl$names)

##specify number of cores to run in paraller processors
##Uncomment following if desired to use multiple cores for parallel computing
#options(mc.cores = 2)
#getOption("mc.cores")

fit.data.SL = CV.SuperLearner(Y = Y, X = X,
                              SL.library = SL.library,
                              family = binomial(),
                              method = "method.AUC",
                              cvControl = list(V = 5, stratifyCV = TRUE),
                              innerCvControl = list(list(V = 10, stratifyCV = TRUE)),
                              verbose = TRUE,
                              parallel = 'seq')
#use parallel = 'multicore' for parallel computing

#save fitted model as a .RData file
save(fit.data.SL,file = paste('FittedModel_Simulation', cohort, '.RData',sep = ''))


    
