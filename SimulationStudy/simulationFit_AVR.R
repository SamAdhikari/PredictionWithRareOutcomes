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

##load dataset
##this dataset is provided in the folder 'SimulatedData' 
##in this GitHub repository
load('SimulatedData/SimulatedDataAVR_Within30Days.RData')

#specify cohort
cohort = 'AVR_30Day'

###
Y = Y.sim

## create group indicators for group lasso and sparse group lasso algorithms
groupFolds = getGroupsFoldspartial(TT, X.sim, Y)
groupIndicators = groupFolds$groupIndicators
X = X.sim[ , groupFolds$varNames]

##Specify the library of algorithms##
SL.glassoAVR = function(...){
    SL.glasso(..., groupid = groupIndicators)
}

SL.sparseglassoAVR = function(...){
    SL.sparseglasso(..., grouping = groupIndicators)
}

SL.glmnetNoStandardize = function(...){
    SL.glmnet(..., standardize = FALSE)
}

create.spgl = create.Learner("SL.sparseglassoAVR",
                             tune = list(alpha = c(0.15, 0.5, 0.85)))
                             
SL.library <- c("SL.mean",
				"SL.glm",
                "SL.randomForest",
                "SL.glmnetNoStandardize",
                "SL.glmnetT0",
                "SL.logistf",
                "SL.glassoAVR",
                create.spgl$names)

##Uncomment following if desired to use multiple cores for parallel computing
##specify number of cores to run in parallel processors
#options(mc.cores = 2)
#getOption("mc.cores")

fit.data.SL<- CV.SuperLearner(Y = Y, X = X,
                              SL.library= SL.library, 
                               family=binomial(),
                               method= "method.AUC",
                               cvControl = list(V = 5,  stratifyCV = TRUE),
                               innerCvControl = list(list(V = 10, stratifyCV = TRUE)),
                               verbose = TRUE,
                               parallel = 'seq')

#use parallel = 'multicore' for parallel computing

#save fitted model as a .RData file
save(fit.data.SL, file=paste('FittedModel_Simulation', cohort, '.RData', sep = '' ) )


    
