#input:
##          fitted superlearner object and simulated dataset
#output:
##          a dataframe 'meanrslt', 'sdrslt' with performance metrics as well as performance metrics for individual cross validation folds
##          

#load required libraries
library(glmnet)
library(ROCR)

source('SLwrappersAVR.R')
source('foldmetric.R')

###########
###########
##adjust the following lines of codes depending the type of analysis

##as an example we present the code to summarize fits for AVR_Within30Days.
##specify name of the cohort
cohort = 'AVR30Day'
##load fits from super learner object
load(paste('FittedModel_Simulation',cohort,'.RData',sep = ''))
##load simulated data
load('SimulatedData/SimulatedDataAVR_Within30Days.RData')

#############
#############

Y = Y.sim

#specify prediction thresholds
thresholds = seq(0.05, 0.8, length = 20)

## compute Accuracy, AUC, TPR, FPR, PPV and F score for each CV fold
##
fold1 = foldMetric(fold=1, Y.sim = Y.sim,
        fit.data.SL = fit.data.SL, thresholds = thresholds)

fold2 = foldMetric(fold=2, Y.sim = Y.sim,
            fit.data.SL = fit.data.SL, thresholds = thresholds)

fold3 = foldMetric(fold=3, Y.sim = Y.sim,
    fit.data.SL = fit.data.SL, thresholds = thresholds)

fold4 = foldMetric(fold=4, Y.sim = Y.sim,
        fit.data.SL = fit.data.SL, thresholds = thresholds)

fold5 = foldMetric(fold=5, Y.sim = Y.sim,
            fit.data.SL = fit.data.SL, thresholds = thresholds)


##compute mean and standard deviation of the evaluation metrics over the CV folds

meanrslt = sdrslt = fold1$rslt
for(kk in 2:dim(meanrslt)[2]){
    meanrslt[, kk] = sapply(1:12, function(x) mean(c(fold1$rslt[x,kk],
                fold2$rslt[x,kk],
                fold3$rslt[x,kk],
                fold4$rslt[x,kk],
                fold5$rslt[x,kk]), na.rm = TRUE))
    
    sdrslt[,kk] = sapply(1:12, function(x) sd(c(fold1$rslt[x,kk],
        fold2$rslt[x,kk],
        fold3$rslt[x,kk],
        fold4$rslt[x,kk],
        fold5$rslt[x,kk]),na.rm=TRUE))
}


