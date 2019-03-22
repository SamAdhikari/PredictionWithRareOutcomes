#input:
##          fitted superlearner object and simulated dataset
#output:
##          a dataframe 'rslt' with performance metrics
##          threshold plots saved as pdf files


#load required libraries
library(glmnet)
library(ROCR)

source('SLwrappersAVR.R')

###########
###########
##adjust the following lines of codes depending the types of analysis

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
##
pr.naive = fit.data.SL$library.predict[, 1]
pr.logistic = fit.data.SL$library.predict[, 2]
pr.randomforest = fit.data.SL$library.predict[, 3]
pr.lasso = fit.data.SL$library.predict[, 4]
pr.lassoT0 = fit.data.SL$library.predict[, 5]
pr.logistf = fit.data.SL$library.predict[, 6]
pr.glasso = fit.data.SL$library.predict[, 7]
pr.sglasso1 = fit.data.SL$library.predict[, 8]
pr.sglasso2 = fit.data.SL$library.predict[, 9]
pr.sglasso3 = fit.data.SL$library.predict[, 10]
pr.SL = fit.data.SL$SL.predict


##report True positive, False negative, misclassification error and AUC score
predict.logistic = lapply(1:length(thresholds), function(x)
    1*(pr.logistic > thresholds[x]))
predict.lasso = lapply(1:length(thresholds), function(x)
    1*(pr.lasso > thresholds[x]))
predict.lassoT0 = lapply(1:length(thresholds), function(x)
    1*(pr.lassoT0 > thresholds[x]))
predict.naive = lapply(1:length(thresholds), function(x)
    1*(pr.naive > thresholds[x]))
predict.glasso = lapply(1:length(thresholds), function(x)
    1*(pr.glasso > thresholds[x]))
predict.rf = lapply(1:length(thresholds), function(x)
    1*(pr.randomforest > thresholds[x]))
predict.logistf = lapply(1:length(thresholds), function(x)
    1*(pr.logistf > thresholds[x]))
predict.sglasso1 = lapply(1:length(thresholds), function(x)
    1*(pr.sglasso1 > thresholds[x]))
predict.sglasso2 = lapply(1:length(thresholds), function(x)
    1*(pr.sglasso2 > thresholds[x]))
predict.sglasso3 = lapply(1:length(thresholds), function(x)
    1*(pr.sglasso3 > thresholds[x]))
predict.SL = lapply(1:length(thresholds), function(x)
    1*(pr.SL > thresholds[x]))

##MC error
MCerrNaive = sapply(1:length(thresholds), function(x)
    MCerrfunc(predictions = predict.naive[[x]],
              trueVal = Y))
MCerrlogistic = sapply(1:length(thresholds), function(x)
    MCerrfunc(predictions = predict.logistic[[x]],
              trueVal = Y))
MCerrlasso = sapply(1:length(thresholds), function(x)
    MCerrfunc(predictions = predict.lasso[[x]],
              trueVal = Y))
MCerrlassoT0 = sapply(1:length(thresholds), function(x)
    MCerrfunc(predictions = predict.lassoT0[[x]],
              trueVal = Y))
MCerrglasso = sapply(1:length(thresholds), function(x)
    MCerrfunc(predictions = predict.glasso[[x]],
              trueVal = Y))
MCerrRF = sapply(1:length(thresholds), function(x)
    MCerrfunc(predictions = predict.rf[[x]],
              trueVal = Y))
MCerrlogistf = sapply(1:length(thresholds), function(x)
    MCerrfunc(predictions = predict.logistf[[x]],
              trueVal = Y))
MCerrsglasso1 = sapply(1:length(thresholds), function(x)
    MCerrfunc(predictions = predict.sglasso1[[x]],
              trueVal = Y))
MCerrsglasso2 = sapply(1:length(thresholds), function(x)
    MCerrfunc(predictions = predict.sglasso2[[x]],
              trueVal = Y))
MCerrsglasso3 = sapply(1:length(thresholds), function(x)
    MCerrfunc(predictions = predict.sglasso3[[x]],
              trueVal = Y))
MCerrSL = sapply(1:length(thresholds), function(x)
    MCerrfunc(predictions = predict.SL[[x]],
              trueVal = Y))
##TPR 
TPRnaive = sapply(1:length(thresholds),
                  function(x)TPRfunc(predictions = predict.naive[[x]],
                                     trueVal = Y))
TPRlogistic = sapply(1:length(thresholds),
                     function(x)TPRfunc(predictions = predict.logistic[[x]],
                                        trueVal = Y))
TPRlasso = sapply(1:length(thresholds),
                  function(x)TPRfunc(predictions = predict.lasso[[x]],
                                     trueVal = Y))
TPRlassoT0 = sapply(1:length(thresholds),
                    function(x)TPRfunc(predictions = predict.lassoT0[[x]],
                                       trueVal = Y))
TPRlogistf = sapply(1:length(thresholds),
                    function(x)TPRfunc(predictions = predict.logistf[[x]],
                                       trueVal = Y))
TPRrf = sapply(1:length(thresholds),
              function(x)TPRfunc(predictions = predict.rf[[x]],
                                 trueVal = Y))
TPRglasso = sapply(1:length(thresholds),
                  function(x)TPRfunc(predictions=predict.glasso[[x]],
                                     trueVal=Y))
TPRsglasso1 = sapply(1:length(thresholds),
                   function(x)TPRfunc(predictions=predict.sglasso1[[x]],
                                      trueVal=Y))
TPRsglasso2 = sapply(1:length(thresholds),
                    function(x)TPRfunc(predictions = predict.sglasso2[[x]],
                                       trueVal = Y))
TPRsglasso3 = sapply(1:length(thresholds),
                    function(x)TPRfunc(predictions = predict.sglasso3[[x]],
                                       trueVal = Y))
TPRSL = sapply(1:length(thresholds),
                    function(x)TPRfunc(predictions = predict.SL[[x]],
                                       trueVal = Y))

#FPR
FPRnaive = sapply(1:length(thresholds),
                  function(x)FPRfunc(predictions = predict.naive[[x]],
                                     trueVal = Y))
FPRlogistic = sapply(1:length(thresholds), function(x)
    FPRfunc(predictions = predict.logistic[[x]],
            trueVal = Y))
FPRlasso = sapply(1:length(thresholds), function(x)
    FPRfunc(predictions = predict.lasso[[x]],
            trueVal = Y))
FPRlassoT0 = sapply(1:length(thresholds), function(x)
    FPRfunc(predictions = predict.lassoT0[[x]],
            trueVal = Y))
FPRlogistf = sapply(1:length(thresholds), function(x)
    FPRfunc(predictions = predict.logistf[[x]],
            trueVal = Y))
FPRrf = sapply(1:length(thresholds), function(x)
    FPRfunc(predictions = predict.rf[[x]],
            trueVal = Y))
FPRglasso = sapply(1:length(thresholds), function(x)
    FPRfunc(predictions = predict.glasso[[x]],
            trueVal = Y))
FPRsglasso1 = sapply(1:length(thresholds), function(x)
    FPRfunc(predictions = predict.sglasso1[[x]],
            trueVal = Y))
FPRsglasso2 = sapply(1:length(thresholds), function(x)
    FPRfunc(predictions = predict.sglasso2[[x]],
            trueVal = Y))
FPRsglasso3 = sapply(1:length(thresholds), function(x)
    FPRfunc(predictions = predict.sglasso3[[x]],
            trueVal = Y))
FPRSL = sapply(1:length(thresholds), function(x)
    FPRfunc(predictions = predict.SL[[x]],
            trueVal = Y))
##Positive predictive value PPV
PPVnaive = sapply(1:length(thresholds), function(x)
    PPVfunc(predictions = predict.naive[[x]],
            trueVal = Y))
PPVlogistic = sapply(1:length(thresholds), function(x)
    PPVfunc(predictions = predict.logistic[[x]],
            trueVal = Y))
PPVlasso = sapply(1:length(thresholds), function(x)
    PPVfunc(predictions = predict.lasso[[x]],
            trueVal = Y))
PPVglasso = sapply(1:length(thresholds), function(x)
    PPVfunc(predictions = predict.glasso[[x]],
            trueVal = Y))
PPVlassoT0 = sapply(1:length(thresholds), function(x)
    PPVfunc(predictions = predict.lassoT0[[x]],
            trueVal = Y))
PPVlogistf = sapply(1:length(thresholds), function(x)
    PPVfunc(predictions = predict.logistf[[x]],
            trueVal = Y))
PPVrf = sapply(1:length(thresholds), function(x)
    PPVfunc(predictions = predict.rf[[x]],
            trueVal = Y))
PPVsglasso1 = sapply(1:length(thresholds), function(x)
    PPVfunc(predictions = predict.sglasso1[[x]],
            trueVal = Y))
PPVsglasso2 = sapply(1:length(thresholds), function(x)
    PPVfunc(predictions = predict.sglasso2[[x]],
            trueVal = Y))
PPVsglasso3 = sapply(1:length(thresholds), function(x)
    PPVfunc(predictions = predict.sglasso3[[x]],
            trueVal = Y))
PPVSL = sapply(1:length(thresholds), function(x)
    PPVfunc(predictions = predict.SL[[x]],
            trueVal = Y))
##AUC
###ROC performance
pred.naive = prediction(pr.naive, Y)
roc.perf.naive = performance(pred.naive,
                             measure = 'tpr', x.measure = 'fpr')
pred.logistic = prediction(pr.logistic, Y)
roc.perf.logistic = performance(pred.logistic,
                                measure = 'tpr', x.measure = 'fpr')
pred.logistf = prediction(pr.logistf, Y)
roc.perf.logistf = performance(pred.logistf,
                               measure = 'tpr', x.measure = 'fpr')
pred.lasso = prediction(pr.lasso , Y)
roc.perf.lasso = performance(pred.lasso,
                             measure = 'tpr', x.measure = 'fpr')
pred.lassoT0 = prediction(pr.lassoT0 , Y)
roc.perf.lassoT0 = performance(pred.lassoT0,
                               measure = 'tpr', x.measure = 'fpr')
pred.rf = prediction(pr.randomforest , Y)
roc.perf.rf = performance(pred.rf,
                          measure = 'tpr', x.measure = 'fpr')
pred.glasso = prediction(pr.glasso, Y)
roc.perf.glasso = performance(pred.glasso,
                              measure='tpr',x.measure = 'fpr')
pred.sglasso1 = prediction(pr.sglasso1, Y)
roc.perf.sglasso1 = performance(pred.sglasso1,
                               measure='tpr',x.measure = 'fpr')
pred.sglasso2 = prediction(pr.sglasso2, Y)
roc.perf.sglasso2 = performance(pred.sglasso2,
                               measure='tpr',x.measure = 'fpr')
pred.sglasso3 = prediction(pr.sglasso3, Y)
roc.perf.sglasso3 = performance(pred.sglasso3,
                               measure='tpr',x.measure = 'fpr')

pred.superlearner = prediction(pr.SL, Y)
roc.perf.superlearner = performance(pred.superlearner,
                                measure='tpr',x.measure = 'fpr')

perf.naive = performance(pred.naive,measure = 'auc')
perf.logistic =  performance(pred.logistic,measure = 'auc')
perf.logistf = performance(pred.logistf,measure = 'auc')
perf.lasso = performance(pred.lasso,measure = 'auc')
perf.lassoT0 = performance(pred.lassoT0,measure = 'auc')
perf.rf = performance(pred.rf,measure = 'auc')
perf.glasso = performance(pred.glasso,measure = 'auc')
perf.sglasso1 = performance(pred.sglasso1,measure = 'auc')
perf.sglasso2 = performance(pred.sglasso2,measure = 'auc')
perf.sglasso3 = performance(pred.sglasso3,measure = 'auc')
perf.superlearner = performance(pred.superlearner,measure = 'auc')



lasso_id = which((1-MCerrlasso) == max((1-MCerrlasso)))[1]
lassoT0_id = which((1-MCerrlassoT0) == max((1-MCerrlassoT0)))[1]
logit_id = which((1-MCerrlogistic) == max((1-MCerrlogistic)))[1]
glasso_id = which((1-MCerrglasso) == max((1-MCerrglasso)))[1]
RF_id = which((1-MCerrRF) == max((1-MCerrRF)))[1]
SL_id = which((1-MCerrSL) == max((1-MCerrSL)))[1]
logistf_id = which((1-MCerrlogistf) == max((1-MCerrlogistf)))[1]
sglasso1_id=which((1-MCerrsglasso1) == max((1-MCerrsglasso1)))[1]
sglasso2_id=which((1-MCerrsglasso2) == max((1-MCerrsglasso2)))[1]
sglasso3_id =which((1-MCerrsglasso3) == max((1-MCerrsglasso3)))[1]

##sensitivity = TPR
TPR_lasso = TPRlasso[lasso_id]
TPR_logistic = TPRlogistic[logit_id]
TPR_glasso = TPRglasso[glasso_id]
TPR_SL = TPRSL[SL_id]
TPR_RF = TPRrf[RF_id]
TPR_lassoT0 = TPRlassoT0[lassoT0_id]
TPR_logistf = TPRlogistf[logistf_id]
TPR_sglasso1 = TPRsglasso1[sglasso1_id]
TPR_sglasso2 = TPRsglasso2[sglasso2_id]
TPR_sglasso3 = TPRsglasso3[sglasso3_id]


##specificity = 1- FPR
FPR_lasso = FPRlasso[lasso_id]
FPR_logistic = FPRlogistic[logit_id]
FPR_glasso = FPRglasso[glasso_id]
FPR_SL = FPRSL[SL_id]
FPR_RF = FPRrf[RF_id]
FPR_lassoT0 = FPRlassoT0[lassoT0_id]
FPR_logistf = FPRlogistf[logistf_id]
FPR_sglasso1 = FPRsglasso1[sglasso1_id]
FPR_sglasso2 = FPRsglasso2[sglasso2_id]
FPR_sglasso3 = FPRsglasso3[sglasso3_id]

#positive prediction value
PPV_lasso = PPVlasso[lasso_id]
PPV_logistic = PPVlogistic[logit_id]
PPV_glasso = PPVglasso[glasso_id]
PPV_SL = PPVSL[SL_id]
PPV_RF = PPVrf[RF_id]
PPV_lassoT0 = PPVlassoT0[lassoT0_id]
PPV_logistf = PPVlogistf[logistf_id]
PPV_sglasso1 = PPVsglasso1[sglasso1_id]
PPV_sglasso2 = PPVsglasso2[sglasso2_id]
PPV_sglasso3 = PPVsglasso3[sglasso3_id]

##accuracy
accuracy_lasso = 1 - MCerrlasso[lasso_id]
accuracy_logistic = 1 - MCerrlogistic[logit_id]
accuracy_glasso = 1 - MCerrglasso[glasso_id]
accuracy_SL = 1 - MCerrSL[SL_id]
accuracy_RF = 1 - MCerrRF[RF_id]
accuracy_lassoT0 = 1 - MCerrlassoT0[lassoT0_id]
accuracy_logistf = 1 - MCerrlogistf[logistf_id]
accuracy_sglasso1 = 1 - MCerrsglasso1[sglasso1_id]
accuracy_sglasso2 = 1 - MCerrsglasso2[sglasso2_id]
accuracy_sglasso3 = 1 - MCerrsglasso3[sglasso3_id]


rslt = data.frame('Method'=c('Logistic','LogistF','Lasso',
                             'LassoT0','Glasso','Sglasso1',
                             'Sglasso2','Sglasso3','RandomForest','SuperLearner'),
                  'TPR' = c(TPR_logistic,TPR_logistf,TPR_lasso,TPR_lassoT0,
                            TPR_glasso,TPR_sglasso1,TPR_sglasso2,TPR_sglasso3,
                            TPR_RF,TPR_SL),
                  'FPR' = c(FPR_logistic,FPR_logistf,FPR_lasso,FPR_lassoT0,
                            FPR_glasso,FPR_sglasso1,FPR_sglasso2,FPR_sglasso3,
                            FPR_RF,FPR_SL),
                  'PPV' = c(PPV_logistic,PPV_logistf,PPV_lasso,PPV_lassoT0,
                            PPV_glasso,PPV_sglasso1,PPV_sglasso2,PPV_sglasso3,
                            PPV_RF,PPV_SL),
                  'Accuracy' = c(accuracy_logistic,accuracy_logistf,accuracy_lasso,accuracy_lassoT0,
                                 accuracy_glasso,accuracy_sglasso1,accuracy_sglasso2,accuracy_sglasso3,
                                 accuracy_RF,accuracy_SL),
                  'AUC' =  unlist(c(attributes(perf.logistic)$y.values[1],
                             attributes(perf.logistf)$y.values[1],
                             attributes(perf.lasso)$y.values[1],
                             attributes(perf.lassoT0)$y.values[1],
                             attributes(perf.glasso)$y.values[1],
                             attributes(perf.sglasso1)$y.values[1],
                             attributes(perf.sglasso2)$y.values[1],
                             attributes(perf.sglasso3)$y.values[1],
                             attributes(perf.rf)$y.values[1],
                             attributes(perf.superlearner)$y.values[1]))
)



