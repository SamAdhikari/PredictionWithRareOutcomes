##  foldMetric: function to compute evaluation metrics over a range of probability thresholds for each cross validation (CV) fold

##Input: fold := relevant CV fold in numeric, Y.sim := simulated data,
## fit.data.SL := fitted model, thresholds := vector of probability thresholds

#Output: rslt := dataframe with metrics at optimized threshold,
#MCerrlist := List of misclassification error for individual algorithm over the range of thresholds
#TPRlist := List of true positive rate for individual algorithm over the range of thresholds
#PPVlist := List of positive predictive value for individual algorithm over the range of thresholds
#FPRlist := List of false positive rate for individual algorithm over the range of thresholds


foldMetric = function(fold, Y.sim, fit.data.SL, thresholds = seq(0.05,0.8,length=20))
{
  
  folds_id = fit.data.SL$folds[[fold]]
  Y = Y.sim[folds_id]
  #for simulation study
  pr.naive = fit.data.SL$library.predict[folds_id, 1]
  pr.logistic = fit.data.SL$library.predict[folds_id, 2]
  pr.randomforest = fit.data.SL$library.predict[folds_id, 3]
  pr.lasso = fit.data.SL$library.predict[folds_id, 4]
  pr.lassoT0 = fit.data.SL$library.predict[folds_id, 5]
  pr.logistf = fit.data.SL$library.predict[folds_id, 6]
  pr.glasso = fit.data.SL$library.predict[folds_id, 7]
  pr.sglasso1 = fit.data.SL$library.predict[folds_id, 8]
  pr.sglasso2 = fit.data.SL$library.predict[folds_id, 9]
  pr.sglasso3 = fit.data.SL$library.predict[folds_id, 10]
  pr.xgboost = fit.data.SL$library.predict[folds_id, 11]
  pr.nnet1 = fit.data.SL$library.predict[folds_id, 12]
  pr.nnet2 = fit.data.SL$library.predict[folds_id, 13]
  pr.nnet3 = fit.data.SL$library.predict[folds_id, 14]
  pr.ksvm = fit.data.SL$library.predict[folds_id, 15]
  pr.bartMachine = fit.data.SL$library.predict[folds_id, 16]
  pr.SL = fit.data.SL$SL.predict[folds_id]
  
  ##report True positive, False negative, misclassification error and AUC score
  predict.logistic = lapply(1:length(thresholds),function(x)
    1*(pr.logistic > thresholds[x]))
  predict.lasso = lapply(1:length(thresholds),function(x)
    1*(pr.lasso > thresholds[x]))
  predict.lassoT0 = lapply(1:length(thresholds),function(x)
    1*(pr.lassoT0 > thresholds[x]))
  predict.naive = lapply(1:length(thresholds),function(x)
    1*(pr.naive > thresholds[x]))
  predict.glasso = lapply(1:length(thresholds),function(x)
    1*(pr.glasso > thresholds[x]))
  predict.rf = lapply(1:length(thresholds),function(x)
    1*(pr.randomforest > thresholds[x]))
  predict.logistf = lapply(1:length(thresholds),function(x)
    1*(pr.logistf > thresholds[x]))
  predict.sglasso1 = lapply(1:length(thresholds),function(x)
    1*(pr.sglasso1 > thresholds[x]))
  predict.xgboost = lapply(1:length(thresholds),function(x)
    1*(pr.xgboost > thresholds[x]))
  predict.nnet1 = lapply(1:length(thresholds),function(x)
    1*(pr.nnet1 > thresholds[x]))
  predict.nnet2 = lapply(1:length(thresholds),function(x)
    1*(pr.nnet2 > thresholds[x]))
  predict.nnet3 = lapply(1:length(thresholds),function(x)
    1*(pr.nnet3 > thresholds[x]))
  predict.ksvm = lapply(1:length(thresholds),function(x)
    1*(pr.ksvm > thresholds[x]))
  predict.bartMachine = lapply(1:length(thresholds),function(x)
    1*(pr.bartMachine > thresholds[x]))
  predict.SL = lapply(1:length(thresholds),function(x)
    1*(pr.SL > thresholds[x]))
  
  ##MC error
  MCerrNaive = sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.naive[[x]],
              trueVal=Y))
  MCerrlogistic = sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.logistic[[x]],
              trueVal=Y))
  MCerrlasso = sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.lasso[[x]],
              trueVal=Y))
  MCerrlassoT0 = sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.lassoT0[[x]],
              trueVal=Y))
  MCerrglasso = sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.glasso[[x]],
              trueVal=Y))
  MCerrRF = sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.rf[[x]],
              trueVal=Y))
  MCerrlogistf = sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.logistf[[x]],
              trueVal=Y))
  MCerrsglasso1 =sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.sglasso1[[x]],
              trueVal=Y))
  MCerrxgboost =sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.xgboost[[x]],
              trueVal=Y))
  MCerrnnet1 =sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.nnet1[[x]],
              trueVal=Y))
  MCerrnnet2 =sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.nnet2[[x]],
              trueVal=Y))
  MCerrnnet3 =sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.nnet3[[x]],
              trueVal=Y))
  MCerrksvm =sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.ksvm[[x]],
              trueVal=Y))
  MCerrbartMachine =sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.bartMachine[[x]],
              trueVal=Y))
  MCerrSL =sapply(1:length(thresholds),function(x)
    MCerrfunc(predictions=predict.SL[[x]],
              trueVal=Y))
  
  MCerrlist = list(MCerrNaive, MCerrlogistic, MCerrlasso, MCerrlassoT0, 
                   MCerrglasso, MCerrRF, MCerrlogistf, MCerrsglasso1, 
                   MCerrxgboost, MCerrnnet1, MCerrnnet2, MCerrnnet3, 
                   MCerrksvm, MCerrbartMachine,MCerrSL)

  ##TPR 
  TPRnaive = sapply(1:length(thresholds),
                    function(x)TPRfunc(predictions=predict.naive[[x]],
                                       trueVal=Y))
  TPRlogistic = sapply(1:length(thresholds),
                       function(x)TPRfunc(predictions=predict.logistic[[x]],
                                          trueVal = Y))
  TPRlasso = sapply(1:length(thresholds),
                    function(x)TPRfunc(predictions=predict.lasso[[x]],
                                       trueVal=Y))
  TPRlassoT0 = sapply(1:length(thresholds),
                      function(x)TPRfunc(predictions=predict.lassoT0[[x]],
                                         trueVal=Y))
  TPRlogistf = sapply(1:length(thresholds),
                      function(x)TPRfunc(predictions=predict.logistf[[x]],
                                         trueVal=Y))
  TPRrf= sapply(1:length(thresholds),
                function(x)TPRfunc(predictions=predict.rf[[x]],
                                   trueVal=Y))
  TPRglasso= sapply(1:length(thresholds),
                    function(x)TPRfunc(predictions=predict.glasso[[x]],
                                       trueVal=Y))
  TPRsglasso1= sapply(1:length(thresholds),
                      function(x)TPRfunc(predictions=predict.sglasso1[[x]],
                                         trueVal=Y))
  TPRxgboost= sapply(1:length(thresholds),
                     function(x)TPRfunc(predictions=predict.xgboost[[x]],
                                        trueVal=Y))
  
  TPRnnet1= sapply(1:length(thresholds),
                   function(x)TPRfunc(predictions=predict.nnet1[[x]],
                                      trueVal=Y))
  TPRnnet2= sapply(1:length(thresholds),
                   function(x)TPRfunc(predictions=predict.nnet2[[x]],
                                      trueVal=Y))
  
  TPRnnet3= sapply(1:length(thresholds),
                   function(x)TPRfunc(predictions=predict.nnet3[[x]],
                                      trueVal=Y))
  
  TPRksvm = sapply(1:length(thresholds),
                   function(x)TPRfunc(predictions=predict.ksvm[[x]],
                                      trueVal=Y))
  
  TPRbartMachine = sapply(1:length(thresholds),
                          function(x)TPRfunc(predictions=predict.bartMachine[[x]],
                                             trueVal=Y))
  
  TPRSL= sapply(1:length(thresholds),
                function(x)TPRfunc(predictions=predict.SL[[x]],
                                   trueVal=Y))
  
  TPRlist = list(TPRnaive, TPRlogistic, TPRlasso, TPRlassoT0, TPRlogistf, TPRrf, 
                 TPRglasso, TPRxgboost, TPRnnet1, TPRnnet2, TPRnnet3,
                 TPRksvm, TPRbartMachine, TPRSL)
  
  #FPR
  FPRnaive = sapply(1:length(thresholds),
                    function(x)FPRfunc(predictions=predict.naive[[x]],
                                       trueVal=Y))
  FPRlogistic =sapply(1:length(thresholds),function(x)
    FPRfunc(predictions=predict.logistic[[x]],
            trueVal=Y))
  FPRlasso =sapply(1:length(thresholds),function(x)
    FPRfunc(predictions=predict.lasso[[x]],
            trueVal=Y))
  FPRlassoT0 =sapply(1:length(thresholds),function(x)
    FPRfunc(predictions=predict.lassoT0[[x]],
            trueVal=Y))
  FPRlogistf =sapply(1:length(thresholds),function(x)
    FPRfunc(predictions=predict.logistf[[x]],
            trueVal=Y))
  FPRrf =sapply(1:length(thresholds),function(x)
    FPRfunc(predictions=predict.rf[[x]],
            trueVal=Y))
  FPRglasso = sapply(1:length(thresholds),function(x)
    FPRfunc(predictions=predict.glasso[[x]],
            trueVal=Y))
  FPRsglasso1 = sapply(1:length(thresholds),function(x)
    FPRfunc(predictions=predict.sglasso1[[x]],
            trueVal=Y))
  
  FPRxgboost= sapply(1:length(thresholds),
                     function(x)FPRfunc(predictions=predict.xgboost[[x]],
                                        trueVal=Y))
  
  FPRnnet1= sapply(1:length(thresholds),
                   function(x)FPRfunc(predictions=predict.nnet1[[x]],
                                      trueVal=Y))
  
  FPRnnet2= sapply(1:length(thresholds),
                   function(x)FPRfunc(predictions=predict.nnet2[[x]],
                                      trueVal=Y))
  
  FPRnnet3= sapply(1:length(thresholds),
                   function(x)FPRfunc(predictions=predict.nnet3[[x]],
                                      trueVal=Y))
  
  FPRksvm = sapply(1:length(thresholds),
                   function(x)FPRfunc(predictions=predict.ksvm[[x]],
                                      trueVal=Y))
  
  FPRbartMachine = sapply(1:length(thresholds),
                          function(x)FPRfunc(predictions=predict.bartMachine[[x]],
                                             trueVal=Y))
  
  FPRSL = sapply(1:length(thresholds),function(x)
    FPRfunc(predictions=predict.SL[[x]],
            trueVal=Y))
  
  FPRlist = list(FPRnaive, FPRlogistic, FPRlasso, FPRlassoT0, FPRlogistf, 
                 FPRrf, FPRglasso, FPRsglasso1, FPRxgboost,FPRnnet1, FPRnnet2, 
                 FPRnnet3, FPRksvm, FPRbartMachine )
  
  ##Positive predictive value PPV
  PPVnaive = sapply(1:length(thresholds),function(x)
    PPVfunc(predictions=predict.naive[[x]],
            trueVal=Y))
  PPVlogistic = sapply(1:length(thresholds),function(x)
    PPVfunc(predictions=predict.logistic[[x]],
            trueVal=Y))
  PPVlasso = sapply(1:length(thresholds),function(x)
    PPVfunc(predictions=predict.lasso[[x]],
            trueVal=Y))
  PPVglasso = sapply(1:length(thresholds),function(x)
    PPVfunc(predictions=predict.glasso[[x]],
            trueVal=Y))
  PPVlassoT0 = sapply(1:length(thresholds),function(x)
    PPVfunc(predictions=predict.lassoT0[[x]],
            trueVal=Y))
  PPVlogistf = sapply(1:length(thresholds),function(x)
    PPVfunc(predictions=predict.logistf[[x]],
            trueVal=Y))
  PPVrf = sapply(1:length(thresholds),function(x)
    PPVfunc(predictions=predict.rf[[x]],
            trueVal=Y))
  PPVsglasso1 = sapply(1:length(thresholds),function(x)
    PPVfunc(predictions=predict.sglasso1[[x]],
            trueVal=Y))
  
  PPVxgboost= sapply(1:length(thresholds),
                     function(x)PPVfunc(predictions=predict.xgboost[[x]],
                                        trueVal=Y))
  
  PPVnnet1= sapply(1:length(thresholds),
                   function(x)PPVfunc(predictions=predict.nnet1[[x]],
                                      trueVal=Y))
  
  PPVnnet2= sapply(1:length(thresholds),
                   function(x)PPVfunc(predictions=predict.nnet2[[x]],
                                      trueVal=Y))
  
  PPVnnet3= sapply(1:length(thresholds),
                   function(x)PPVfunc(predictions=predict.nnet3[[x]],
                                      trueVal=Y))
  
  PPVksvm = sapply(1:length(thresholds),
                   function(x)PPVfunc(predictions=predict.ksvm[[x]],
                                      trueVal=Y))
  
  PPVbartMachine = sapply(1:length(thresholds),
                          function(x)PPVfunc(predictions=predict.bartMachine[[x]],
                                             trueVal=Y))
  
  PPVSL = sapply(1:length(thresholds),function(x)
    PPVfunc(predictions=predict.SL[[x]],
            trueVal=Y))
  
  PPVlist = list(PPVnaive, PPVlogistic, PPVlasso, PPVglasso, PPVlassoT0, PPVlogistf,
                 PPVrf, PPVsglasso1, PPVxgboost, PPVnnet1, PPVnnet2, PPVnnet3, PPVksvm, 
                 PPVbartMachine)
  
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
  pred.lasso = prediction(pr.lasso ,Y)
  roc.perf.lasso = performance(pred.lasso,
                               measure = 'tpr', x.measure = 'fpr')
  pred.lassoT0 = prediction(pr.lassoT0 ,Y)
  roc.perf.lassoT0 = performance(pred.lassoT0,
                                 measure = 'tpr', x.measure = 'fpr')
  pred.rf = prediction(pr.randomforest ,Y)
  roc.perf.rf = performance(pred.rf,
                            measure = 'tpr', x.measure = 'fpr')
  pred.glasso = prediction(pr.glasso, Y)
  roc.perf.glasso = performance(pred.glasso,
                                measure='tpr',x.measure = 'fpr')
  pred.sglasso1 = prediction(pr.sglasso1, Y)
  roc.perf.sglasso1 = performance(pred.sglasso1,
                                  measure='tpr',x.measure = 'fpr')
  
  pred.xgboost= prediction(pr.xgboost, Y)
  roc.perf.xgboost = performance(pred.xgboost,
                                 measure='tpr',x.measure = 'fpr')
  
  pred.nnet1 = prediction(pr.nnet1, Y)
  roc.perf.nnet1 = performance(pred.nnet1,
                               measure='tpr',x.measure = 'fpr')
  
  pred.nnet2 = prediction(pr.nnet2, Y)
  roc.perf.nnet2 = performance(pred.nnet2,
                               measure='tpr',x.measure = 'fpr')
  
  pred.nnet3 = prediction(pr.nnet3, Y)
  roc.perf.nnet3 = performance(pred.nnet3,
                               measure='tpr',x.measure = 'fpr')
  
  pred.ksvm = prediction(pr.ksvm, Y)
  roc.perf.ksvm = performance(pred.ksvm,
                              measure='tpr',x.measure = 'fpr')
  
  pred.bartMachine = prediction(pr.bartMachine, Y)
  roc.perf.bartMachine = performance(pred.bartMachine,
                                     measure='tpr',x.measure = 'fpr')
  pred.superlearner = prediction(pr.SL, Y)
  roc.perf.superlearner = performance(pred.superlearner,
                                      measure='tpr',x.measure = 'fpr')
  
  perf.naive = performance(pred.naive,measure='auc')
  perf.logistic =  performance(pred.logistic,measure = 'auc')
  perf.logistf = performance(pred.logistf,measure = 'auc')
  perf.lasso = performance(pred.lasso,measure='auc')
  perf.lassoT0 = performance(pred.lassoT0,measure='auc')
  perf.rf = performance(pred.rf,measure='auc')
  perf.glasso = performance(pred.glasso,measure='auc')
  perf.sglasso1 = performance(pred.sglasso1,measure='auc')
  perf.nnet1 = performance(pred.nnet1,measure='auc')
  perf.nnet2 = performance(pred.nnet2,measure='auc')
  perf.nnet3 = performance(pred.nnet3,measure='auc')
  perf.xgboost = performance(pred.xgboost,measure='auc')
  perf.ksvm = performance(pred.ksvm,measure='auc')
  perf.bartMachine = performance(pred.bartMachine,measure='auc')
  perf.superlearner = performance(pred.superlearner,measure='auc')
  
  
  # 
  lasso_id = which((1-MCerrlasso)==max((1-MCerrlasso)))[1]
  lassoT0_id = which((1-MCerrlassoT0)==max((1-MCerrlassoT0)))[1]
  logit_id = which((1-MCerrlogistic)==max((1-MCerrlogistic)))[1]
  glasso_id = which((1-MCerrglasso)==max((1-MCerrglasso)))[1]
  RF_id = which((1-MCerrRF)==max((1-MCerrRF)))[1]
  SL_id = which((1-MCerrSL)==max((1-MCerrSL)))[1]
  logistf_id = which((1-MCerrlogistf)==max((1-MCerrlogistf)))[1]
  sglasso1_id=which((1-MCerrsglasso1)==max((1-MCerrsglasso1)))[1]
  nnet1_id=which((1-MCerrnnet1)==max((1-MCerrnnet1)))[1]
  xgboost_id =which((1-MCerrxgboost)==max((1-MCerrxgboost)))[1]
  ksvm_id =which((1-MCerrksvm)==max((1-MCerrksvm)))[1]
  bartMachine_id =which((1-MCerrbartMachine)==max((1-MCerrbartMachine)))[1]
  
  ##sensitivity = TPR
  TPR_lasso = TPRlasso[lasso_id]
  TPR_logistic = TPRlogistic[logit_id]
  TPR_glasso = TPRglasso[glasso_id]
  TPR_SL = TPRSL[SL_id]
  TPR_RF = TPRrf[RF_id]
  TPR_lassoT0 = TPRlassoT0[lassoT0_id]
  TPR_logistf = TPRlogistf[logistf_id]
  TPR_sglasso1 = TPRsglasso1[sglasso1_id]
  TPR_nnet1 = TPRnnet1[nnet1_id]
  TPR_xgboost = TPRxgboost[xgboost_id]
  TPR_ksvm = TPRksvm[ksvm_id]
  TPR_bartMachine = TPRbartMachine[bartMachine_id]
  
  
  ##specificity = 1- FPR
  FPR_lasso = FPRlasso[lasso_id]
  FPR_logistic = FPRlogistic[logit_id]
  FPR_glasso = FPRglasso[glasso_id]
  FPR_SL = FPRSL[SL_id]
  FPR_RF = FPRrf[RF_id]
  FPR_lassoT0 = FPRlassoT0[lassoT0_id]
  FPR_logistf = FPRlogistf[logistf_id]
  FPR_sglasso1 = FPRsglasso1[sglasso1_id]
  FPR_nnet1 = FPRnnet1[nnet1_id]
  FPR_xgboost = FPRxgboost[xgboost_id]
  FPR_ksvm = FPRksvm[ksvm_id]
  FPR_bartMachine = FPRbartMachine[bartMachine_id]
  
  #positive prediction value
  PPV_lasso = PPVlasso[lasso_id]
  PPV_logistic = PPVlogistic[logit_id]
  PPV_glasso = PPVglasso[glasso_id]
  PPV_SL = PPVSL[SL_id]
  PPV_RF = PPVrf[RF_id]
  PPV_lassoT0 = PPVlassoT0[lassoT0_id]
  PPV_logistf = PPVlogistf[logistf_id]
  PPV_sglasso1 = PPVsglasso1[sglasso1_id]
  PPV_nnet1 = PPVnnet1[nnet1_id]
  PPV_xgboost = PPVxgboost[xgboost_id]
  PPV_ksvm = PPVksvm[ksvm_id]
  PPV_bartMachine = PPVbartMachine[bartMachine_id]
  
  ##accuracy
  accuracy_lasso = 1-MCerrlasso[lasso_id]
  accuracy_logistic = 1-MCerrlogistic[logit_id]
  accuracy_glasso = 1-MCerrglasso[glasso_id]
  accuracy_SL = 1-MCerrSL[SL_id]
  accuracy_RF = 1-MCerrRF[RF_id]
  accuracy_lassoT0 = 1-MCerrlassoT0[lassoT0_id]
  accuracy_logistf = 1-MCerrlogistf[logistf_id]
  accuracy_sglasso1 = 1-MCerrsglasso1[sglasso1_id]
  accuracy_nnet1 = 1 - MCerrnnet1[nnet1_id]
  accuracy_xgboost = 1 - MCerrxgboost[xgboost_id]
  accuracy_ksvm = 1 - MCerrksvm[ksvm_id]
  accuracy_bartMachine = 1 - MCerrbartMachine[bartMachine_id]
  
  
  rslt = data.frame('Method'=c('Logistic','LogistF','Lasso',
                               'LassoT0','Glasso','Sglasso1',
                               'nnet1','xgboost','ksvm', 'bartMachine',
                               'RandomForest','SuperLearner'),
                    'TPR' = c(TPR_logistic,TPR_logistf,TPR_lasso,TPR_lassoT0,
                              TPR_glasso,TPR_sglasso1,TPR_nnet1,TPR_xgboost,
                              TPR_ksvm, TPR_bartMachine,
                              TPR_RF,TPR_SL),
                    'FPR' = c(FPR_logistic,FPR_logistf,FPR_lasso,FPR_lassoT0,
                              FPR_glasso,FPR_sglasso1,FPR_nnet1,FPR_xgboost,
                              FPR_ksvm, FPR_bartMachine,
                              FPR_RF,FPR_SL),
                    'PPV' = c(PPV_logistic,PPV_logistf,PPV_lasso,PPV_lassoT0,
                              PPV_glasso,PPV_sglasso1,PPV_nnet1,PPV_xgboost,
                              PPV_ksvm, PPV_bartMachine,
                              PPV_RF,PPV_SL),
                    'Accuracy' = c(accuracy_logistic,accuracy_logistf,accuracy_lasso,accuracy_lassoT0,
                                   accuracy_glasso,accuracy_sglasso1,accuracy_nnet1,accuracy_xgboost,
                                   accuracy_ksvm, accuracy_bartMachine,
                                   accuracy_RF,accuracy_SL),
                    'AUC' =  unlist(c(attributes(perf.logistic)$y.values[1],
                                      attributes(perf.logistf)$y.values[1],
                                      attributes(perf.lasso)$y.values[1],
                                      attributes(perf.lassoT0)$y.values[1],
                                      attributes(perf.glasso)$y.values[1],
                                      attributes(perf.sglasso1)$y.values[1],
                                      attributes(perf.nnet1)$y.values[1],
                                      attributes(perf.xgboost)$y.values[1],
                                      attributes(perf.ksvm)$y.values[1],
                                      attributes(perf.bartMachine)$y.values[1],
                                      attributes(perf.rf)$y.values[1],
                                      attributes(perf.superlearner)$y.values[1]))
  )
  
  rslt$Fscore = 1/(1/rslt$TPR + 1/rslt$PPV)
  
  return(list(rslt = rslt, MCerrlist = MCerrlist, TPRlist = TPRlist, PPVlist = PPVlist, FPRlist = FPRlist))
}

