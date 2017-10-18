### New Super learner wrappers
### 1. SL.glmnet.T0 : glmnet function to run lasso regression without penalty on the treatments
### 2. SL.glasso : funtion to run group lasso penalized regression
### 3. SL.sparseglasso : function to run sparse group lasso penalized regression
### 4. SL.logistf : SL wrapper to fit logistic regression using Firth's bias reduction method

### other methods included in the function: SL.glm, SL.glmnet, SL.randomforest

### SLwrapperALL : wrapper function to run CV.SuperLearner on externally specified data, group indicators 
### for group lasso and sparse group lasso and penalty.factor for SL.glmnet.T0

##Input:
##  X: matrix of predictors
##  Y: vector of output (currently only handles binary data)
##  method: method of CV.SuperLearner
##  groupIndicators: a vector with group identifiers of the predictors to use for SL.glasso and SL.sparseglasso
##  lambdaseq: sequence of lambda values (in decreasing order) to for SL.sparseglasso
##  penalty.factor: a vector with identifier for the treatments that are not to be penalized in SL.glmnet.T0  


##Output: An object of class CV.SuperLearner

library(SuperLearner)
SLwrapperALL = function(X,Y,method,groupIndicators,
                        lambdaseq,penalty.factor,...)
{
    ###glmnet with no penalization for the treatments
    SL.glmnet.T0 = function(Y=Y,X=X,newX=newX,nfolds=10,
                            family=family,
                            penalty.factor=penanlty.factor,
                            ...)
        {
        if (!is.matrix(X)) {
            X <- model.matrix(~ -1 + ., X)
            newX <- model.matrix(~ -1 + ., newX)
        }
        cv.fit.lasso.up = cv.glmnet(X=X,Y=Y,family=family,
                                standardize=FALSE,
                                penalty.factor=penalty.factor, 
                                nfolds=nfolds)
        pred = predict(cv.fit.lasso.up,newX,s='lambda.min')
        fit = list(object = cv.fit.lasso.up)
        out = list(fit,pred)
        class(out$fit) = 'SL.glmnet'
        return(out)
    }
    
    #########   group lasso penalization
    SL.glasso = function(Y, X, newX, 
                         family=family,
                         groupid = groupIndicators,
                         nfolds=10,lambda=100,
                         loss='logit',...)
    {
        require(gglasso) #package to fit group lasso
        if (family$family == "gaussian") {
            stop("SL.glasso only available for family = binomial()")
        }
        if(length(groupid)!= dim(X)[2]){
            stop("Number of covariates is different than the lenght of groupid")
        }
        if (!is.matrix(X)) {
            X <- model.matrix(~ -1 + ., X)
            newX <- model.matrix(~ -1 + ., newX)
        }
        Y[which(Y==0)] = -1
        fit.CVglasso = gglasso::cv.gglasso(x=X, y=Y, 
                                           group=groupid,
                                           loss=loss,
                                           nfolds=nfolds)
        predict.gglasso <- predict(fit.CVglasso,
                                   newX,
                                   s=fit.CVglasso$lambda.min,
                                   type='link')
        pred = sapply(1:length(predict.gglasso),
                      function(x)1/(1+exp(-predict.gglasso[x])))
        
        fit <- list(object = fit.CVglasso )
        out <- list(pred = pred, fit = fit)
        class(out$fit) <- 'SL.glasso'
        return(out)
    }
    
    ######### sparse group lasso penalization
    SL.sparseglasso = function(Y, X, newX, family, 
                               grouping = groupIndicators,
                               alpha = 0.5,
                               standardize = FALSE,
                               lambda= lambdaseq,
                               ...)
    {  
        require(msgl) #package to fit sparse group lasso
        if (family$family == "gaussian") {
            stop("SL.sparseglasso only available for family = binomial()")
        }
        if(length(grouping)!= dim(X)[2]){
            stop("Number of covariates is different than the lenght of groupid")
        }
        Y[which(Y==0)] = -1
        if (!is.matrix(X)) {
            X <- model.matrix(~ -1 + ., X)
            newX <- model.matrix(~ -1 + ., newX)
        }
        fit.CVspglasso =  msgl::cv(x=X, Y,
                                   sampleWeights = NULL, 
                                   grouping = grouping,
                                   groupWeights = NULL,
                                   parameterWeights = NULL,
                                   alpha = alpha,
                                   standardize = standardize,
                                   lambda=lambda)
        lambda.min = lambda[best_model(fit.CVspglasso)]
        fit.msgl <- msgl::fit(x=X, classes=Y, alpha = alpha,
                              grouping =groupIndicators,
                              lambda = lambda.min,d=1,
                              standardize = FALSE)
        predict.test.msgl = cbind(1,newX)%*%fit.msgl$beta[[1]][2,] 
        pred = sapply(1:length(predict.test.msgl),
                      function(x)1/(1+exp(-predict.test.msgl[x])))
        fit <- list(object = fit.msgl)
        out <- list(pred = pred, fit = fit)
        class(out$fit) <- 'SL.sparseglasso'
        return(out)
    }
    
    ####### logistic regression with firth's adjustment for sparse outcome
    SL.logistf = function(Y, X, newX, ...)
    {
        require(logistf)
        if (!is.matrix(X)) {
            X <- model.matrix(~ -1 + ., X)
            newX <- model.matrix(~ -1 + ., newX)
        }
        dtf = data.frame(Y,X)
        fit = logistf( as.formula(paste("Y~",paste(dimnames(X)[[2]],collapse = '+'))),
                       data = dtf, pl = TRUE, 
                       alpha = 0.05, 
                       firth = TRUE,  
                       plconf = NULL, dataout = TRUE)
        pred.vals = cbind(1,newX)%*%fit$coef 
        pred = sapply(1:length(pred.vals),
                      function(x)1/(1+exp(-pred.vals[x])))
        out = list(pred=pred,fit=fit)
        class(out$fit) = 'SL.logistf'
        return(out)
    }
    
    ##Specify a library of algorithms##
    create.spgl = create.Learner("SL.sparseglasso",
                                 tune = list(alpha=c(0,0.2,0.5,0.75,1)))
    SL.library <- c("SL.glm","SL.mean",
                    "SL.randomForest","SL.glmnet",
                    "SL.logistf","SL.glasso",create.spgl$names)
    
    ##Run the super learner to obtain final predicted values for 
    ##the super learner as well as CV risk for algorithms in the library##
    fit.data.SL<- CV.SuperLearner(Y=data[,6],X=data[,1:5],
                                  SL.library=SL.library, 
                                  family=binomial(),
                                  method= method,
                                  V=5,
                                  cvControl = list(stratifyCV = TRUE),
                                  verbose=TRUE)
    return(fit.data.SL)
}


#Probability of Detection = sum true positive/sum condition positive observed
TPRfunc = function(predictions,trueVal){
    length(which(predictions==1&trueVal==1))/length(trueVal==1)
}

#Probability of False Alarm = 
#   sum false positive/sum condition negative observed
FPRfunc = function(predictions,trueVal){
    length(which(predictions==1&trueVal==0))/length(which(trueVal==0))
} 


MCerrfunc = function(predictions,trueVal){
    1- (length(which(predictions==1&trueVal==1))+
            length(which(predictions==0&trueVal==0)))/length(trueVal)
}







