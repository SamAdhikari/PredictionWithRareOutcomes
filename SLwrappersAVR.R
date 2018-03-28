##I am using this function fromt the package SuperLearner
##for some reason R in linux is not recognizing this function in the learner
create.Learner =
function (base_learner, params = list(), tune = list(), env = parent.frame(), 
          name_prefix = base_learner, detailed_names = F, verbose = F) 
{
    if (length(tune) > 0) {
        tuneGrid = expand.grid(tune, stringsAsFactors = FALSE)
        names = rep("", nrow(tuneGrid))
        max_runs = nrow(tuneGrid)
    }
    else {
        max_runs = 1
        tuneGrid = NULL
        names = c()
    }
    for (i in seq(max_runs)) {
        name = paste(name_prefix, i, sep = "_")
        if (length(tune) > 0) {
            g = tuneGrid[i, , drop = F]
            if (detailed_names) {
                name = do.call(paste, c(list(name_prefix), g, 
                                        list(sep = "_")))
            }
        }
        else {
            g = c()
        }
        names[i] = name
        fn_params = ""
        all_params = c(as.list(g), params)
        for (name_i in names(all_params)) {
            val = all_params[[name_i]]
            if (!is.null(val) && val != "NULL") {
                if (class(val) == "character") {
                    val = paste0("\"", val, "\"")
                }
                fn_params = paste0(fn_params, ", ", name_i, "=", 
                                   val)
            }
        }
        fn = paste0(name, " <- function(...) ", base_learner, 
                    "(...", fn_params, ")")
        if (verbose) {
            cat(fn, "\n")
        }
        eval(parse(text = fn), envir = env)
    }
    results = list(grid = tuneGrid, names = names, base_learner = base_learner, 
                   params = params)
    invisible(results)
}



##glmnet without penalty on the treatments
SL.glmnetT0 <- function(Y, X, newX, family, obsWeights, id,
                                     alpha = 1, nfolds = 10, nlambda = 100, useMin = TRUE,
                                     loss = "deviance",
                                    standardize = FALSE,
                                     ...) 
    {
    require('glmnet')
    if (!is.matrix(X)) {
        X <- model.matrix(~ -1 + ., X)
        newX <- model.matrix(~ -1 + ., newX)
    }
    p.factor = rep(1,dim(X)[2])
    p.factor[grep('Valve_G',dimnames(X)[[2]])] = 0 #no penalty on the valves
    # Use CV to find optimal lambda.
    fitCV <- glmnet::cv.glmnet(x = X, y = Y, intercept=FALSE,
                               type.measure = loss,
                               nfolds = nfolds,
                               family = family$family,
                               nlambda = nlambda,
                               penalty.factor=p.factor,
                               standardize = standardize,
                               ...)
    # If we predict with the cv.glmnet object we can specify lambda using a
    # string.
    pred <- predict(fitCV, newx = newX, type = "response",
                    s = ifelse(useMin, "lambda.min", "lambda.1se"))
    fit <- list(object = fitCV, useMin = useMin)
    class(fit) <- "SL.glmnet"
    out <- list(pred = pred, fit = fit)
    return(out)
}

#########   group lasso penalization
SL.glasso = function(Y, X, newX, 
                         family,
                         groupid=1:dim(X)[2],
                         nfolds=10,
                        ...)
{
        require(gglasso) #package to fit group lasso
        if(family$family == "gaussian") {
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
                                           pred.loss='loss',
                                           nfolds=nfolds)
        predict.gglasso <- predict(fit.CVglasso,
                                   newX,
                                   s="lambda.min",
                                   type='link')
        pred = sapply(1:length(predict.gglasso),
                      function(x)1/(1+exp(-predict.gglasso[x])))
        fit <- list(object = fit.CVglasso)
        out <- list(pred = pred, fit = fit)
        class(out$fit) <- 'SL.glasso'
        return(out)
}

    
predict.SL.glasso = function(object,newdata,...){
    predict.glasso = predict(object,
               newdata,
               s="lambda.min",
               type='link')
    pred = sapply(1:length(predict.gglasso),
              function(x)1/(1+exp(-predict.gglasso[x])))
    return(pred)
    }    
    
    

SL.sparseglasso = function(Y, X, newX, family,
                               alpha = 0.5,
                               standardize = FALSE,
                                grouping = 1:dim(X)[2],
                               ...)
    {
        require(msgl) #package to fit sparse group lasso
     #   grouping = getGroupsFoldsAVR(TT,X,Y)$groupIndicators
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
        lambda.use = lambda(x=X, classes=Y,lambda.min=0.001,alpha = alpha,
                            standardize = standardize,d=50)
        fit.cv = msgl::cv(x=X, classes=Y,lambda= lambda.use,
                          sampleWeights = NULL,
                          grouping = grouping,
                          groupWeights = NULL,
                          parameterWeights = NULL,
                          alpha = alpha,
                          standardize = standardize)

        model.fit = msgl::fit(x=X, classes=Y,lambda= lambda.use,
                              sampleWeights = NULL,
                              grouping = grouping,
                              groupWeights = NULL,
                              parameterWeights = NULL,
                              alpha = alpha,
                              standardize = standardize)
        
        #prediction in test data using default prediction in msgl
        predict.test = predict(model.fit,x=newX)
        loglikeDev = Err(fit.cv,type='loglike')
        lambda.min = which(loglikeDev==min(loglikeDev))[1]
        pred = predict.test$response[[lambda.min]][2,]
        fit <- list(object = model.fit,lam=lambda.min)
        out <- list(pred = pred, fit = fit)
        class(out$fit) <- 'SL.sparseglasso'
        return(out)
    }

predict.SL.sparseglasso =  function(object,newdata,lam,...){
    predict.test = predict(object,x=newdata)
    pred = predict.test$response[[lam]][2,]
    return(pred)
}
    
####### logistic regression with firth's adjustment for sparse outcome
SL.logistf = function(Y, X, newX,  ...)
    {
        require(logistf)
        if (!is.matrix(X)) {
            X <- model.matrix(~ -1 + ., X)
            newX <- model.matrix(~ -1 + ., newX)
        }
        dtf = data.frame(Y,X)
        fit = logistf(as.formula(paste("Y~",
                        paste(dimnames(X)[[2]],collapse = '+'))),
                       data = dtf, pl = TRUE, 
                       firth = TRUE,  
                       plconf = NULL, dataout = TRUE)
        pred.vals = cbind(1,newX)%*%fit$coef 
        pred = sapply(1:length(pred.vals),
                      function(x)1/(1+exp(-pred.vals[x])))
        out = list(pred=pred,fit=fit)
        class(out$fit) = 'SL.logistf'
        return(out)
}

predict.SL.logistf = function(object,newdata,...){
    if (!is.matrix(newdata)) {
        newdata <- model.matrix(~ -1 + ., newdata)
    }
    pred.vals = cbind(1,newdata)%*%object$coef 
    pred = sapply(1:length(pred.vals),
                  function(x)1/(1+exp(-pred.vals[x])))
    return(pred)
}
    
##Evalution metrics
##
#Probability of Detection = sum true positive/sum condition positive observed
TPRfunc = function(predictions,trueVal){
    length(which(predictions==1&trueVal==1))/length(which(trueVal==1))
}

#Probability of False Alarm = 
#   sum false positive/sum condition negative observed
FPRfunc = function(predictions,trueVal){
    length(which(predictions==1&trueVal==0))/length(which(trueVal==0))
} 

#positive predictive value
# sum true positive / (sum true positive + sum false positives)
PPVfunc = function(predictions,trueVal){
    length(which(predictions==1&trueVal==1))/
        (length(which(predictions==1&trueVal==1))+
             length(which(predictions==1&trueVal==0)))
}

##misclassification error
MCerrfunc = function(predictions,trueVal){
    1- (length(which(predictions==1&trueVal==1))+
            length(which(predictions==0&trueVal==0)))/length(trueVal)
}







