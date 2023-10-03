if (!exists("regression")) {
  regression <- new.env()
}

assign("packages", function(){
    # return list of packages required by this module
    return(c(
        ### LogisticDx is no longer available in CRAN
        #"LogisticDx", # logistic regression, gof()
        "devtools", # install_github() in order to download LogisticDx from GitHub
        "pROC" # ROC curve, auc(), roc()
        "ineq" # for Gini coefficient computation
        ))
}, envir = regression)

assign("install_logisticdx", function(){
    # install LogisticDx package from GitHub
    devtools::install_github("cran/LogisticDx")
    library(LogisticDx)
}, envir = regression)

assign("train_model", function(data, dependent_variable, independent_variables){
    # train model - logistic regression
    return(glm(paste(dependent_variable, paste(independent_variables, collapse = " + "), sep = " ~ "), data = data, family = binomial(link = "logit")))
}, envir = regression)

assign("chi_square_test", function(model){
    # gof test using Chi-square test
    return(pchisq(model$null.deviance - model$deviance, df = model$df.null - model$df.residual, lower.tail = FALSE))
}, envir = regression)

assign("roc_curve", function(model, data, dependent_variable){
    # compute ROC curve
    return(pROC::roc(data[[dependent_variable]], predict(model, data, type = "response")))
}, envir = regression)

assign("fit_model", function(model, data){
    # fit model to test data
    return(predict(model, data, type = "response"))
}, envir = regression)

assign("gini", function(data, dependent_variable, prediction){
    # compute Gini coefficient to evaluate model
    return(ineq::Gini(prediction))
}, envir = regression)