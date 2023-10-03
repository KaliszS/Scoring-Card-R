if (!exists("decision_tree")) {
  decision_tree <- new.env()
}

assign("packages", function(){
    # return list of packages required by this module
    return(c(
        "smbinning", # decision trees?
        "rpart", # decision trees
        "ineq" # for Gini coefficient computation
        ))
}, envir = decision_tree)

assign("train_model", function(data, dependent_variable, independent_variables){
    # train model - decision tree
    return(rpart::rpart(paste(dependent_variable, paste(independent_variables, collapse = " + "), sep = " ~ "), data = data, method = "class"))
}, envir = decision_tree)

assign("fit_model", function(model, data){
    # fit model to test data
    return(predict(model, data, type = "class"))
}, envir = decision_tree)

assign("gini", function(data, dependent_variable, prediction){
    # compute Gini coefficient to evaluate model
    return(ineq::Gini(prediction))
}, envir = decision_tree)