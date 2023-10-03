if (!exists("pre_processing")) {
  pre_processing <- new.env()
}

assign("packages", function(){
    # return list of packages required by this module
    return(c(
        "Information", # for IV and WOE computation
        "ineq" # for Gini coefficient computation
        ))
}, envir = pre_processing)

assign("remove_variables", function(data, variables){
    # remove variable columns from data
    data <- data[, !(names(data) %in% variables)]
    return(data)
}, envir = pre_processing)

assign("check_if_null_values_exists", function(data){
    # check if there are null values in the data
    if (sum(is.na(data)) > 0){
        return(TRUE)
    }
    return(FALSE)
}, envir = pre_processing)

assign("compute_iv", function(data, dependent_variable, bins){
    # compute IV for independent variables
    return(Information::create_infotables(data, y=dependent_variable, bins=bins, parallel=TRUE))
}, envir = pre_processing)

assign("compute_woe", function(iv, variable){
    # compute woe for one independent variable
    return(iv$Tables[[variable]])
}, envir = pre_processing)

assign("compute_woe_for_all_variables", function(iv){
    # compute woe for all independent variables
    variables <- names(iv$Tables)
    woe <- list()
    for (variable in variables){
        woe[[variable]] <- pre_processing$compute_woe(iv, variable)
    }
    return(woe)
}, envir = pre_processing)

assign("count_gini", function(data, independent_variable){
    # count gini coefficient for independent variable
    return(ineq::Gini(data[[independent_variable]]))
}, envir = pre_processing)

assign("count_gini_for_all_variables", function(data, dependent_variable){
    # count gini coefficient for all independent variables
    variables <- names(data)
    # exclude dependent variable
    variables <- variables[!(variables %in% dependent_variable)]
    gini <- list()
    for (variable in variables){
        gini[[variable]] <- pre_processing$count_gini(data, variable)
    }
    return(gini)
}, envir = pre_processing)

assign("combine_iv_gini", function(iv, gini, sort_by, desc=TRUE){
    # combine IV and Gini coefficient into one data frame
    variables <- names(iv$Tables)
    iv_woe_gini <- data.frame(variable=variables, iv=NA, gini=NA)
    for (variable in variables){
        iv_woe_gini[iv_woe_gini$variable == variable, "iv"] <- round(iv$Tables[[variable]]$IV[1], 4)
        iv_woe_gini[iv_woe_gini$variable == variable, "gini"] <- round(gini[[variable]], 2)
    }
    # sort by iv or gini and desc or asc
    iv_woe_gini <- iv_woe_gini[order(iv_woe_gini[[sort_by]], decreasing=desc), ]
    return(iv_woe_gini)
}, envir = pre_processing)

assign("count_correlation", function(data){
    # count correlation between independent variables
    return(round(cor(data), 4))
}, envir = pre_processing)