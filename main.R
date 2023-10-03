# cleaning workspace in RStudio
rm(list = ls())

# loading utils functions for entire project
source("src/utils.R")

# dataset loading
train <- read.csv("data/training.csv")
test <- read.csv("data/test.csv")

###############################
###### DATA PRE-ANALYSIS ######
###############################

# data preprocessing functions loading
source("src/pre_processing.R")

# data preprocessing packages install and loading
utils$packages_check(pre_processing$packages())

# remove dependent variable from test data and id column ("X") from entire dataset
train <- pre_processing$remove_variables(train, c("X"))
test <- pre_processing$remove_variables(test, c("SeriousDlqin2yrs", "X"))

# check if there are null values in the data
is_null_exists <- pre_processing$check_if_null_values_exists(train)

# compute Information Value for independent variables
iv <- pre_processing$compute_iv(train, "SeriousDlqin2yrs", 10)

# compute Weight of Evidence for all independent variables
woe <- pre_processing$compute_woe_for_all_variables(iv)

# count Gini coefficient for dependent variables
gini <- pre_processing$count_gini_for_all_variables(train, "SeriousDlqin2yrs")

# combine Information Value and Gini coefficient into one data frame
iv_gini <- pre_processing$combine_iv_gini(iv, gini, "gini")

# count correlation between independent variables
correlation <- pre_processing$count_correlation(train)

# drop variables based on IV, Gini coeff and correlation
train <- pre_processing$remove_variables(train, c("NumberOfTime30-59DaysPastDueNotWorse", "NumberOfTime60-89DaysPastDueNotWorse", "DebtRatio", "MonthlyIncome", "NumberOfDependents"))

#################################
###### LOGISTIC REGRESSION ######
#################################

# logistic regression functions loading
source("src/regression.R")

# logistic regression packages install and loading
utils$packages_check(regression$packages())
regression$install_logisticdx()

# train model using logistic regression
logit_model <- regression$train_model(train, "SeriousDlqin2yrs", names(train))

# gof test using Chi-square test
p_value <- regression$chi_square_test(logit_model)

# compute ROC curve
roc <- regression$roc_curve(logit_model, train, "SeriousDlqin2yrs")

# fit model to test data
logit_prediction <- regression$fit_model(logit_model, test)

# compute Gini coefficient to evaluate model
gini_logit <- regression$gini(test, "SeriousDlqin2yrs", logit_prediction)

###############################
######## DECISION TREE ########
###############################

# decision tree functions loading
source("src/decision_tree.R")

# decision tree packages install and loading
utils$packages_check(decision_tree$packages())

# train model using decision tree
tree_model <- decision_tree$train_model(train, "SeriousDlqin2yrs", names(train))

# fit model to test data
tree_prediction <- decision_tree$fit_model(tree_model, test)

# compute Gini coefficient to evaluate model
gini_tree <- decision_tree$gini(test, "SeriousDlqin2yrs", tree_prediction)