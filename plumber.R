# plumber.R

# Load required packages
library(plumber)
library(randomForest)
library(dplyr)
list.files()
# Load trained models
model_rf <- readRDS("model_rf.rds")   # Random Forest model
model_log <- readRDS("model_log.rds") # Logistic Regression model

#* @apiTitle Credit Risk & Default Prediction API

#* Predict loan default using both models
#* @param Age:int
#* @param Income:double
#* @param Loan_Amount:double
#* @param Loan_Term:int
#* @param Employment_Status:string
#* @param Credit_History:int
#* @param Marital_Status:string
#* @param Dependents:int
#* @param Bank_Statement_Score:double
#* @param CIBIL_Score:int
#* @get /predict
function(Age, Income, Loan_Amount, Loan_Term, Employment_Status, Credit_History, Marital_Status, Dependents, Bank_Statement_Score, CIBIL_Score) {
  
  # Create a new input data frame
  new_data <- data.frame(
    Age = as.integer(Age),
    Income = as.numeric(Income),
    Loan_Amount = as.numeric(Loan_Amount),
    Loan_Term = as.integer(Loan_Term),
    Employment_Status = as.factor(Employment_Status),
    Credit_History = as.integer(Credit_History),
    Marital_Status = as.factor(Marital_Status),
    Dependents = as.integer(Dependents),
    Bank_Statement_Score = as.numeric(Bank_Statement_Score),
    CIBIL_Score = as.integer(CIBIL_Score)
  )
  
  # Match factor levels with training data (only needed if models were trained on factors)
  new_data$Employment_Status <- factor(new_data$Employment_Status, levels = levels(model_rf$forest$xlevels$Employment_Status))
  new_data$Marital_Status <- factor(new_data$Marital_Status, levels = levels(model_rf$forest$xlevels$Marital_Status))
  
  # Predictions from both models
  rf_prob <- predict(model_rf, new_data, type = "prob")[,2]
  log_prob <- predict(model_log, new_data, type = "response")
  
  # Recommendation logic
  recommendation <- ifelse(rf_prob > 0.5 | log_prob > 0.5, "High Risk", "Low Risk")
  
  return(list(
    "Random_Forest_Probability" = round(rf_prob, 4),
    "Logistic_Regression_Probability" = round(log_prob, 4),
    "Final_Recommendation" = recommendation
  ))
}
library(plumber)
pr <- plumb("plumber.R")
pr$run(port = 8000)
