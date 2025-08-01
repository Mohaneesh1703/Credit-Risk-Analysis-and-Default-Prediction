install.packages(c("dplyr", "DBI", "RMySQL", "ggplot2"))
install.packages("randomForest")
library(randomForest)
library(dplyr)
library(DBI)
library(RMySQL)
library(ggplot2)
data <- read.csv(file.choose())
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "credit_risk_db",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Mo#@neesh1703",
                 client.flag = CLIENT_LOCAL_FILES)
dbWriteTable(con, "credit_data", data, overwrite = TRUE, row.names = FALSE)
print(dbGetQuery(con, "SELECT COUNT(*) FROM credit_data"))
# Average CIBIL score for defaulters
print(dbGetQuery(con, "SELECT AVG(CIBIL_Score) FROM credit_data WHERE `Default` = 1"))
# Employment status distribution
print(dbGetQuery(con, "
    SELECT Employment_Status, COUNT(*) as Count
    FROM credit_data
    GROUP BY Employment_Status
"))

# 🧹 5. Data Preprocessing for Modeling
df <- dbReadTable(con, "credit_data")

# Convert categorical columns to factors
cols_to_factor <- c("Default", "Marital_Status", "Employment_Status")
df[cols_to_factor] <- lapply(df[cols_to_factor], as.factor)
# 🧪 6. Split Data into Train/Test
set.seed(123)
train_index<-sample(1:nrow(df),0.8*nrow(df))
train<-df[train_index,]
test<-df[-train_index,]
model2<-randomForest(Default ~., train,ntree=100,mtry=2)
# 🧮 8. Predict and Evaluate
# For random forest
pred_probs2 <- predict(model2, newdata = test, type = "response")
actual2=test$Default
conf_matrix2 <- table(pred_probs2,actual2)
print(conf_matrix2)
accuracy=sum(pred_probs2==actual2)/length(actual2)
print(paste('accuracy: ',accuracy))
# 📊 9. Visualize CIBIL Score Distribution
ggplot(df, aes(x = CIBIL_Score, fill = Default)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  theme_minimal() +
  labs(title = "CIBIL Score Distribution by Default Status")

# 🧹 10. Close MySQL Connection
dbDisconnect(con)
# Save the trained models
saveRDS(model2, "model_rf.rds")
# -------------------------------------------
# 📊 Tableau Dashboard Objectives:
# -------------------------------------------
# 1. Identify High-Risk Customer Segments
# 2. Analyze the Impact of CIBIL Score on Loan Default
# 3. Explore Loan Distribution Across Demographics
# 4. Monitor Credit Health of the Portfolio