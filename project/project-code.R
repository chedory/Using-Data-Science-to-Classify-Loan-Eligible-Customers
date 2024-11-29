#org_data <- read.csv("C://Users//osama//OneDrive//Desktop//university//lvl 7//ISY351-project//project//datasets//train_u6lujuX_CVtuZ9i (1).csv")
org_data <- read.csv("C:/Users/brooo/OneDrive/سطح المكتب/IS/مستوى السابع/ISY351/Project/ISY351PROJECT_DATASET.csv")
org_data

############################
# Understanding the Data
############################


str(org_data)
summary(org_data)

#to see the first and last 6 rows of the data
head(org_data)
tail(org_data)

#to know the numbers of rows and col
dim(org_data)

# to know the col names
colnames(org_data)


############################
#    data prepare
############################

data <- org_data #org_data = original data , data is the new data

data$Married <- ifelse(data$Married == "Yes", 1, ifelse(data$Married == "No", 0, NA))
data$Gender <- ifelse(data$Gender == "Male", 1, ifelse(data$Gender == "Female", 0, NA))
data$Education <- ifelse(data$Education == "Graduate", 1, ifelse(data$Education == "Not Graduate", 0, NA))
data$Self_Employed <- ifelse(data$Self_Employed == "Yes", 1, ifelse(data$Self_Employed == "No", 0, NA))
data$Loan_Status <- ifelse(data$Loan_Status == "Y", 1, ifelse(data$Loan_Status == "N", 0, NA))
data$Dependents <- ifelse(data$Dependents == "3+", 4, data$Dependents)  # Replace '3+' with 4
data$Dependents <- as.numeric(data$Dependents)

View(org_data)
View(data)

str(data)
summary(data)

#to see the first and last 6 rows of the data
head(data)
tail(data)

#to know the numbers of rows and col
dim(data)

# to know the col names
colnames(data)


###################################
# Exploratory Data Analysis (EDA)
###################################
library(ggplot2)

par(mfrow = c(3, 2))

# Barplot for Self_Employed
Self_Employed_counts <- table(data$Self_Employed)
barplot(Self_Employed_counts, main = "Self_Employed Count", xlab = "Self_Employed", col = c("red", "blue"), names.arg = c("Not Employed", "Employed"))

# Barplot for Gender
gender_counts <- table(data$Gender)  # Count occurrences of each category
barplot(gender_counts, main = "Gender Count", xlab = "Gender",ylim = c(0,500), col = c("lightpink", "lightblue"), names.arg = c("Female", "Male"))

# Barplot for Married
married_counts <- table(data$Married)  # Count occurrences of each category
barplot(married_counts, main = "Marital Status Count", xlab = "Marital Status",ylim = c(0,500), col = c("red", "blue"), names.arg = c("Not married", "married"))

# Histogram for LoanAmount (numerical variable)
hist(data$LoanAmount, breaks = 10, main = "Loan Amount Distribution",ylim = c(0,300), xlab = "Loan Amount", col = "green", border = "black")

# Barplot for Education
education_counts <- table(data$Education)
barplot(education_counts, main = "Education Level Count", xlab = "Education Level", col = c("orange", "purple"), names.arg = c("Not Graduate", "Graduate"), ylim = c(0,500))

# Barplot for Loan Status
Loan_Status_Counts <- table(data$Loan_Status)
barplot(Loan_Status_Counts, main = "Loan_Status Count", xlab = "Loan Status",ylim = c(0,500), col = c("darkred", "darkblue"), names.arg = c("Not Accepted", "Accepted"))


###################################
#   checking missing values
###################################

#there messing value or not
is.na(data)
is.na(org_data)

#numbers of missing values for each columns
colSums(is.na(data))
colSums(is.na(org_data))

#numbers of missing values
sum(is.na(data))
sum(is.na(org_data))

##################################
#    Handling missing values
##################################

# Handling missing values if it's numric and not 1or0 type:
columns_mv <- c("ApplicantIncome", "CoapplicantIncome", "LoanAmount", "Loan_Amount_Term")

for (col in columns_mv) {
  # Replace NA with mean
  data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
}

#delete NA row if it's 1 or 0:
data <- na.omit(data) 

nrow(data) #check how many row after deal with messing value

sum(is.na(data))
sum(is.na(org_data))

View(data)

########################################################################################
###############################   modeling  ############################################
########################################################################################
library(caret) #library to split , train , model evaluation ,etc..

set.seed(100)  # For reproducibility

# Split data into training and testing (80% train, 20% test)
trainIndex <- createDataPartition(data$Loan_Status, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ] #train 80%
testData <- data[-trainIndex, ] #test 20%

# Remove the Loan_ID column from both training and test data
trainData <- trainData[, -which(names(trainData) == "Loan_ID")]
testData <- testData[, -which(names(testData) == "Loan_ID")]


#model 1 - Logistic Regression
# training model
model_logistic <- glm(Loan_Status ~ ., data = trainData, family = "binomial")

# Summary of the model
summary(model_logistic)

# Make predictions on test set
pred_logistic <- predict(model_logistic, newdata = testData, type = "response")

# Convert probabilities to binary predictions
pred_logistic_class <- ifelse(pred_logistic > 0.5, 1, 0)

# Plot ROC Curve for Logistic Regression
library(pROC)
roc_logistic <- roc(testData$Loan_Status, pred_logistic) 
plot(roc_logistic, main = "ROC Curve for Logistic Regression", col = "blue", lwd = 2)

# Add AUC
cat("AUC for Logistic Regression:", auc(roc_logistic), "\n")


#model2 - desicion tree
# Load library for decision trees
library(rpart)
library(rpart.plot)

# Train decision tree
model_tree <- rpart(Loan_Status ~ ., data = trainData, method = "class")

# Visualize decision tree
rpart.plot(model_tree)

# Make predictions on test set
pred_tree <- predict(model_tree, newdata = testData, type = "class")



########################################################################################
###############################   Evaluate  ############################################
########################################################################################

# Evaluate Logistic Regression
conf_matrix_logistic <- confusionMatrix(as.factor(pred_logistic_class), as.factor(testData$Loan_Status))
print(conf_matrix_logistic)

# Evaluate Decision Tree
conf_matrix_tree <- confusionMatrix(as.factor(pred_tree), as.factor(testData$Loan_Status))
print(conf_matrix_tree)

# Compare Accuracy
cat("Logistic Regression Accuracy:", conf_matrix_logistic$overall["Accuracy"], "\n")
cat("Decision Tree Accuracy:", conf_matrix_tree$overall["Accuracy"], "\n")





