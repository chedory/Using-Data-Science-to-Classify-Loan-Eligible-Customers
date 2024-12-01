#org_data <- read.csv("C://Users//osama//OneDrive//Desktop//university//lvl 7//ISY351-project//project//datasets//Loan Prediction.csv")
org_data <- read.csv("C:/Users/brooo/OneDrive/سطح المكتب/IS/مستوى السابع/ISY351/Project/Loan Prediction.csv")
org_data


############################
# Understanding the Data
############################

str(org_data) #type of column
summary(org_data) #summary of each column (mean, min ,max , etc..)


#to see the first and last 6 rows of the data
head(org_data) #first 6
tail(org_data) #last 6

#to know the numbers of rows and col
dim(org_data)

# to know the col names
colnames(org_data)


############################
#    data prepare
############################

data <- org_data #org_data = original data , data = data we work on

# Convert categorical columns to numeric (1 and 0)
data$Married <- ifelse(data$Married == "Yes", 1, ifelse(data$Married == "No", 0, NA)) # Replace "Yes" with 1 and "No"with 0
data$Gender <- ifelse(data$Gender == "Male", 1, ifelse(data$Gender == "Female", 0, NA)) # Replace "Male" with 1 and "Female" with 0
data$Education <- ifelse(data$Education == "Graduate", 1, ifelse(data$Education == "Not Graduate", 0, NA))  # Replace "Graduate" with 1 and "Not Graduate" with 0
data$Self_Employed <- ifelse(data$Self_Employed == "Yes", 1, ifelse(data$Self_Employed == "No", 0, NA))   # Replace "Yes" with 1 and "No"with 0
data$Loan_Status <- ifelse(data$Loan_Status == "Y", 1, ifelse(data$Loan_Status == "N", 0, NA))   # Replace "Y" with 1 and "N"with 0
data$Dependents <- ifelse(data$Dependents == "3+", 4, data$Dependents)  # Replace '3+' with 4
data$Dependents <- as.numeric(data$Dependents) #convert to numeric

View(org_data) #view original data
View(data)  #view data

str(data) #type of column
summary(data) #summary of each column (mean, min ,max , etc..)

#to see the first and last 6 rows of the data
head(data)
tail(data)

#to know the numbers of rows and col
dim(data)

# to know the col names
colnames(data)


##############################################
####### Exploratory Data Analysis (EDA) ######
##############################################

#######################
#### visualization ####
#######################
library(ggplot2)

par(mfrow = c(3, 3)) #to show graph 3x3

# Barplot for Self_Employed
Self_Employed_counts <- table(data$Self_Employed)
barplot(Self_Employed_counts,ylim = c(0,500), main = "Self_Employed Count", xlab = "Self_Employed", col = c("red", "blue"), names.arg = c("Not Employed", "Employed"))

# Barplot for Gender
gender_counts <- table(data$Gender)  # Count occurrences of each category
barplot(gender_counts, main = "Gender Count", xlab = "Gender",ylim = c(0,500), col = c("lightpink", "lightblue"), names.arg = c("Female", "Male"))

# Barplot for Married
married_counts <- table(data$Married)  # Count occurrences of each category
barplot(married_counts, main = "Marital Status Count", xlab = "Marital Status",ylim = c(0,500), col = c("red", "blue"), names.arg = c("Not married", "married"))

# Histogram for LoanAmount (numerical variable)
hist(data$LoanAmount, breaks = 10, main = "Loan Amount Distribution",ylim = c(0,300), xlab = "Loan Amount in Thousnd", col = "green", border = "black")

# Barplot for Education
education_counts <- table(data$Education)
barplot(education_counts, main = "Education Level Count", xlab = "Education Level", col = c("orange", "purple"), names.arg = c("Not Graduate", "Graduate"), ylim = c(0,500))

# Barplot for Loan_Amount_Term
Loan_Amount_Term_counts <- table(data$Loan_Amount_Term)
barplot(Loan_Amount_Term_counts,ylim = c(0,500), main = "Loan Amount Term Count", xlab = "Loan Amount Term", col = c("blue", "orange", "green"))

# Barplot for Credit_History
Credit_History_counts <- table(data$Credit_History)
barplot(Credit_History_counts,ylim = c(0,500), main = "Credit History Count", xlab = "Credit History", col = c("purple", "yellow"), names.arg = c("No Credit", "Has Credit"))

# Barplot for Property_Area
Property_Area_counts <- table(data$Property_Area)
barplot(Property_Area_counts,ylim = c(0,250), main = "Property Area Count", xlab = "Property Area", col = c("green", "blue", "orange"), names.arg = c("Urban", "Semiurban", "Rural"))
Property_Area_counts

# Barplot for Loan Status
Loan_Status_Counts <- table(data$Loan_Status)
barplot(Loan_Status_Counts, main = "Loan_Status Count", xlab = "Loan Status",ylim = c(0,500), col = c("darkred", "darkblue"), names.arg = c("Not Accepted", "Accepted"))

dev.off() # to stop graph and 3x3 graph

##########################
# Check duplicate rows
#########################
duplicates <- duplicated(data)
sum(duplicates)

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
sum(is.na(data)) #missing value in work data
sum(is.na(org_data)) #missing value in orginal data

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

sum(is.na(data)) #missing value in work data
sum(is.na(org_data)) #missing value in orginal data

View(data)


#################
#  outliers
#################

 # Function to remove outliers and return cleaned data and removed values
  remove_outliers <- function(data, column) {
  # Calculate the IQR for the specified column
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
 
  # Define the lower and upper bounds
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
 
  # Identify outliers
  outliers <- data[!(data[[column]] >= lower_bound & data[[column]] <= upper_bound), ]
 
  # Remove outliers from the data
  cleaned_data <- data[data[[column]] >= lower_bound & data[[column]] <= upper_bound | is.na(data[[column]]), ]

  # Return cleaned data and outliers
  return(list(cleaned_data = cleaned_data, removed_values = outliers))
  }
 
  columns_to_check <- c("ApplicantIncome", "CoapplicantIncome", "LoanAmount")

  cleaned_data <- data
  all_removed_values <- data.frame()
 
  # Apply the function to each column
  for (column in columns_to_check) {
  results <- remove_outliers(cleaned_data, column)
  cleaned_data <- results$cleaned_data
 
  # Combine removed outliers from each column
  all_removed_values <- rbind(all_removed_values, results$removed_values)
  }

# Print removed values and dimensions for inspection
 print(all_removed_values)
 print(dim(cleaned_data))
 print(dim(data))
 View(cleaned_data)
 
 data <- cleaned_data


########################################################################################
###############################   modeling  ############################################
########################################################################################
library(caret) #library to split , train , model evaluation , Logistic Regression ,etc..

set.seed(105)  # For reproducibility

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


#model2 - desicion tree
# Load library for decision trees
library(rpart)
library(rpart.plot) #Visualize decision tree library

# Train decision tree
model_tree <- rpart(Loan_Status ~ ., data = trainData, method = "class")

# Visualize decision tree
rpart.plot(model_tree)

# Make predictions on test set
pred_tree <- predict(model_tree, newdata = testData, type = "class")



########################################################################################
###################################   Evaluate  ########################################
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

# Plot bar chart comparing the Accuracy
accu <- c(conf_matrix_logistic$overall["Accuracy"], conf_matrix_tree$overall["Accuracy"])
barplot(accu, names.arg = c("Logistic Regression", "Decision Tree"), col = c("blue", "green"), main = "Model Accuracy Comparison", ylim = c(0, 1))
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted") #code for lines in graph

# Plot ROC Curve for Logistic Regression
library(pROC)  #Visualize roc library 
roc_logistic <- roc(testData$Loan_Status, pred_logistic) 
plot(roc_logistic, main = "ROC Curve for Logistic Regression", col = "blue", lwd = 2) #roc curve graph
cat("AUC for Logistic Regression:", auc(roc_logistic), "\n") # add AUC for Logistic Regression 


