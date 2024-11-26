#setwd("C:\Users\osama\OneDrive\Desktop\university\lvl 7\ISY351-project\project\project-code.R")
odata <- read.csv("C://Users//osama//OneDrive//Desktop//university//lvl 7//ISY351-project//project//datasets//train_u6lujuX_CVtuZ9i (1).csv")
#odata <- read.csv("C:/Users/brooo/OneDrive/سطح المكتب/IS/مستوى السابع/ISY351/Project/ISY351-project/project/datasets/train_u6lujuX_CVtuZ9i (1).csv")

odata
View(data)

#we dont want this column
odata[12] <- NULL

data <- odata
data$Married <- ifelse(data$Married == "Yes", 1, ifelse(data$Married == "No", 0, NA))
data$Gender <- ifelse(data$Gender == "Male", 1, ifelse(data$Gender == "Female", 0, NA))
data$Education <- ifelse(data$Education == "Graduate", 1, ifelse(data$Education == "Not Graduate", 0, NA))
data$Self_Employed <- ifelse(data$Self_Employed == "Yes", 1, ifelse(data$Self_Employed == "No", 0, NA))
data$Loan_Status <- ifelse(data$Loan_Status == "Y", 1, ifelse(data$Loan_Status == "N", 0, NA))


View(data)
View(odata)
############################
# Understanding the Data
############################


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
library(dplyr)
library(caret)
#numbers of missing values
sum(is.na(data))
sum(is.na(odata))



is.na(data)
is.na(odata)
#numbers of missing values for each columns
colSums(is.na(data))
colSums(is.na(odata))


summary(data)

par(mfrow = c(3, 2))


hist(data$Gender,breaks =3.5, labels = c("Female", "Male"), main = "Gender count", xlab = "Gender")
hist(data$Married,breaks =3.5, labels = c("not married", "married"), main = "married count", xlab = "")
hist(data$LoanAmount,breaks =3.5, main = "loan amount count")
hist(data$Education,breaks =3.5, main = "graduate count")

# Barplot for Self_Employed
Self_Employed_counts <- table(data$Self_Employed)
barplot(Self_Employed_counts, main = "Self_Employed Count", xlab = "Self_Employed", col = c("red", "blue"))

# Barplot for Gender
gender_counts <- table(data$Gender)  # Count occurrences of each category
barplot(gender_counts, main = "Gender Count", xlab = "Gender", col = c("lightblue", "lightpink"))

# Barplot for Married
married_counts <- table(data$Married)  # Count occurrences of each category
barplot(married_counts, main = "Marital Status Count", xlab = "Marital Status", col = c("lightgreen", "lightyellow"))

# Histogram for LoanAmount (numerical variable)
hist(data$LoanAmount, breaks = 10, main = "Loan Amount Distribution", xlab = "Loan Amount", col = "lightblue", border = "black")

# Barplot for Education
education_counts <- table(data$Education)
# Count occurrences of each category
barplot(education_counts, main = "Education Level Count", xlab = "Education Level", col = c("lightcoral", "lightcyan"), names.arg = c("Not Graduate", "Graduate"), ylim = c(0,500))

# Barplot for Loan Status
Loan_Status_Counts <- table(data$Loan_Status)
barplot(Loan_Status_Counts, main = "Loan_Status Count", xlab = "Loan Status", col = c("lightcoral", "lightcyan"), names.arg = c("Not Accepted", "Accepted"))




########################################################################################'
########################################################################################'
# Handling missing values (imputation example)

numeric_columns <- sapply(data, is.numeric)
data_numeric <- data[, numeric_columns]

data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.factor), ~ ifelse(is.na(.), "Unknown", as.character(.))))

# Encoding categorical variables (if applicable)
data <- data %>%
  mutate(across(where(is.factor), as.numeric))  # Converts factors to numeric

# Normalize numeric variables
preProcess_range <- preProcess(data_numeric, method = c("range"))
data_numeric_scaled <- predict(preProcess_range, data_numeric)

View(data)


