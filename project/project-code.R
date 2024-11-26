#setwd("C:\Users\osama\OneDrive\Desktop\university\lvl 7\ISY351-project\project\project-code.R")
data <- read.csv("C://Users//osama//OneDrive//Desktop//university//lvl 7//ISY351-project//project//datasets//train_u6lujuX_CVtuZ9i (1).csv")
data
View(data)

#we dont want this column
data[12] <- NULL

data2 <- data
data2$Married <- ifelse(data$Married == "Yes", 1, ifelse(data$Married == "No", 0, NA))
data2$Gender <- ifelse(data$Gender == "Male", 1, ifelse(data$Gender == "Female", 0, NA))
View(data2)
############################
# Understanding the Data
############################


str(data2)


summary(data2)

#to see the first and last 6 rows of the data
head(data2)
tail(data2)

#to know the numbers of rows and col
dim(data2)

# to know the col names
colnames(data2)


###################################
# Exploratory Data Analysis (EDA)
###################################
library(ggplot2)

#numbers of missing values
sum(is.na(data2))
#numbers of missing values for each columns
colSums(is.na(data2))


summary(data)

par(mfrow = c(2, 2))

ggplot(numeric_columns, aes(x = Gender)) + geom_histogram()

hist(data2$Gender,breaks =3.5, labels = c("Female", "Male"), main = "Gender count", xlab = "Gender")
hist(data2$Married,breaks =3.5, labels = c("not married", "married"), main = "married count", xlab = "")
