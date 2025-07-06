# Install packages
install.packages("ggplot2")
library(ggplot2)
install.packages("psych")
library(psych)
install.packages("dplyr")
library(dplyr)
install.packages("moments")
library(moments)

# Check working directory/change working directory
getwd()
setwd("C:/Users/admin/factor_analysis/BIG5/BIG5")

# Import data
data = read.csv("data.csv",header = TRUE,sep = "")

# Descriptive Statistics - Exploratory Data Analysis
str(data)

# Replace race column with the corresponding values
data$race[data$race == 0] = "Other (0=missed)"
data$race[data$race == 1] = "Mixed Race"
data$race[data$race == 2] = "Arctic (Siberian, Eskimo)"
data$race[data$race == 3] = "Caucasian (European)"
data$race[data$race == 4] = "Caucasian (Indian)"
data$race[data$race == 5] = "Caucasian (Middle East)"
data$race[data$race == 6] = "Caucasian (North African, Other)"
data$race[data$race == 7] = "Indigenous Australian"
data$race[data$race == 8] = "Native American"
data$race[data$race == 9] = "North East Asian (Mongol, Tibetan, Korean Japanese, etc)"
data$race[data$race == 10] = "Pacific (Polynesian, Micronesian, etc)"
data$race[data$race == 11] = "South East Asian (Chinese, Thai, Malay, Filipino, etc)"
data$race[data$race == 12] = "West African, Bushmen, Ethiopian"
data$race[data$race == 13] = "Other (0=missed)"

# Replace engnat column, 1 == Yes, 2 == No
data$engnat = ifelse(data$engnat == 1, "Yes", "No")

# Replace gender column, 1 == Male, 2 == Female, 3 == Other
data$gender = ifelse(data$gender == 1, "Male",ifelse(data$gender == 2, "Female", "Other"))

# Replace hand column, 1 == Right, 2 == Left, 3 == Both
data$hand = ifelse(data$hand == 1, "Right", ifelse(data$hand == 2, "Left", "Both"))

# Drop NAs in demographic data
which(is.na(data)==TRUE)
data = na.omit(data)

# Age column : Remove ages above 100 years old
data = subset(data, data$age <= 100)

# Age Distribution
ggplot(data = data,mapping = aes(x = age))+
  geom_histogram(colour = "blue",fill = "blue")+
  ylab("Frequency")+ggtitle("Age Distribution")

# Age Distribution Statistics
summary(data$age)

# Age Skewness
skewness(data$age)

# Age by Gender
data %>% group_by(gender) %>% summarise(mean(age))

# Gender Barplot
ggplot(data = data, mapping = aes(x = gender))+
  geom_bar(fill = c("lightpink","lightblue","red"))+
  ylab("Frequency")+ggtitle("Gender Frequency")

# Gender Count
data %>% group_by(gender) %>% summarise(count = n())

# Hand Barplot
ggplot(data = data, mapping = aes(x = hand))+
  geom_bar(fill = c("lightpink","lightblue","red"))+
  ylab("Frequency")+ggtitle("Main Hand Frequency")

# Hand count
data %>% group_by(hand) %>% summarise(count = n())

# Race Frequencies
data %>% group_by(race) %>% summarise(n = n()) %>% arrange(desc(n))

# Country Frequencies
data %>% group_by(country) %>% summarise(n = n()) %>% arrange(desc(n))

# Split data into demografics (exclude source column) and questionaire 
demo_data = subset(data, select = c("race","age","engnat","gender","hand","country"))
question_data = data %>% select(E1:O10)

####################### Factor Analysis ########################################

# Step 1 : Check if the dataset is suitable for factor analysis
0.00001 < det(cov(question_data)) # Determinant of covariance matrix bigger than 0.00001
cortest.bartlett(question_data) # Bartlett Test of Sphericity - Reject Null Hypothesis
KMO(question_data) # MSA for each item bigger than 0.6 and overall MSA = 0.91

# Step 2 : Determine the number of factors
eigen_values = eigen(cor(question_data))$values
var_explained = cumsum(eigen_values)/sum(eigen_values)
scree(question_data) # 5
fa.parallel(question_data)
VSS(question_data) # 5 ή 6 

# Step 3 : Estimate Models Parameters
factor_model_fit = principal(question_data,nfactors = 5,rotate = "varimax",scores=TRUE)
print.psych(factor_model_fit,cut = 0.35, sort = TRUE)
com_names = names(factor_model_fit$communality)
val = unname(factor_model_fit$communality)
communalities = cbind(com_names,val)
communalities















