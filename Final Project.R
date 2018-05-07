#Open and format data
library(readxl)
my.data <- read_excel("Documents/2018 - Spring Semester/Stat 4510/statistical-learning-final/Project Data.xlsx")
my.data <- my.data[c(2,5,8,9,10,11,15,16)]
colnames(my.data) <- c("MonthYear", "COD", "Age", "Sex", "SexRatio", "Religion", "District", "Region")

#Omit NA Values
library(dplyr)
my.data <- my.data %>% filter(!is.na(MonthYear))
my.data <- my.data %>% filter(!is.na(COD))
my.data <- my.data %>% filter(!is.na(Age))
my.data <- my.data %>% filter(!is.na(Sex))
my.data <- my.data %>% filter(!is.na(SexRatio))
my.data <- my.data %>% filter(!is.na(Religion))
my.data <- my.data %>% filter(!is.na(District))
my.data <- my.data %>% filter(!is.na(Region))

#Convert to Factors
my.data$MonthYear <- as.factor(my.data$MonthYear)
my.data$Age <- as.factor(my.data$Age)
my.data$Sex <- as.factor(my.data$Sex)
my.data$Religion <- as.factor(my.data$Religion)
my.data$District <- as.factor(my.data$District)
my.data$Region <- as.factor(my.data$Region)

#Split into Training and Test
set.seed(1)
sample <- sample.int(n = nrow(my.data), size = floor(.90*nrow(my.data)), replace = F)
train <- my.data[sample, ]
test  <- my.data[-sample, ]

#Fit Initial Boosting Model
set.seed(1)
library(gbm)
library(caret)
model.gbm <- gbm(COD ~ ., data = train, distribution = "multinomial", n.trees = 100)
model.gbm
predict.gbm(model.gbm, train, n.trees = 100, type = "response")
confusionMatrix(train.pred, train$COD)
