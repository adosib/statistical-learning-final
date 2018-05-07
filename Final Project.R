#Open and format data
library(readxl)
my.data <- read_excel("Project Data.xlsx")
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
my.data$COD <- as.factor(my.data$COD)
my.data$Age <- as.factor(my.data$Age)
my.data$Sex <- as.factor(my.data$Sex)
my.data$SexRatio <- as.factor(my.data$SexRatio)
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
model.gbm <- gbm(COD ~ MonthYear+Age+Sex+SexRatio+Religion+District+Region, data = train, 
                 distribution = "gaussian", n.trees = 100)
model.gbm
summary.gbm(model.gbm)
predict.gbm(model.gbm, test, n.trees = 100)
