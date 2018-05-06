# FINAL PROJECT
# GAMs
# taylor


# load data and libraries
library(readr)
Project_Data <- read_csv("~/School/Applied Statistical Models/Project Data.csv")
projectdata <- Project_Data
library(gam)
library(rgr)
library(dplyr)

projectdata <- projectdata[-c(1, 3, 4, 6, 7, 12:14, 17:25)]
colnames(projectdata) <- c("MY", "CoD", "Age", "Sex", "SexRatio", "Religion", "District", "Region")

# changed sex to M = 1 and F = 0
attach(projectdata)
projectdata$Sex[which(projectdata$Sex == "M")] <- 1 
projectdata$Sex[which(projectdata$Sex == "F")] <- 0
projectdata$Sex <- as.factor(projectdata$Sex)

# %>% is the pipe function; tell it to only give you things that are not NA values for that vector in the data frame. 
# need to load "dplyr" function
projectdata <- projectdata %>% filter(!is.na(Age))
projectdata <- projectdata %>% filter(!is.na(Sex))
projectdata <- projectdata %>% filter(!is.na('Death date'))
projectdata <- projectdata %>% filter(!is.na(Religion))


# make sure variables are in as factors
# changed cause of death to numeric for classification purposes (numbered in alphabetical order):
  # 1 = bronchitis
  # 2 = influenza
  # 3 = pneumonia
  # 4 = tuberculosis
projectdata[c('MY','CoD','Religion', 'District', 'Region')] <-
  list(as.factor(projectdata$MY),as.numeric(projectdata$CoD),as.factor(projectdata$Religion), 
       as.factor(projectdata$District),as.factor(projectdata$Region))

# separate train and test sets
set.seed(1)
train.set <- sample(1:nrow(projectdata), 0.9*nrow(projectdata), replace = FALSE)
train <- projectdata[train.set,]
test <- projectdata[-train.set,]
death.test <- test$CoD


# run first model to see significant variables
gam1 <- gam(CoD ~ ., data = train)
summary(gam1)
pred1 <- predict(gam1, newdata = test, type = "link")
pred1 <- round(pred1)
sum(pred1 == death.test) / length(pred1)


# second model using those significant variables
gam2 <- gam(CoD ~ MY + SexRatio + District + Age, data = train)
summary(gam2)
pred2 <- predict(gam2, newdata = test, type = "link")
pred2 <- round(pred2)
sum(pred2 == death.test) / length(pred2)

# MY/district/religion/age
gam3 <- gam(CoD ~ MY + District + Religion + Age, data = train)
summary(gam3)
pred3 <- predict(gam3, newdata = test, type = "link")
pred3 <- round(pred3)
sum(pred3 == death.test) / length(pred3)

# getting rid of age
gam4 <- gam(CoD ~ MY + District + Religion, data = train)
summary(gam4)
pred4 <- predict(gam4, newdata = test, type = "link")
pred4 <- round(pred4)
sum(pred4 == death.test) / length(pred4)

test.rates <- data.frame(death.test, pred1, pred2, pred3, pred4)

