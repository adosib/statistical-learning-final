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

# add interaction term between Age and SexRatio
wt <- projectdata$Age * projectdata$SexRatio
projectdata <- data.frame(projectdata, wt)

# make sure variables are in as factors
# changed cause of death to numeric for classification purposes (numbered in alphabetical order):
  # 1 = bronchitis
  # 2 = influenza
  # 3 = pneumonia
  # 4 = tuberculosis
projectdata[c('MY', 'CoD','Religion', 'District', 'Region')] <-
  list(as.factor(projectdata$MY), as.factor(projectdata$CoD),as.factor(projectdata$Religion), 
       as.factor(projectdata$District),as.factor(projectdata$Region))
projectdata$CoD <- as.numeric(projectdata$CoD)

# separate train and test sets
set.seed(1)
train.set <- sample(1:nrow(projectdata), 0.9*nrow(projectdata), replace = FALSE)
train <- projectdata[train.set,]
test <- projectdata[-train.set,]
death.test <- test$CoD


# run first model to see significant variables
# "cr" = classification rate
gam1 <- gam(CoD ~ ., data = train)
summary(gam1)
pred1 <- predict(gam1, newdata = test, type = "link")
pred1 <- round(pred1)
cr1 <- sum(pred1 == death.test) / length(pred1)


# second model using those significant variables
gam2 <- gam(CoD ~ MY + SexRatio + District + Age, data = train)
summary(gam2)
pred2 <- predict(gam2, newdata = test, type = "link")
pred2 <- round(pred2)
cr2 <- sum(pred2 == death.test) / length(pred2)

# MY/district/religion/age
gam3 <- gam(CoD ~ MY + District + Religion + Age, data = train)
summary(gam3)
pred3 <- predict(gam3, newdata = test, type = "link")
pred3 <- round(pred3)
cr3 <- sum(pred3 == death.test) / length(pred3)

# getting rid of age
gam4 <- gam(CoD ~ MY + District + Religion, data = train)
summary(gam4)
pred4 <- predict(gam4, newdata = test, type = "link")
pred4 <- round(pred4)
cr4 <- sum(pred4 == death.test) / length(pred4)

# an anova test to see if any of these are actually better than the first model that includes all of the predictors
anova(gam1, gam2, gam3, gam4, test = "F")

classifications <- data.frame(death.test, pred1, pred2, pred3, pred4)
avgclass <- rowMeans(classifications)
classifications <- data.frame(classifications, avgclass)
testrates <- c(cr1, cr2, cr3, cr4)

# so far these are all pretty bad but model 4 without the age variable is performing the best.... 

# using gam2 to add a spline to continuous variables
gam5 <- gam(CoD ~ MY + District + s(SexRatio) + s(Age), data = train)
summary(gam5)
pred5 <- predict(gam5, newdata = test, type = "link")
pred5 <- round(pred5)
cr5 <- sum(pred5 == death.test) / length(pred5)

# using gam3 to add a spline to continuous variables
gam6 <- gam(CoD ~ MY + District + Religion + s(Age), data = train)
summary(gam6)
pred6 <- predict(gam6, newdata = test, type = "link")
pred6 <- round(pred6)
cr6 <- sum(pred6 == death.test) / length(pred6)

testrates <- c(testrates, cr5, cr6)
plot(testrates, pch = 19, col = "red")

# this anova shows that there really is no reason to have Age in there at all as a predictor, 
# F is so high that even though this model has the highest prediction accuracy, its performace still technically
# is not better than those that do not have Age in them at all?
anova(gam1, gam5, gam6, test = "F")

gam7 <- gam(CoD ~ MY + District + Religion + lo(Age) + lo(SexRatio), data = train)
summary(gam7)
pred7 <- predict(gam7, newdata = test, type = "link")
pred7 <- round(pred7)
cr7 <- sum(pred7 == death.test) / length(pred7)
plot.Gam(gam7, col = "red", se = TRUE)

gam8 <- gam(CoD ~ MY + District + Religion + poly(Age, 2) + lo(SexRatio), data = train)
summary(gam8)
pred8 <- predict(gam8, newdata = test, type = "link")
pred8 <- round(pred8)
cr8 <- sum(pred8 == death.test) / length(pred8)
plot.Gam(gam8, col = "red", se = TRUE)

# add interaction term between Age and SexRatio 
gam9 <- gam(CoD ~ MY + District + s(Age) + s(SexRatio) + s(wt), data = train)
summary(gam9)
pred9 <- predict(gam9, newdata = test, type = "link")
pred9 <- round(pred9)
cr9 <- sum(pred9 == death.test) / length(pred9)
plot.Gam(gam9, col = "red", se = TRUE)
