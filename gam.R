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

projectdata[c('MY','CoD','Religion', 'District', 'Region')] <-
  list(as.factor(projectdata$MY),as.factor(projectdata$CoD),as.factor(projectdata$Religion), 
       as.factor(projectdata$District),as.factor(projectdata$Region))

set.seed(1)
train.set <- sample(1:nrow(projectdata), 0.9*nrow(projectdata), replace = FALSE)
train <- projectdata[train.set,]
test <- projectdata[-train.set,]

gam1 <- gam(CoD ~ Sex + Age + Region, family = binomial, data = train)
summary(gam1)
plot.Gam(gam1, se = TRUE)

