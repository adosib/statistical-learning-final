# FINAL PROJECT
# GAMs
# taylor


# load data
library(readr)
Project_Data <- read_csv("~/School/Applied Statistical Models/Project Data.csv")
projectdata <- Project_Data

library(gam)
library(rgr)
library(dplyr)

projectdata <- projectdata[-c(1, 3, 4, 6, 7, 12:14, 17:25)]
colnames(projectdata) <- c("MY", "CoD", "Age", "Sex", "SexRatio", "Religion", "District", "Region")

attach(projectdata)
set.seed(1)
projectdata$Sex[which(projectdata$Sex == "M")] <- 1 
projectdata$Sex[which(projectdata$Sex == "F")] <- 0
projectdata <- projectdata %>% filter(!is.na(Age))
projectdata <- projectdata %>% filter(!is.na(Sex))
projectdata <- projectdata %>% filter(!is.na('Death date'))
projectdata <- projectdata %>% filter(!is.na(Religion))
