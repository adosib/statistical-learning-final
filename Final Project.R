#Open the data & split into training and test
library(readxl)
my.data <- read_excel("Documents/2018 - Spring Semester/Stat 4510/Project Data.xlsx")
set.seed(1)
sample <- sample.int(n = nrow(my.data), size = floor(.90*nrow(my.data)), replace = F)
train <- my.data[sample, ]
test  <- my.data[-sample, ]

#Fit Boosting Model
library(mboost)
