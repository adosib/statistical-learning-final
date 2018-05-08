#Open and format data
library(readxl)
my.data <- read_excel("Documents/2018 - Spring Semester/Stat 4510/Project Data.xlsx")
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

#Change classifications to numbers and Convert to numeric
my.data$COD <- factor(my.data$COD,
                    levels = c("tuberculosis","bronchitis","pneumonia","influenza"),
                    labels = c("0","1","2","3"))
my.data$COD <- as.numeric(my.data$COD)
my.data$Age <- as.numeric(my.data$Age)
my.data$Sex <- factor(my.data$Sex, 
                      levels = c("M","F"),
                      labels = c("0","1"))
my.data$Sex <- as.numeric(my.data$Sex)
my.data$Religion <- factor(my.data$Religion,
                           levels=c("Catholic","CE","Cong","J","M","Org","P","Pb","RC","SA"),
                           labels=c("0","1","2","3","4","5","6","7","8","9"))
my.data$Religion <- as.numeric(my.data$Religion)
my.data$District <- factor(my.data$District,
                           levels=c("Bay de Verde","Bonavista","Burgeo/La Poile","Burin","Carbonear","Ferryland","Fogo","Fortune Bay","Harbour Grace","Harbour Main","Placentia/St. Mary's","Port de Grave","St. Barbe","St. George","St. John's","Trinity Bay","Twillingate"),
                           labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))
my.data$District <- as.numeric(my.data$District)
my.data$Region <- factor(my.data$Region,
                         levels=c("Avalon","North","South","West"),
                         labels=c("1","2","3","4"))
my.data$Region <- as.numeric(my.data$Region)
my.data$MonthYear <- factor(my.data$MonthYear,
                            levels=c("Jan 1918","Feb 1918","Mar 1918","Apr 1918","May 1918","June 1918","July 1918","Aug 1918","Sept 1918","Oct 1918","Nov 1918","Dec 1918","Jan 1919","Feb 1919","Mar 1919","Apr 1919","May 1919","June 1919","July 1919","Aug 1919","Sept 1919","Oct 1919","Nov 1919","Dec 1919","Jan 1920","Feb 1920","Mar 1920","Apr 1920","May 1920","June 1920","July 1920","Aug 1920","Sept 1920","Oct 1920","Nov 1920","Dec 1920"),
                            labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36"))
my.data$MonthYear <- as.numeric(my.data$MonthYear)

#Split into Training and Test
set.seed(1)
sample <- sample.int(n = nrow(my.data), size = floor(.90*nrow(my.data)), replace = F)
data.train <- my.data[sample,]
data.test  <- my.data[-sample,]

#Fit Initial KNN Model (K=10)
set.seed(1)
library(class)
knn.10 <- knn(train = data.train, test = data.test, cl = data.train$COD, k = 10)
table(data.test$COD, knn.10)
(175+8+22+89)/(429)

#Variable Importance
library(caret)
model.tree  <- train(COD ~ .,
                     data = data.train,
                     method = "rpart")
plot(varImp(model.tree), top = 7)

#Subset the data with the variables with the most VI
new.train.1 <- my.data[sample, -4]
new.test.1 <- my.data[-sample, -4]
new.train <- new.train.1[,-5]
new.test <- new.test.1[,-5]

#Fit new KNN with K = 10
set.seed(1)
library(class)
newknn.10 <- knn(train = new.train, test = new.test, cl = new.train$COD, k = 10)
table(new.test$COD, newknn.10)
(177+13+21+94)/(429)

#Subset the data, removing District since this is captured by Region and has 
#less importance than Region and fit a new KNN with K=10
final.train <- new.train[,-5]
final.test <- new.test[,-5]
set.seed(1)
library(class)
finalknn.10 <- knn(train = final.train, test = final.test, cl = final.train$COD, k = 10)
table(final.test$COD, finalknn.10)
(184+17+38+97)/(429)

#Does this work
#DONT LOOK AT THIS YET!! NOT DONE
set.seed(1)
knn.1 <- knn(train = data.train, test = data.test, cl = data.train$COD, k = 1)
table(data.test$COD, knn.1)
(172+17+34+90)/(429)

set.seed(1)
knn.2 <- knn(train = data.train, test = data.test, cl = data.train$COD, k = 2)
table(data.test$COD, knn.2)
(169+12+28+90)/(429)

set.seed(1)
knn.3 <- knn(train = data.train, test = data.test, cl = data.train$COD, k = 3)
table(data.test$COD, knn.3)
(174+15+19+87)/(429)

set.seed(1)
knn.5 <- knn(train = data.train, test = data.test, cl = data.train$COD, k = 5)
table(data.test$COD, knn.5)
(170+11+24+89)/(429)

set.seed(1)
knn.7 <- knn(train = data.train, test = data.test, cl = data.train$COD, k = 7)
table(data.test$COD, knn.7)
(171+9+26+90)/(429)

set.seed(1)
knn.8 <- knn(train = data.train, test = data.test, cl = data.train$COD, k = 8)
table(data.test$COD, knn.8)
(168+7+26+91)/(429)

set.seed(1)
knn.15 <- knn(train = data.train, test = data.test, cl = data.train$COD, k = 15)
table(data.test$COD, knn.15)
(168+9+17+88)/(429)

set.seed(1)
knn.20 <- knn(train = data.train, test = data.test, cl = data.train$COD, k = 20)
table(data.test$COD, knn.20)
(169+7+17+90)/(429)

#Table of K values and misclassification rates
K = c()
Rate = c()
plot(K,Rate,ylab="Classification Rate")
lines(K,Rate)