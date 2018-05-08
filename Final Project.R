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
data.train <- my.data[sample,c(1,3,4,5,6,7,8)]
data.test  <- my.data[-sample,c(1,3,4,5,6,7,8)]
data.train.target <- my.data[sample,2]
data.test.target<- my.data[-sample,2]

#Fit Initial KNN Model
set.seed(1)
library(class)
knn.model <- kNN(train = data.train[1:7], test = data.test[1:7], data.train[1], k = 10)
#wtf