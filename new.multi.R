library(openxlsx)
data = read.xlsx("~/Desktop/data.xlsx")

#drop variables that cannot be used in the model or are not useful
#drop variables that cannot be used in the model or are not useful
drops <- c("Cause.of.Death","Date.of.Death","Year","Surname",
           "Given.Names","Notes","Place.of.Birth","Place.of.Death","Place.of.Internment")
data = data[ , !(names(data) %in% drops)]

#convert to categorical
data[c('Sex','monthYear','Abbrev..Cause.of.Death','Religion','District','Region')] <-
  list(as.factor(data$Sex),as.factor(data$monthYear),as.factor(data$Abbrev..Cause.of.Death),
       as.factor(data$Religion),as.factor(data$District),as.factor(data$Region))


#set training and test sets
set.seed(1)
train.set = sample(1:nrow(data), size = .9*nrow(data), replace=FALSE)
train = data[train.set,]
test = data[-train.set,]


#multinomial logistic regression
library(nnet)
glm.fit = multinom(Abbrev..Cause.of.Death~., data=train)
yhat.glm=predict(glm.fit,newdata=test)
table = table(observed = test$Abbrev..Cause.of.Death, predicted = yhat.glm)
sprintf("Classification rate: %f", (table[1][1]+table[6][1]+table[11][1]+table[16][1])/(sum(table)))
summary(glm.fit)
