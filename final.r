install.packages('openxlsx')
library(openxlsx)
df = read.xlsx('Project Data.xlsx', sheet=1)

#drop variables consisting of character sets and variables not useful for model
drops <- c("Cause.of.Death","Date.of.Death","Year","Surname",
           "Given.Names","Notes","Place.of.Birth","Place.of.Death","Place.of.Internment")
df = df[ , !(names(df) %in% drops)]


#convert to factors the variables that are categorical in nature
df[c('Sex','monthYear','Abbrev..Cause.of.Death','Religion','District','Region')] <-
  list(as.factor(df$Sex),as.factor(df$monthYear),as.factor(df$Abbrev..Cause.of.Death),
       as.factor(df$Religion),as.factor(df$District),as.factor(df$Region))

set.seed(1)
train.set = sample(1:nrow(df),.60*nrow(df),
                   replace=FALSE)
train = df[train.set,]
test = df[-train.set,]
train_index=c(as.numeric(rownames(train)))

#check frequencies of diseases
as.data.frame(table(df$Abbrev..Cause.of.Death))

library(tree)
library(randomForest)

deaths.rf = randomForest(Abbrev..Cause.of.Death~.,data=train,na.action='na.omit',importance=TRUE)
yhat.rf=predict(deaths.rf,newdata=test)
table = table(observed = test$Abbrev..Cause.of.Death, predicted = yhat.rf)
sprintf("Classification rate: %f", (table[1][1]+table[6][1]+table[11][1]+table[16][1])/(sum(table)))
importance(deaths.rf)

#train = na.omit(train)
#X = train[,-which(names(train)=="Abbrev..Cause.of.Death")]

#variable importance visuals
library(caret)
varImpPlot(deaths.rf)
VI_F=importance(deaths.rf)
barplot(t(VI_F/sum(VI_F)))



