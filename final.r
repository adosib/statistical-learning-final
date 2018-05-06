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
table(observed = test$Abbrev..Cause.of.Death, predicted = yhat.rf)
importance(deaths.rf)

(43+267+87+725)/(43+267+87+725+20+26+17+14+61+62+28+36+43+40+112+137)


