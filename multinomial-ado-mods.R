library(readxl)
newdata = read_xlsx("~/Desktop/data.xlsx")
#drop variables that cannot be used in the model or are not useful
drops = c("Cause of Death","Date of Death","Year","Surname",
           "Given Names","Notes","Place of Birth","Place of Death","Place of Internment")
newdata = newdata[ ,!(names(newdata) %in% drops)]

#convert to categorical
newdata[c('Sex','Month & Year','Abbrev. Cause of Death','Religion','District','Region')] =
  list(as.factor(newdata$Sex),as.factor(newdata$`Month & Year`),as.factor(newdata$`Abbrev. Cause of Death`),
       as.factor(newdata$Religion),as.factor(newdata$District),as.factor(newdata$Region))
newdata$`Age` = as.numeric(newdata$`Age`)
newdata$`Age` = scale(newdata$`Age`)

#set training and test sets
set.seed(1)
train = sample(1:nrow(newdata),size = .9*nrow(newdata),replace=FALSE)
newdata.train = newdata[train, ]
newdata.test = newdata[-train, ]

#multinomial logistic regression
library(nnet)
set.seed(1)
glm.fit = multinom(`Abbrev. Cause of Death` ~., data=newdata.train)
yhat.glm=predict(glm.fit, newdata=newdata.test)
table = table(observed = newdata.test$`Abbrev. Cause of Death`, predicted = yhat.glm)
table
sprintf("Classification rate: %f", (table[1,1]+table[2,2]+table[3,3]+table[4,4])/(sum(table)))
summary(glm.fit)
