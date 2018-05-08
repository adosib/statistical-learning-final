newdata = read_xlsx("~/Desktop/data.xlsx")

#drop variables that cannot be used in the model or are not useful
drops <- c("Cause of Death","Date of Death","Month & Year","Surname",
           "Given Names","Notes","Place of Birth","Place of Death","Place of Internment")
newdata = newdata[ ,!(names(newdata) %in% drops)]
newdata <- newdata[newdata$Year!=1910,]


#convert to categorical
newdata[c('Sex','Year','Abbrev. Cause of Death','Religion','District','Region')] <-
  list(as.factor(newdata$Sex),as.factor(newdata$Year),as.factor(newdata$`Abbrev. Cause of Death`),
       as.factor(newdata$Religion),as.factor(newdata$District),as.factor(newdata$Region))
newdata$`Age (Years)` = as.numeric(newdata$`Age (Years)`)
newdata$`Age (Years)` = scale(newdata$`Age (Years)`)

#set training and test sets
set.seed(1)
train = sample(1:nrow(newdata),size = .9*nrow(newdata),replace=FALSE)
test = (1:nrow(newdata))[-train]
newdata.train = newdata[train, ]
newdata.test = newdata[test, ]


#multinomial logistic regression
library(nnet)
library(foreign)


glm.fit = multinom(`Abbrev. Cause of Death` ~. + District:`Age (Years)` + District:Region, data=newdata.train)
yhat.glm=predict(glm.fit, newdata=newdata.test)
table = table(observed = newdata.test$`Abbrev. Cause of Death`, predicted = yhat.glm)
table
sprintf("Classification rate: %f", (table[1,1]+table[2,2]+table[3,3]+table[4,4])/(sum(table)))



summary(glm.fit)


