install.packages("openxlsx")
install.packages("mice")

library(openxlsx)
df = read.xlsx('Project Data.xlsx', sheet=1)

#drop variables consisting of character sets and variables not useful for model
drops = c("Cause.of.Death","Date.of.Death","Year","Surname",
           "Given.Names","Notes","Place.of.Birth","Place.of.Death","Place.of.Internment")
df = df[ , !(names(df) %in% drops)]

#convert to factors the variables that are categorical in nature
df[c('Sex','monthYear','Abbrev..Cause.of.Death','Religion','District','Region')] =
  list(as.factor(df$Sex),as.factor(df$monthYear),as.factor(df$Abbrev..Cause.of.Death),
       as.factor(df$Religion),as.factor(df$District),as.factor(df$Region))

df = na.omit(df) #omit records with NA values

#train-test split of data
set.seed(1)
train.set = sample(1:nrow(df),.60*nrow(df),
                   replace=TRUE)
train = df[train.set,]
test = df[-train.set,]

#check frequencies of diseases
as.data.frame(table(df$Abbrev..Cause.of.Death))

####build initial random forest
library(randomForest)
set.seed(1)
deaths.rf = randomForest(Abbrev..Cause.of.Death~.,data=train,importance=TRUE)
yhat.rf = predict(deaths.rf,newdata = test)
table = table(observed=test$Abbrev..Cause.of.Death, predicted = yhat.rf)
sprintf("Classification rate: %f", (table[1][1]+table[6][1]+table[11][1]+table[16][1])/(sum(table)))
importance(deaths.rf)
print(deaths.rf)

#tuning the forest
set.seed(1)
t = tuneRF(train[,-2],train[,2],stepFactor=.5,
           plot=TRUE,ntreeTry = 300,
           trace = TRUE, improve = 0.05) #plot OOB error as a function of mtry
plot(deaths.rf,main="Random Forest: OOB Error vs. Ntrees") #plot OOB error as a function of ntree
rf_tuned = randomForest(Abbrev..Cause.of.Death~.,data=train, na.action='na.omit',
                        ntree=300, mtry=4, importance=TRUE)
yhat.rf = predict(rf_tuned,newdata=test)
table = table(observed = test$Abbrev..Cause.of.Death, predicted = yhat.rf)
sprintf("Classification rate: %f", (table[1][1]+table[6][1]+table[11][1]+table[16][1])/(sum(table)))
print(rf_tuned)

#variable importance visuals
varImpPlot(deaths.rf, main="Variable Importances for Random Forest")
VI_F = importance(deaths.rf)
barplot(t(VI_F/sum(VI_F)),col = c("darkblue","red","aquamarine4","cadetblue4",
                                  "darkorchid4","coral3"))
legend("topright", legend = colnames(VI_F), fill=c("darkblue","red","aquamarine4","cadetblue4",
                                        "darkorchid4","coral3"),cex = 0.75)

####attempting to impute missing data
#check which data is missing
count(is.na(df))
#assign continuous variables to new dataframe
df_cont = df[,c('Age','SexRatio')]
#assign categorical variables to new dataframe
df_factor = df[ , !(names(df) %in% names(df_cont))]
library(mice)
#imputing continuous data
imputed_df_cont = mice(df_cont, m=5, maxit = 50, method = 'pmm', seed = 500)
#combine the 5 imputed models by averaging
combined_age = rowMeans(imputed_df_cont$imp$Age)
combined_sexRatio = rowMeans(imputed_df_cont$imp$SexRatio)
which(is.na(df$Age))
which(is.na(df$SexRatio))
#for loops to replace na values in original dataframe with impute values for Age and SexRatio
for(value in which(is.na(df$Age))){
  for(i in 1:28){ #there were 28 NA values for Age
    df$Age[value]=combined_age[i]
  }
}
for(value in which(is.na(df$SexRatio))){
  for(i in 1:28){ #likewise for SexRatio
    df$SexRatio[value]=combined_sexRatio[i]
  }
}
library(plyr)
count(is.na(df$Age))
count(is.na(df$SexRatio))
#imputing factor data
imputed_df_factor = mice(df_factor, m=1, maxit = 50, method = 'polyreg', seed = 500)
imputed_sex = t(imputed_df_factor$imp$Sex)
imputed_religion = t(imputed_df_factor$imp$Religion)
#replace NA values in df with imputed values for Sex and Religion
for(value in which(is.na(df$Sex))){
  for(i in 1:11){ #there were 11 NA values for Sex
    df$Sex[value]=imputed_sex[i]
  }
}
for(value in which(is.na(df$Religion))){
  for(i in 1:26){ #there were 26 NA values for Religion
    df$Religion[value]=imputed_religion[i]
  }
}

#tuned random forest with no NA values in the dataframe
set.seed(1)
train.set = sample(1:nrow(df),.60*nrow(df),
                    replace=TRUE)
train = df[train.set,]
test = df[-train.set,]
rf_tuned = randomForest(Abbrev..Cause.of.Death~.,data=train,
                          ntree=300, mtry=4, importance=TRUE)
yhat.rf = predict(rf_tuned,newdata=test)
table = table(observed = test$Abbrev..Cause.of.Death, predicted = yhat.rf)
sprintf("Classification rate: %f", (table[1][1]+table[6][1]+table[11][1]+table[16][1])/(sum(table)))
