install.packages('openxlsx')
library(openxlsx)
df = read.xlsx('Project Data.xlsx', sheet=1)
drops <- c("Cause.of.Death","Month.&.Year","Surname","Given.Names")
df = df[ , !(names(df) %in% drops)]

set.seed(1)
train.set = sample(1:nrow(df),.65*nrow(df),
                   replace=FALSE)
train = df[train.set,]
test = df[-train.set,]

