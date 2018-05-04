data = read_xlsx("~/Desktop/data.xlsx")
summary(data)
names(data)

pairs(data)
cor(data[, -9])

#set training and test
set.seed(1)
train = sample(1:nrow(data),size = .9*nrow(data),replace=FALSE)
test = (1:nrow(data))[-train]
data.train = data[train, ]
data.test = data[test, ]

#logistic regression
glm.fit=glm(Sex ~ Year + `Month & Year` + 
              `Date of Death` +
              `Cause of Death` + `Abbrev. Cause of Death` + Surname
            + `Given Names` + `Age (Years)` + `Sex` + `SexRatio` + `Religion`
            + `Place of Birth` + `Place of Death` + `Place of Internment`,
            family=binomial ,data=data.train)
summary(glm.fit)