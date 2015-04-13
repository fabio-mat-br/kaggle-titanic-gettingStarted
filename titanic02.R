setwd("C:/Users/Suzana e Fabio/Fabio/specialization/kaggle-titanic-gettingStarted")
library(rpart)
train <- read.csv("train.csv")

test <- read.csv("test.csv")
test$Survived <- NA
combined <- rbind(train, test)
table(complete.cases(combined))

combined$Name <- as.character(combined$Name)
combined$Title <- sapply(combined$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combined$Title <- sub(' ', '', combined$Title)
combined$Title[combined$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combined$Title[combined$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combined$Title[combined$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# title is a factor, a categorized data
combined$Title <- factor(combined$Title)
combined$FamilySize <- combined$SibSp + combined$Parch + 1
#View(combined)
# str(na.omit(combined))
# complete.cases(combined)
# table(is.na(combined$Age))
#na.locf(combined)
library(zoo)

combined$Age <- na.spline(combined$Age)
combined$Fare <- na.spline(combined$Fare)

## combined[complete.cases(combined), ]
#complete.cases(combined)

#fit <- rpart(train$Survived ~ train$Pclass + 
#               train$Sex + 
#               train$Age + 
#               train$SibSp + 
#               train$Parch + 
#               train$Fare + 
#               train$Embarked + 
#               train$FamilySize + 
#               train$Title)
fit <- NULL
fit <- rpart(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Title + FamilySize, data=combined, method="class")

clean_test <- combined[892:1309,]
prediction <- predict(fit, clean_test, type = "class")
length(prediction)

result <- data.frame(PassengerId = clean_test$PassengerId, Survived = prediction)
write.csv(result, file = "titanic-brolesi.csv", row.names = FALSE)

