setwd("E:/challenges/titanic")

library(rpart)
library(zoo)
library(randomForest)

train <- read.csv("train.csv")
test <- read.csv("test.csv")
test$Survived <- NA

## SPLIT DATA IN TRAIN, TEST AND VALIDATION ####################################
splitData <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  
  trainindex <- sample(index, trunc(length(index)*.8))
  
  trainset <- dataframe[trainindex, ]
  
  subindex <- 1:nrow(trainset)
  validationindex <- sample(subindex, trunc(length(subindex)*.8))
  
  validationset <- dataframe[-validationindex, ]
  trainset <- trainset[validationindex, ]
  
  
  testset <- dataframe[-trainindex, ]
  list(train=trainset, test=testset, validation=validationset)
}

combined <- rbind(train, test)

combined$Name <- as.character(combined$Name)
combined$Title <- sapply(combined$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combined$Title <- sub(' ', '', combined$Title)
combined$Title[combined$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combined$Title[combined$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combined$Title[combined$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combined$Title <- factor(combined$Title)
combined$FamilySize <- combined$SibSp + combined$Parch + 1
combined$Surname <- sapply(combined$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combined$FamilyID <- factor(paste(as.character(combined$FamilySize), combined$Surname, sep=""))
## TO CALCULATE SOME MISSING VALUES IN A BETTER WAY
combined <- combined[order(combined$Surname, na.last=FALSE) , ]

combined$Age <- na.spline(combined$Age)
combined$Fare <- na.spline(combined$Fare)

## REORDER SURVIVED NA AT LAST
combined <- combined[order(combined$Survived, na.last=TRUE) , ]

## RPART
#fit <- rpart(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Title + FamilySize + Embarked, data=combined, method="class")

#RandomForest FTW
full_data_combined = combined[1:891,]
split_data <- splitData(full_data_combined)
split_data$train

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Title + FamilySize + Embarked, data=full_data_combined, importance=TRUE, ntree = 2500)

prediction_test <- predict(fit, split_data$test, type = "class")
dt_test = data.frame(PassengerId = split_data$test$PassengerId, 
                     Survived_P = prediction_test, 
                     Survived = split_data$test$Survived, 
                     Correct = (split_data$test$Survived == prediction_test))


clean_test <- combined[892:1309,]
prediction <- predict(fit, clean_test, type = "class")
length(prediction)

result <- data.frame(PassengerId = clean_test$PassengerId, Survived = prediction)
View(result)
write.csv(result, file = "titanic-brolesi.csv", row.names = FALSE)

