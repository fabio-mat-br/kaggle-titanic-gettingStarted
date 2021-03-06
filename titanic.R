#based on https://www.kaggle.com/c/titanic-gettingStarted
#https://github.com/codebender/kaggle-titanic-r
#https://github.com/denniskorablev/Titanic-Kaggle
#VARIABLE DESCRIPTIONS:
#  survival        Survival
#(0 = No; 1 = Yes)
#pclass          Passenger Class
#(1 = 1st; 2 = 2nd; 3 = 3rd)
#name            Name
#sex             Sex
#age             Age
#sibsp           Number of Siblings/Spouses Aboard
#parch           Number of Parents/Children Aboard
#ticket          Ticket Number
#fare            Passenger Fare
#cabin           Cabin
#embarked        Port of Embarkation
#(C = Cherbourg; Q = Queenstown; S = Southampton)
#
#SPECIAL NOTES:
#  Pclass is a proxy for socio-economic status (SES)
#1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower
#
#Age is in Years; Fractional if Age less than One (1)
#If the Age is Estimated, it is in the form xx.5
#
#With respect to the family relation variables (i.e. sibsp and parch)
#some relations were ignored.  The following are the definitions used
#for sibsp and parch.
#
#Sibling:  Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard Titanic
#Spouse:   Husband or Wife of Passenger Aboard Titanic (Mistresses and Fiances Ignored)
#Parent:   Mother or Father of Passenger Aboard Titanic
#Child:    Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic#

#Other family relatives excluded from this study include cousins,
#nephews/nieces, aunts/uncles, and in-laws.  Some children travelled
#only with a nanny, therefore parch=0 for them.  As well, some
#travelled with very close friends or neighbors in a village, however,
#the definitions do not support such relations.

# SETUP: INSTAL REQUIRED PACKAGES ############################################
#install.packages('rattle', 'rpart.plot')
#install.packages('RColorBrewer')
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
#install.packages('randomForest')
#install.packages('party')
#install.packages('Amelia')

#SETUP: LOAD LIBRARIES #########################################################
#library(Amelia)
#library(rattle)
#library(rpart.plot)
#library(RColorBrewer)
#library(rpart)

#missmap(test, main="Titanic Training Data - Missings Map", 
#        col=c("yellow", "black"), legend=FALSE)

# SETUP: LOAD DATA #############################################################
setwd("C:/Users/Suzana e Fabio/Fabio/specialization/kaggle-titanic-gettingStarted")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

# HANDLING DATA ################################################################
# normalizing train and test data (adding a column Survived to test)
test$Survived <- NA

# create a full data.table
full <- rbind(train, test)

# Because the csv wasn't load with stringsAsFactors=FALSE
full$Name <- as.character(full$Name)

# get (an normalize) the peope titles
full$Title <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
full$Title <- sub(' ', '', full$Title)
full$Title[full$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
full$Title[full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
full$Title[full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# title is a factor, a categorized data
full$Title <- factor(full$Title)

full$FamilySize <- full$SibSp + full$Parch + 1

full$Surname <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
full$FamilyID <- paste(as.character(full$FamilySize), full$Surname, sep="")
full$FamilyID[full$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(full$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
full$FamilyID[full$FamilyID %in% famIDs$Var1] <- 'Small'
full$FamilyID <- factor(full$FamilyID)
