# Titanic Survival

# VARIABLE DESCRIPTIONS:
# survival        Survival(0 = No; 1 = Yes)
# pclass          Passenger Class(1 = 1st; 2 = 2nd; 3 = 3rd)
# name            Name
# sex             Sex
# age             Age
# sibsp           Number of Siblings/Spouses Aboard
# parch           Number of Parents/Children Aboard
# ticket          Ticket Number
# fare            Passenger Fare
# cabin           Cabin
# embarked        Port of Embarkation(C = Cherbourg; Q = Queenstown; S = Southampton)

# Read the dataset
rm(list=ls())
setwd("C:/Users/Deepak/Documents/Analytics/Kaggle/Titanic")
titanic <- read.csv("train.csv",stringsAsFactors = FALSE)
cat("The training dataset has",nrow(titanic),"observations")
str(titanic)
attach(titanic)

# Converting Pclass into factor and ordering them
# Pclass is a proxy for socio-economic status (SES)
# 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower
Pclass <- factor(Pclass,levels = c(3,2,1),ordered = TRUE)
str(Pclass)

# Checking the number of people survived
table(Survived)

# proportion of people survived?
prop.table(table(Survived)) 

## 62% people died in the training dataset. 
## hence the baseline model would be to assume that everyone dies
## We will try to build a model which beats this base model

############################-----------------------------------------############################

# Gender

# Checking the people survived based on gender
prop.table(table(Sex,Survived)) 
# this divides each entry with the total number of people. not very good. Lets see the row wise proportion
prop.table(table(Sex,Survived),1)
# This shows that ost of the females, nearly 3/4ths, onboard survived and very few males survived
# hence we can say that all females survived as an improvement over our baseline model

# Age

# Checking Age as a feature for improving the model
summary(Age)
# There are lot of missing values. lets add the average age to all the missing values
meanAge <- mean(Age,na.rm = T)
Age[is.na(Age)] <- meanAge
# Since Age is a continuous variable, checking for proportions for each Age is not very effective
# Hence we create 2 Age groups -> <18 and >18. 
titanic$Child <- 0
titanic$Child[Age<18] <- 1
# Create table of Age along with Gender to see the survival proportions
# prop. table does not work. Gives columns 0,1,2 and rows with male and female. use aggregate function
aggregate(Survived ~ Child + Sex, data=titanic, FUN=sum) # Gives the number of 0s and 1s
aggregate(Survived ~ Child + Sex, data=titanic, FUN=length) # Gives the number of obs in the data
aggregate(Survived ~ Child + Sex, data=titanic, FUN=function(x){sum(x)/length(x)}) # gives prop
# Again we see that females are more likely to survive. Age grouping has not made much change
# Male children have 40% chances of surviving

# Fare

# Fare is again a continuous variable, hence we divide it into 4 groups
# Group 1: Fare < $10
# Group 2: fare between $10 to $20
# Group 3: Fare between $20 to $30
# group 4: Fare > $30

titanic$fareGrp[Fare <10] <- "Grp1"
titanic$fareGrp[Fare>=10 & Fare<20] <- "Grp2"
titanic$fareGrp[Fare>=20 & Fare<30] <- "Grp3"
titanic$fareGrp[Fare>=30] <- 'Grp4'

aggregate(Survived ~ Child + Sex + fareGrp, data=titanic, FUN=function(x){sum(x)/length(x)}) # gives prop
# Grp2 male child has nearly 73% chance of survival

# Pclass

# Use class also for building model
aggregate(Survived ~ Child + fareGrp + Pclass + Sex, data=titanic, FUN=function(x){sum(x)/length(x)}) # gives prop
# Females in Grp3 and Grp4 have low survival rate!!!! So if the fare they paid was greater than $20,
# the chances of them surviving are less
# Males still have no much chance

############################-----------------------------------------############################


