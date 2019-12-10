####Part 1####
library(readr)
test <- read_csv("test.csv")
library(readr)
train <- read_csv("train.csv")
# Adding a "Survived" Variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# Combining Data Sets by Rows
data.combined <- rbind(train, test.survived)

#Fixing the Columns in order to have two datasets matching
test.survived <- test.survived[,c(1,2,3,4,5,6,7,8,9,10,11,12)]

#Figuring the Data Types
str(data.combined)

#Changing PClass data from int to factor
data.combined$Pclass <- as.factor(data.combined$Pclass)

#Changing Survived Data from integer to factor
data.combined$Survived <- as.factor(data.combined$Survived)

#Changing Name data from Chr to factor
data.combined$Name <- as.factor(data.combined$Name)

#Changing other Chr data to factor
data.combined$Sex <- as.factor(data.combined$Sex)
data.combined$Ticket <- as.factor(data.combined$Ticket)
data.combined$Cabin <- as.factor(data.combined$Cabin)
data.combined$Embarked <- as.factor(data.combined$Embarked)

#Gross Survival Rates
table(data.combined$Survived)

#Distribution Across Classes
table(data.combined$Pclass)

#ggplot2 package for visualizations
library(ggplot2)

#Coming up with Hyphotesis: Rich people survived at a higher rate
##Changing Pclass data from int to Factor in Train DataSet
train$Pclass <- as.factor(train$Pclass)
##Creating a New ggpolt
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar(width = 0.5) +
  xlab("Pclass") + 
  ylab("Total Count") + 
  labs(fill = "Survived")

#Examine the first few names in the trainind data set
head(as.character(train$Name))

#How many uique names are there across both tain & test?
length(unique(as.character(data.combined$Name)))
#It seems like there are some duplicate names...
#Find out the duplicate names and store them as a vector
dup.names <- data.combined[duplicated(data.combined$Name),]

#Find the duplicate names in the combined data set
data.combined[data.combined$Name %in% dup.names,]
data.combined[which(data.combined$Name %in% dup.names),]
### Something wrong, does not match with the tutorial - Moving on

#Tryging to find correlation beteen "Miss" and "Mr"
library(stringr)
##Any correlation with other variables (eg., sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),] 
misses[1:5,]

#Hyphotesis: Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mrses[1:5,]
##Check out males to see if pattern continues
males <- data.combined[which(train$Sex == "male"), ]
males[1:5,]

#Expanding upon the relationship between "Survived" and "Pclass" by adding the new title variable
# data set then explore a potential 3-dimensional relationship
##Create a utitlity function to help with title extraction
extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if (length(grep("Miss.", Name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
    return("Master.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return("Mr.")
  }  else {
      return("Other")
  }
}

  
  titles <- NULL
  for(i in 1:nrow(data.combined)) {
    titles <- c(titles, extractTitle(data.combined[i,"Name"]))
  }
  data.combined$title <- as.factor(titles)

  ###Since we only have survived labels for the train set,
  #only use the first 891 rows
  ggplot(data.combined[1:891,], aes(x= title, fill = Survived)) +
    stat_count(width = 0.5) +
    facet_wrap(~Pclass) +
    ggtitle("Pclass") +
    xlab("Title") +
    ylab("Total Count") +
    labs(fill = "Survived")

####Part 2####
  
#What is the distribution of females to males across train & test?
table(data.combined$Sex)

#Visualize the 3-way relationship of sex, plcass, and survival
# compare to analyse
  ggplot(data.combined[1:189,], aes(x=Sex, fill=Survived)) +
    stat_count(width = 0.5) +
    facet_wrap(~Pclass) +
    ggtitle("Pclass") +
    xlab("Sex") +
    ylab("Total Count") +
    labs(fill = "Survived")
  
#Exploring the distributions of age over entire data set
  summary(data.combined$Age)
  summary(data.combined[1:891, "Age"])

#Visualizing survival rates broken out by; sex, pclass, age
  ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
    facet_wrap(~Sex + Pclass) +
    geom_bar(width = 10) +
    xlab("Age") +
    ylab("Total Count")
  
#Validate that "Master" title is a proxy for male children
  boys <- data.combined[which(data.combined$title == "Master."),]
  summary(boys$Age)
  
#"Miss" is a more complicated proxy
  misses <- data.combined[which(data.combined$title == "Miss."),]
  summary(misses$Age)
## Visualizing "Miss"
  ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
    facet_wrap(~Pclass) +
    geom_bar(width = 5) +
    ggtitle("Age for 'Miss.' by Pclass") +
    xlab("Age") +
    ylab("Total Count")
  
# Female Children may have different survival rate
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

#Summarizing Sibsp variable (Number of Sibling & Spouse)
summary(data.combined$SibSp)

#Can we treat sibsp as a factor?
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

# Title might be predictive. Visualizing survival rates by sibsp, pclass, and title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  xlab("Sibsp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#Treat the parch variable as a factor and visualize
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Cout") +
  ylim(0,300) +
  labs(fill = "Survived")

# Creating a family size feature.
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

#Visualize family size to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
####Tutorial 3

####Part 3####
# Ticket Variable
str(data.combined$Ticket)

#Becuase there are so many number of levels, ticket is not a factor but a string variable
#Convert ticket variable to sting
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

#There is no structure in Ticket data
#Take a look a t just the first character of each
ticket.first.char <- ifelse(data.combined$Ticket ==""," ", substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)

#We can make a factor for analysis purposes and visualize
data.combined$ticket.first.char <- as.factor(ticket.first.char)

#High-level pot of data for starter
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

#Ticket seems to be predictive, go even deeper
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("PcLass") +
  xlab("ticket.first.char") +
  ylab("Total Cout") +
  labs(fill = "Survived")

#Lets see if there is a pattern when using combination of pclass  & tittle
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Fares that Titanic passengers paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))

#Fare cant be a factor, treat it as numeric and visualize
ggplot(data.combined, aes(x = Fare)) +
  geom_bar(width = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count")

#Check whether fare has a predictive power
ggplot(data.combined[1:891,], aes(x = Fare, Fill = Survived)) +
  geom_bar(width = 5) +
  facet_wrap(~Pclass + title) +
  xlab("Fare") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Analysis of the cabin variable
str(data.combined$Cabin)

#Cabin cannot be a factor since it has 186 levels, make it a sting
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

#Replaceing NA cabins with a "U"
data.combined$Cabin[is.na(data.combined$Cabin)] <- "U"
data.combined$Cabin[1:100]

#Take a lookat the first character as a factor
cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)

#Add to combined dataset
data.combined$cabin.first.char <- cabin.first.char

#High level plot
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Cabin seems to be predictive, go even deeper
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")
  
#Does this feature improve upon pclass + title?
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  labs(fill = "Survived")

#What about passengers with multiple cabins?
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Does survivability depend on where you got onboard the Titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)

#Plot tdata for analysis
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Embarked") +
  ylab("Total Count") +
  labs(fill = "Survived") 
####Part 4####
library(randomForest)

#Train a Random Forest with the default parameters using Pclass & Title
rf.train.1 <- data.combined[1:891, c("Pclass", "title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

#Train a Random Forest using Pclass, Title & Sibsp
rf.train.2 <- data.combined[1:891, c("Pclass", "title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

# Train a Random Forest using Pclass, Title & Parch
rf.train.3 <- data.combined[1:891, c("Pclass", "title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

# Train a Random Forest using Pclass, Title, SibSpv& Parch
rf.train.4 <- data.combined[1:891, c("Pclass", "title", "SibSp", "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

# Train a Random Forest using Pclass, Title & family.size
rf.train.5 <- data.combined[1:891, c("Pclass", "title", "family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

# Train a Random Forest using Pclass, Title, SibSp & family.size
rf.train.6 <- data.combined[1:891, c("Pclass", "title","SibSp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)

# Train a Random Forest using Pclass, Title, Parch & family.size
rf.train.7 <- data.combined[1:891, c("Pclass", "title", "Parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)


####Part 5####
#Before jumping into feature engineering, we need to establish a methodology
#for estimating our error rate on the test set. Starting by submittin rf.5 to 
#Kaggle to see if OOB error estimate is accurate.

#Subset test records and features
test.submit.df <- data.combined[892:1309, c("Pclass", "title", "family.size")]

#Make predictions 
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

#Write out a csv file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "TitanicSubmission1.csv", row.names = FALSE)

#Submission scored 0.79425 although rf.5 OOB predicted error rate of 18.18%
#which should get us a score of 0.8182
