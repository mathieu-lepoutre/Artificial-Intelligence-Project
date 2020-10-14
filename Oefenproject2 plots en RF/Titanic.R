#Oefening op ggplots om uit te zoeken welke mensen met welke variabelen de titanic crash hebben overleefd

library(readr)
library(ggplot2)
library("randomForest")

#Data import en twee delen splitsen, dit voor ons voorspellingsmodel te trainen

titanic <-read.csv("Titanic.csv")

titanic.train <- titanic[titanic$IsTrainSet == TRUE,]
titanic.test <- titanic[titanic$IsTrainSet == FALSE,]

str(titanic.train)

#Survived kolom as factor zetten

titanic.train$Survived <- as.factor(titanic.train$Survived)
str(titanic.train)

#Data visualisatie, enkele variabelen dat me interessant lijken te visualiseren

#plot 1
countSexPlot <- ggplot(data=titanic.train, aes(x=Sex,fill=Survived)) + geom_bar()+
  theme_grey(base_size = 14)
countSexPlot

#plot 2

countPclassPlot <- ggplot(data=titanic.train, aes(x=Pclass,fill=Survived)) + geom_bar()+
  theme_grey(base_size = 14)
countPclassPlot

#plot 3

CountEmbarkedPlot  <- ggplot(data=titanic.train, aes(x=Embarked,fill=Survived)) + geom_bar() +
  theme_grey(base_size = 14)
CountEmbarkedPlot

#plot 4
Ageplot <- ggplot(data=titanic.train, aes(x=Age)) + geom_density()
Ageplot

#plot 5
FarePlot <- ggplot(data=titanic.train, aes(x=Fare)) + geom_density()+
  facet_grid(~Pclass)
FarePlot


#Jitterplot
Jitterplot <- ggplot(data=titanic.train, aes(x=Pclass,y=Fare,
                                             color=Survived))+ geom_jitter() + ggtitle("Titanic Survival by Class")

Jitterplot



#Voorspellingsmodellen
#Random forest predictive model


survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)


titanic.model <- randomForest(formula=survived.formula,
                              data=titanic.train,
                              ntree = 500,
                              mtry = 3,
                              nodesize = 0.01*nrow(titanic.train)
)

Survived <- predict(titanic.model,newdata=titanic.test)




####
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)

output.df$Survived <- Survived

output.df
