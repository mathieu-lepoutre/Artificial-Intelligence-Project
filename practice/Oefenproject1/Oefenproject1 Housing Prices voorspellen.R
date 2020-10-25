
library(tidyverse) 
library(rpart) 
library(randomForest) 
library(readr)
library(modelr)
options(warn=-1)



#Huis prijzen voorspellen adhv data, met decision tree en uiteindelijk met
#random forest omdat dit accurater is.



housing_data <-read_csv("train.csv")
housing_data_test <- read_csv("test.csv")



summary(housing_data)

# Ik wil de prijs voorspellen, dit doe ik door rpart te gebruiken op enkele predictors. Zo is een huis met 2 slaapkamers duurder als zonder


names(housing_data)

# Decision tree trainen 

Tree <- rpart(SalePrice ~ LotArea + YearBuilt + BedroomAbvGr + TotRmsAbvGrd + FullBath , data = housing_data)

plot(Tree, uniform = TRUE)
text(Tree, cex=.6)

#Enkele huis prijzen voorspellen van de trainingsdata

print("Huisprijzen voorspellen van eerste 5 huizen")

print("echte prijzen")
print(head(housing_data$SalePrice))

print("voorspellingen")
print(predict(trees, head(housing_data)))

#Nakijken of ons model goed is door de Mean Absolute Error te nemen.
# Hier wordt de echte prijs min de voorspelde prijs genomen (absoluut) en dan het gemiddelde ervan berekent

mae(model = Tree, data = housing_data)

#Voorspelling klopt maar dit is op getrainde data, hoe springt het model om met nieuwe data?

Tree2 <- rpart(SalePrice ~ LotArea + YearBuilt + BedroomAbvGr + TotRmsAbvGrd + FullBath, data = housing_data_test)
mae(model = Tree2, data = housing_data_test)


#Random Forest(doet weg met overfitting en underfitting problemen van decision trees), random forest gaat veel trees gebruiken en een voorspelling doen door het berekenen van het gemiddelde
#van de voorspellingen van elk component van een tree


TreeRandomForest <- rpart(SalePrice ~ LotArea + YearBuilt + BedroomAbvGr + TotRmsAbvGrd + FullBath , data = housing_data)
mae(model = TreeRandomForest, data=housing_data_test)
