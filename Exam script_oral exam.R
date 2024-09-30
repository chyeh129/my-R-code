### Online exam script - BAS110 ###

## R preparation

getwd()

options(scipen=999)


######################################################

### House price dataset ###

## Load data

load('house_price_exam.Rdata')

View(dat1)

## Dataset

# Age = House age
# Distance = Distance to nearest MRT station
# Store = Number of convenience stores within 1 km
# Latitude = latitude
# Longitude = longitude
# Price = house price of unit area

## Descriptive analysis

str(dat1)
summary(dat1)

library("psych")
describe(dat1)

summary(dat1$Age)
length(dat1$Age)
mean(dat1$Age)

## Boxplot

boxplot(dat1$Age,main="Boxplot of house age", xlab="x-axis", ylab="Age", col="lightgrey")
boxplot(dat1$Price,main="Boxplot of house price", xlab="x-axis", ylab="Age", col="lightgrey")

## Correlation & covariance

cor(dat1)
round(cor(dat1), digits=2)

cor(dat1$Age, dat1$Price)

cov(dat1)

## Normality of Y

shapiro.test(dat1$Price)

## Linear model

LM1<-lm(Price~Age+Distance+Store+Latitude+Longitude, data=dat1)
summary(LM1)

library("jtools")
summ(LM1)

coef(LM1)

confint(LM1, 'Age', level=0.95)

## VIF

library(car)
vif(LM1)

## Heteroscedasticity

# Breusch Pagan Test
library("lmtest")
bptest(LM1)

# White test

library("skedastic")
white_lm(LM1, interactions = TRUE)

## Autocorrelation

# Breusch-Godfrey LM Test
bgtest(LM1, order=1)

#  Durbin-Watson test 
dwtest(LM1)


rm(dat1, LM1)

######################################################

### Berlin Neighborhood data ###

## Load data

load('Berlin_neighborhood_exam.Rdata')

View(dat2)

## Dataset:

# Pop = Total population
# Pop_change = % change in population over past several years
# Pop_18 = % of children (under18) in population
# Lunch = % free school lunch particpation
# Income_change = % change in household income over past several years
# Pop_crime = Crime rate per 1000 population
# Crime_change = % change in crime rate over past several years

## Descriptive analysis

str(dat2)
summary(dat2)

library("psych")
describe(dat2)

summary(dat2$Lunch)
length(dat2$Lunch)
mean(dat2$Lunch)

## Boxplot

boxplot(dat2$Pop_change,main="Boxplot of % change in population over past several years", xlab="x-axis", ylab="y-axis", col="lightgrey")
boxplot(dat2$Pop_crime, main="Boxplot of Crime rate per 1000 population", xlab="x-axis", ylab="y-axis", col="lightgrey")

## Correlation & covariance

cor(dat2)
round(cor(dat2), digits=2)

cor(dat2$Pop_change, dat2$Pop_crime)

cov(dat2)

## Normality of Y

shapiro.test(dat2$Pop_crime)

## Linear model

LM2<-lm(Pop_crime~Pop+Pop_change+Pop_18+Lunch+Income_change+Crime_change, data=dat2)
summary(LM2)

library("jtools")
summ(LM2)

coef(LM2)

confint(LM2, 'Pop_change', level=0.95)

## VIF

library(car)
vif(LM2)

## Heteroscedasticity

# Breusch Pagan Test
library("lmtest")
bptest(LM2)

# White test

library("skedastic")
white_lm(LM2, interactions = TRUE)

## Autocorrelation

# Breusch-Godfrey LM Test
bgtest(LM2, order=1)

#  Durbin-Watson test 
dwtest(LM2)


rm(dat2, LM2)



######################################################

### Health data ###

## Load data

load('Health_exam.Rdata')

## Dataset

# Death = "death rate per 1000 residents"                   
# Doc = "doctor availability per 100,000 residents"       
# Hospital =  "hospital availability per 100,000 residents"     
# Income = "annual per capita income in thousands of dollars"
# Population = "population density people per square mile"

View(dat3)

## Descriptive analysis

str(dat3)
summary(dat3)

library("psych")
describe(dat3)

summary(dat3$Death)
length(dat3$Death)
mean(dat3$Death)

## Boxplot

boxplot(dat3$Death,main="Death rate per 1000 residents", xlab="x-axis", ylab="y-axis", col="lightgrey")
boxplot(dat3$Hospital, main="Boxplot of hospital availability per 100,000 residents", xlab="x-axis", ylab="y-axis", col="lightgrey")

## Correlation & covariance

cor(dat3)
round(cor(dat3), digits=2)

cor(dat3$Death, dat3$Hospital)

cov(dat3)

## Normality of Y

shapiro.test(dat3$Death)

## Linear model

LM3<-lm(Death~Doc+Hospital+Income+Population, data=dat3)
summary(LM3)

library("jtools")
summ(LM3)

coef(LM3)

confint(LM3, 'Doc', level=0.95)

## VIF

library(car)
vif(LM3)

## Heteroscedasticity

# Breusch Pagan Test
library("lmtest")
bptest(LM3)

# White test
library("skedastic")
white_lm(LM3, interactions = TRUE)

## Autocorrelation

# Breusch-Godfrey LM Test
bgtest(LM3, order=1)

#  Durbin-Watson test 
dwtest(LM3)


rm(dat3, LM3)

######################################################

### Farmland data ###

## Load data

load('Farmland_sales_exam.Rdata')

## Dataset

# "Price" = The final price in EUR / m²
# "Lot_size" = The size of the lot in m²
# "Lot_Quality" = Soil quality index 0 - 104, low values indicate bad soils
# "Seller" = A dummy variable if the BVVG is the seller (the German state)
# "Lot_Leased" = A dummy if the lot is leased to a farmer at the point of sale
# "City" = A continuous variable giving the distance to the next large city.

View(dat4)

## Descriptive analysis

str(dat4)
summary(dat4)

library("psych")
describe(dat4)

summary(dat4$Price)
length(dat4$Price)
mean(dat4$Price)

## Boxplot

boxplot(dat4$Price,main="Price", xlab="x-axis", ylab="y-axis", col="lightgrey")
boxplot(dat4$Lot_size, main="Lot size", xlab="x-axis", ylab="y-axis", col="lightgrey")

## Correlation & covariance

cor(dat4[,c(1,2,3,6)])
round(cor(dat4[,c(1,2,3,6)]), digits=2)

cov(dat4[,c(1,2,3,6)])

## Normality of Y

shapiro.test(dat4$Price)

## Linear model

LM4<-lm(Price~Lot_size+Lot_Quality+City, data=dat4)
summary(LM4)

library("jtools")
summ(LM4)

coef(LM4)

confint(LM4, 'City', level=0.95)

## VIF

library(car)
vif(LM4)

## Heteroscedasticity

# Breusch Pagan Test
library("lmtest")
bptest(LM4)

# White test
library("skedastic")
white_lm(LM4, interactions = TRUE)

## Autocorrelation

# Breusch-Godfrey LM Test
bgtest(LM4, order=1)

#  Durbin-Watson test 
dwtest(LM4)

## Linear model with dummy

LM5<-lm(Price~Lot_size+Lot_Quality+City+Lot_Leased, data=dat4)
summary(LM5)

library("jtools")
summ(LM5)

rm(dat4, LM4, LM5)

######################################################

### Pig data data ###

## Load data

load('EmpMeth_pigdata_exam.Rdata')

## Dataset

#"Profit" = As the name says, measured in Euro per animal (its actually not profit, but "Direktkostenfreie Leistung)
#"Fattening_places" = Size of the farm
#"Fattening_sqm" = Average m² per animal
#"Weight" = Weight of the animal before they are delivered down the value chain.
#"Initiative" = Dummy for participation in the Initiative Tierwohl
#"Health" = Average health index of the animals, scaling is 1 to 100 with 100 indicating a perfectly healthy animal.

View(dat5)

## Descriptive analysis

str(dat5)
summary(dat5)

library("psych")
describe(dat5)

summary(dat5$Profit)
length(dat5$Profit)
mean(dat5$Profit)

## Boxplot

boxplot(dat5$Profit,main="Profit", xlab="x-axis", ylab="y-axis", col="lightgrey")
boxplot(dat5$Health, main="Health Index", xlab="x-axis", ylab="y-axis", col="lightgrey")

## Correlation & covariance

cor(dat5[,c(1,2,3,4,6)])
round(cor(dat5[,c(1,2,3,4,6)]), digits=2)

cov(dat5[,c(1,2,3,4,6)])

## Normality of Y

shapiro.test(dat5$Profit)

## Linear model

LM6<-lm(Profit~Fattening_places+Fattening_sqm+Weight+Health, data=dat5)
summary(LM6)

library("jtools")
summ(LM6)

coef(LM6)

confint(LM6, 'Health', level=0.95)

## VIF

library(car)
vif(LM6)

## Heteroscedasticity

# Breusch Pagan Test
library("lmtest")
bptest(LM6)

# White test
library("skedastic")
white_lm(LM6, interactions = TRUE)

## Autocorrelation

# Breusch-Godfrey LM Test
bgtest(LM6, order=1)

#  Durbin-Watson test 
dwtest(LM6)

## Linear model with dummy

LM7<-lm(Profit~Fattening_places+Fattening_sqm+Weight+Health+Initiative, data=dat5)
summary(LM7)

library("jtools")
summ(LM7)

rm(dat5, LM6, LM7)


