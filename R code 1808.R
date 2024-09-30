curve(4*x^2+3, 0, 100)
curve(4*x^2+3, -1, 1)

x <- c(1, 3, 5, 7, 9)
x
is.atomic(x) || is.list(x)

#assigning" the name x to the list of numbers
assign("x", c(1, 3, 5, 7, 9))
length(x)

#adding an 8 between numbers 7 and 9
x <- c(x[1:4], 8, x[5])

# extract elements 2 and 4, which correspond to numbers 3 and 7 in the vector
x[c(2, 4)]

#exclude the last element of x, which corresponds to the number 9
x[-6]

#remove elements 2 to 4 inclusive
x[-2:-4]

#cumulative sum of a vector
x <- c(1, 3, 5, 7, 9)
cumsum(x)

#dot product: sum of each element multiplied pairwise
x <- c(1, 3, 5, 7, 9)
y <- c(2, 4, 6, 8, 10)
x%*%y
#To demonstrate how this was computed, we compute it the long way
dot.product <- 1*2 + 3*4 + 5*6 + 7*8 + 9*10
dot.product


#reate a matrix with numbers 1 through 8, having 4 rows and 2 columns
S <- matrix(1:8, 4, 2)
S

#the following asks for a matrix with values 20 through 29, but only specifies a 2 × 2 matrix
S <- matrix(20:29, 2, 2)
S

#dimension of a matrix is the number of rows by the number of columns
dim(S)

#we can also index matrices.For example, suppose for our matrix S we wished to extract the second column
S[,2]

#We first define each row by "concatenating" elements using c(). Then, we name the object cormatrix, and bind the columns of the matrix together using cbind()
c1 <- c(1.000, 0.343, 0.505, 0.308, 0.693, 0.208, 0.400, 0.455)
c2 <- c(0.343, 1.000, 0.203, 0.400, 0.187, 0.108, 0.386, 0.385)
c3 <- c(0.505, 0.203, 1.000, 0.398, 0.303, 0.277, 0.286, 0.167)
c4 <- c(0.308, 0.400, 0.398, 1.000, 0.205, 0.487, 0.385, 0.465)
c5 <- c(0.693, 0.187, 0.303, 0.205, 1.000, 0.200, 0.311, 0.485)
c6 <- c(0.208, 0.108, 0.277, 0.487, 0.200, 1.000, 0.432, 0.310)
c7 <- c(0.400, 0.386, 0.286, 0.385, 0.311, 0.432, 1.000, 0.365)
c8 <- c(0.455, 0.385, 0.167, 0.465, 0.485, 0.310, 0.365, 1.000)
cormatrix <- cbind(c1, c2, c3, c4, c5, c6, c7, c8)
cormatrix

#The sum of values along the main diagonal of the matrix is referred to as the trace of the matrix, which we can compute as tr() in R.
library(psych)
tr(cormatrix)

#The dimension of the matrix should equal 8 by 8
dim(cormatrix)

#there are only values of 1 along the main diagonal using the diag() function, which will reproduce the values on that main diagonal
diag(cormatrix)

#compute the inverse of a matrix using the function solve()
I <- solve(cormatrix)
I

#compute the determinant in R quite easily, using the det() function
det(cormatrix)

#demonstrate the extraction of eigenvalues and eigenvectors:
eigen(cormatrix)

#this first eigenvector must correspond to the first eigenvalue: by multiplying the eigenvector by the correlation matrix to get the left-hand side of the equation, then multiplying the eigenvector by the eigenvalue to get the right-hand side.
eigen.vector <- c(-0.4125682, -0.3034726, -0.3180940, -0.3730602, -0.3572744, -0.3008318, -0.3664721, -0.3806410)

#multiply this vector by cormatrix which corresponds to matrix A:
Ax = cormatrix%*%eigen.vector
Ax

#Getting data into R
quant <- c(5, 2, 6, 9, 8, 7, 9, 10, 10)
quant
verbal <- c(2, 1, 3, 7, 9, 8, 8, 10, 9)
train <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)

#we are ready to join them into a data frame by two ways: cbind() ("column bind") andgenerated the data frame by reference to the function data.frame() .
iq.train <- cbind(quant, verbal, train)
iq.train
iq.train <- data.frame(quant, verbal, train)

#read the data file through read.table().
iq.train <- read.table("iqtrain.txt", header = T)
iq.train


#Installing R package
install.packages("car")
library(car)
install.packages(c("car", "MASS"))
library(help = car)

# Select Subsets from a Dataframe
names(iq.train)
iq.train[1:2]  #extract variables quant and verbal
vq <- iq.train[1:2] # 
vq

library(dplyr) 
select(iq.train, quant, verbal)  
iq.train$quant   #we wanted the quant variable only,
iq.train[iq.train$quant > 8,] #select only those cases having quant scores greater than 8
filter(iq.train, quant == 10, verbal == 10)   #select cases or groups of cases

(iq.train.q10.v10 <- filter(iq.train, quant == 10, verbal == 10))  #Notice above we had to call the new object so that it prints. That is, we had to specifically code iq.train.q10.v10 so that the new object is produced

#Deals with Missing Data
v <- c(1:9, NA)  #NA is "not available"
object <- is.na(v)  #.na() to tell us for which values of the vector there are missing values:
object

v <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, NA)
mean(v)
mean(v, na.rm=T)  #removes the missing value. We get a mean of values 1 through 9, ignoring the missing element in position 10:

#observe all the current objects in R's environment
ls()
rm(x, y) #We can use the function rm() to remove objects.
rm(list=ls()) #If we chose to remove all objects at the same time

#creating factor in R
v <- c(0, 1, 2, 3, 4)
v
v.f <- factor(v)
v.f
mean(v)
mean(v.f)  #argument is not numeric or logical:returning NA
is.factor(v.f) #confirm for us that v.f is now a factor, and that v is not
is.factor(v)

levels(v.f)  #request the levels of the factor versus the original variable
levels(v)
nlevels(v.f )
nlevels(v)  #v.f has 5 levels, and v has none (since it is not a factor, it does not have levels)

v.f <- factor(v, levels = c(10, 11, 12, 13, 14))
v.f 


x <- c(0, 0, 1, 2, 3)  
table(x)  # table() tells us there are two 0s, then each one value of 1,2,3
#table() function to generate the different frequencies associated with the variable

#tapply() to get specific numerical summaries for categories of a factor
tapply(children, gender, mean) # reports the mean of children for each category of gender.

library(tidyverse) #contains ggplot2 as well as a few other packages useful for data analysis in R 

#plotting graph
x <- c(1, 3, 5, 7, 9)
y <- c(2, 4, 6, 8, 10)
plot(x, y)
lines(x, y)

#generating 1000 random observations in each vector
z = rnorm(1000)
w = rnorm(1000)
library(car)
some(z)
some(w)

plot(z)
lines(z)
hist(z)  #histogram of z
rug(z)   #rug plot

plot(w, z)
plot(w, z, main = "Plot of W and Z", xlab="z for absicca axis", ylab="w for ordinate axis",pch = 19)
# giving them names via xlab and ylab statements, as well as adjust pch, which is the plotting character:


#Assessing Normality
qqnorm(z); qqline(z)
shapiro.test(z)

#boxplot
boxplot(parent, main = "Boxplot of Parent Height")
library(lattice)
bwplot(child, main = "Boxplot of Child Height")

pnorm(0) # for z = 0, the density above this value is half of the normal curve, giving a value of 0.5
1-pnorm(3.74)   #the area above 3.74


#Plotting Normal Distributions
x <- seq(-5, 5, length = 100)
norm <- dnorm(x)
plot(norm)

#Correlation coefficient
cor(parent, child)
cor.test(parent, child, method = "pearson")  

bill.fav <- c(10.0, 9.5, 8.4, 7.6, 2.1)
mary.fav <- c(9.7, 9.6, 9.0, 8.5, 7.6)
cor(bill.fav, mary.fav, method = "pearson")
cor(bill.fav, mary.fav, method = "spearman")

grade <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
study.time <- c(30, 25, 59, 42, 31, 140, 90, 95, 170, 120)
grade.data <- data.frame(grade, study.time)
grade.data
cor.test(grade, study.time)

#t-test
iq<- c(105, 98, 110, 105, 95)
t.test(iq, mu = 100)   #single sample t-test


#Two-Sample t-Test
grade.0 <- c(30, 25, 59, 42, 31)
grade.1 <- c(140, 90, 95, 170, 120)
t.test(grade.0, grade.1)


#t-Test via Linear Model Set-up
studytime <- c(30, 25, 59, 42, 31, 140, 90, 95, 170, 120)
grade <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
t.test(studytime ~ grade)
t.test(studytime ~ grade, var.equal = TRUE)

#Paired sample t-test
trial.1 <- c(10, 12.1, 9.2, 11.6, 8.3, 10.5)
trial.2 <- c(8.2, 11.2, 8.1, 10.5, 7.6, 9.5)
t.test(trial.1, trial.2, paired = TRUE)

#Exact binomial test
binom.test(2, 5, p = 0.5)  # probability of getting 2 heads out of 5 flips on a fair coin
binom.test(2, 5, p = 0.9)

#Categorical Data Having More Than Two Possibilities
diag.table <- matrix(c(20, 5, 10, 15), nrow = 2)
diag.table
chisq.test(diag.table, correct = F)  #chi squared test

library(psych)
phi(diag.table, digits = 3)  #As a measure of association between condition and exposure, we can compute a phi coefficient

library(vcd)
assocstats(diag.table)  #The phi coefficient could have also been obtained via the vcd package, along with a few other related statistics useful for measuring association in contingency tables:

library(vcd)
mosaic(diag.table)

library(vcd)
fourfold(diag.table)

#Radar chart
library(fmsb)
data = data.frame(matrix(sample(1:100, 10, replace = T), ncol = 10))
colnames (data) = c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "F")
data = rbind(rep(100, 10), rep(0, 10), data)
data
radarchart(data, axistype=1,
           cglcol="grey", cglty=1, axislabcol="grey",
           caxislabels=seq(0,100, 25), cglwd=0.8)

#Power Analysis and Sample Size Estimation Using R

#Power for t-tests
pwr.t.test(n = , d = , sig.level = , power = , type =
             c("two.sample", "one.sample", "paired"))
#n is sample size
#d is effect size (Cohen's d)
#sig.level is the level of significance desired for the given test (e.g. 0.05, 0.01)
#power is the level of statistical power desired type is the kind of t-test that we are running, options include the two-sample independent groups t-test, the one-sample test, and the paired-samples test.

library(pwr)
pwr.t.test(n = , d = 0.5, sig.level = 0.05, power = 0.90,
             type = "two.sample")  #Two-sample t test power calculation

pwr.t.test(n = , d = 2.0, sig.level = 0.05, power = 0.90,
           type = "two.sample")

pwr.t.test(n = , d = 0.1, sig.level = 0.05, power = 0.90,
           type = "two.sample") #Extremely Small Effect Size

pwr.t.test(n = 100, d = 0.5, sig.level = 0.05, power = ,
           type = "two.sample") #Estimating Power for a Given Sample Size

pwr.anova.test(k = , n = , f = , sig.level = , power = ) #Power for One-Way ANOVA
pwr.anova.test(k = 5, n = , f = 0.5, sig.level = 0.05,
               power = 0.90)

pwr.r.test(n = , r = , sig.level = , power = ) # Power for Correlations
pwr.r.test(n = , r = .10, sig.level = 0.05, power =0.90)
pwr.r.test(n = , r = .90, sig.level = 0.05, power =0.90)


#ANOVA
achiev <- read.table("achiev.txt", header = T)
achiev

attach(achiev)
boxplot(ac ~ teach, data = achiev, main="Achievement as
a Function of Teacher",
          + xlab = "Teacher", ylab = "Achievement")

library(FSA)
f.teach <- factor(teach)
hist(ac~f.teach, data = achiev)

#Inferential Tests for Normality
shapiro.test(ac)

#Evaluating Homogeneity of Variances: this include Fligner-Killeen test, Bartlett's test, and Levene's test.
fligner.test(ac~f.teach, data = achiev)
aggregate(ac ~ f.teach, FUN = var)
aggregate(ac ~ f.teach, FUN = mean)

#Performing the ANOVA Using aov()
f.teach <- factor(teach)
f.teach
tapply(ac, f.teach, mean)
tapply(ac, f.teach, median)
mean(ac)

#The ANOVA Analysis of Variance Summary Table
anova.fit <- aov(ac ~ f.teach, data = achiev)
summary(anova.fit)
#Obtaining Treatment Effects
model.tables(anova.fit)

#Plotting Results of the ANOVA
plot.design(ac~f.teach)

#Post Hoc Tests
anova.fit <- aov(ac~f.teach)
TukeyHSD(anova.fit)
plot(TukeyHSD(anova.fit))

# Alternative Way of Getting ANOVA Results via lm()
anova.lm <- lm(ac ~ f.teach)
summary(anova.lm)


#Factorial Analysis of Variance
head(achiev)
f.teach <- factor(teach)
f.text <- factor(text)
f.teach
f.text

fit.factorial <- aov(ac ~ f.teach + f.text + f.teach:f.
                     text, data = achiev)  #We now fit the factorial model:
summary(fit.factorial)

#We can inspect the cell means more closely in R using the phia package
library(phia)
(fit.means <- interactionMeans(fit.factorial))

interaction.plot(f.teach, f.text, ac)

plot.design(ac~f.teach + f.text + f.teach:f.text, data =
              achiev)

interactionMeans(fit.factorial, factors="f.teach")
pairwise.t.test(ac, f.teach, p.adj = "bonf")

library(phia)
interactionMeans(fit.factorial, factors="f.text")

library(phia)
plot(fit.means)

#Simple Main Effects
library(phia)
testInteractions(fit.factorial, fixed="f.teach",
                   across="f.text")

testInteractions(fit.factorial, fixed="f.text", across="f.teach")


#One-Way Random Effects ANOVA in R
library(lme4)
fit.random <- lmer(ac ~ 1 + (1|f.teach), achiev, REML = FALSE)
summary(fit.random)

fit.random.reml<-lmer(ac~1+(1|f.teach),achiev,REML=TRUE)
summary(fit.random.reml)

#Mixed Models
library(nlme)
mixed.model <- lme(ac ~ f.text, data = achiev, random =
                       ~1 | f.teach)
summary(mixed.model)


#Repeated-Measures Models
learn <- read.table("learning.txt", header = T)
learn

attach(learn)
library(ggplot2)
qplot(trial, time)

f.rat <- factor(rat)
f.trial <- factor(trial)
rm.model <- aov(time ~ f.trial + Error(f.rat/f.trial), data =
                    learn)
summary(rm.model)
learn
attach(learn)
f.treat <- factor(treat)
f.treat

rat.two.way <- aov(time~f.trial*f.treat + Error(f.rat/f.
                                                  trial), data = learn)
summary(rat.two.way)

#Simple Linear Regression

iq.data <- read.table("iq.data.txt", header = T)
some(iq.data)

attach(iq.data)
scatterplot(verbal ~ quant)


#Ordinary Least-Squares Regression OLS
ggplot(iq.data, aes(quant, verbal)) +
  + geom_point()
fit <- lm(verbal ~ quant)
fit
coef(fit)  #get the estimates; we would have gotten the same thing by coding fit$coef
summary(fit)

cor(fitted(fit), verbal)^2   #R-squared
cor(verbal, quant)^2   #R-squared; squared correlation between verbal and quant, as we can readily demonstrate
anova(fit)   #ANOVA table

# Multiple linear regression OLS
fit.mr <- lm(verbal ~ quant + analytic)
summary(fit.mr)
anova(fit.mr)  #ANOVA table
cor(verbal, fitted(fit.mr))^2   #R-squared

#Verifying Model Assumptions
library(car)
qqPlot(fit.mr)  
hist(resid(fit.mr))  
#We can verify the normality of errors assumption for our multiple regression using a Q-Q plot and a histogram, where resid(fit.mr) generates the residuals for the model object fit.mr

plot(fitted(fit.mr), resid(fit.mr))   #Homoscedasticity of Errors.
# To verify the assumption, it suffices to plot the residuals against predicted or "fitted" values from the regression

#Absence of Outlying or Influential Observations via computing  hat values and Cook's distance is helpful.
plot(hatvalues(fit.mr)) 
which.max(hatvalues(fit.mr))
cooks.distance(fit.mr)  # this results will produce lots of scientific notation
options(scipen=999) #turn off scientific notation
cooks.distance(fit.mr)

#Collinearity Among Predictors and the Variance Inflation Factor
p1 <- c(2, 4, 6, 8, 10)
p2 <- c(4, 8, 12, 16, 20)
y <- c(12, 15, 8, 4, 1)
model <- lm(y ~ p1 + p2)
summary(model)
model.1 <- lm(y ~ p1)
summary(model.1)

p2 <- c(4, 8, 12, 16, 21)
model <- lm(y ~ p1 + p2)
summary(model)
library(car)
vif(model)
library(car)
vif(model)


#Hierarchical Regression
gaf.data <- read.table("gaf.txt", header = T)
gaf.data
attach(gaf.data)
gaf.fit <- lm(gaf ~ age)
summary(gaf.fit)

gaf.fit <- lm(gaf ~ age + pretherapy)
summary(gaf.fit)

mod.1 <- lm(gaf ~ age)
summary(mod.1)


#Statistical Mediation
mod.1 <- lm(gaf ~ age)
summary(mod.1)
mod.2 <- lm(gaf ~ age + pretherapy)
summary(mod.2)

#Stepwise Selection regression (Hybrid approach)
library(MASS)
iq.step.fit <- lm(verbal ~ quant + analytic, data = iq.data)
step <- stepAIC(iq.step.fit, direction = "both")
#The specification of direction = both above tells R to conduct stepwise regression, which includes a combination of forward and backward approaches.
#To perform a purely forward selection, we would specify direction = forward, and to perform a purely backward selection, we would specify direction = backward.

#Logistic regression & Generalized linear model

n_accidents <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5)
hist(n_accidents)
n_accidents_bin <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
                     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
# number of accidents now has only two values (0 and 1)
hist(n_accidents_bin)
hist(n_accidents_bin, xaxt = 'n')
axis(side=1, at=seq(0, 1))
q <- c(5, 2, 6, 9, 8, 7, 9, 10, 10, 9)
v <- c(2, 1, 3, 7, 9, 8, 8, 10, 9, 8)
train <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
qv.data <- data.frame(q, v, train)
qv.data
fit <- glm(train ~ q, data = qv.data, family=binomial())  #logistic model
summary(fit) 
log(3) #natural logarithm of 3,
log(0.3333) 
exp(1.0986) # e to 1.0986

oring <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0,
           0, 1, 0, 0, 0, 0, 0)
temp <- c(53, 57, 58, 63, 66, 67, 67, 67, 68, 69, 70, 70,
          70, 70, 72, 73, 75, 75, 76, 76, 78, 79, 81)
challenger <- data.frame(oring, temp)
challenger
challenger.linear <- lm(oring ~ temp)
summary(challenger.linear)
fitted <- fitted(challenger.linear)
resid <- residuals(challenger.linear)
plot(fitted, resid)  # linear model on these data may not be ideal. We need a new approach, which is why we will turn to logistic regression to analyze these data

challenger.fit <- glm(oring ~ temp, data = challenger, family = binomial())
summary(challenger.fit)
anova(challenger.fit, test="Chisq")  #Analysis of Deviance Table
confint.default(challenger.fit) #obtain confidence intervals for both the intercept and predictor

predict.prob <- data.frame(temp = 30)  #predicting probability
predict(challenger.fit, type="response", newdata =
            predict.prob)
predict.prob <- data.frame(temp = 90)
predict(challenger.fit, type="response", newdata =
            predict.prob)
predict(challenger.fit)  #obtain predicted logits for cases in our data 1 through 23
y <- 15.0429 -0.2322*temp
y


#Multiple Logistic Regression
fit.qv <- glm(train ~ q + v, data = qv.data,
              family=binomial())
summary(fit.qv)
exp(coef(fit.qv))  #odds ratios (Cohen et al., 2003) for these predictors
glm.probs = predict(fit.qv, type = "response")
glm.probs[1:5]


# MANOVA in R
quant <- c(5, 2, 6, 9, 8, 7, 9, 10, 10)
verbal <- c(2, 1, 3, 7, 9, 8, 8, 10, 9)
train <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
iq.train <- data.frame(quant, verbal, train)
iq.train
y <- cbind(quant, verbal)  #generate the new dependent variable, which is the combination of quant and verbal.
y
train.f <- factor(train, levels = 1:3)
levels(train.f ) <- c("none", "some", "much")  #we designate train as a factor, and give the levels of the factor names "none," "some," and "much," where 1:3 denotes the three levels
train.f
manova.fit <- manova(y ~ train.f)
summary(manova.fit)  #By default, R reports Pillai's trace.
summary(manova.fit, test = "Wilks") #ask R to produce classic Wilks' lambda:


iq.train.t <- t(iq.train)
iq.train.t
library(mvnormtest) #conduct the multivariate Shapiro test using the mshapiro.test() function 
mshapiro.test(iq.train.t)

#Outliers
library(distances)
distances(iq.train[1:2], normalize = "mahalanobize")

#Homogeneity of Covariance Matrices 
library(biotools)
iq.train <- data.frame(iq.train)
boxM(iq.train[1:2], iq.train[,3]) #using Box-M test; Box-M test is quite sensitive to sample sizes.


#Discriminant Analysis:sought out a function that would maximize group differentiation
iq.train
library(MASS)
lda.fit <- lda(train.f ~ verbal + quant, data=iq.train)
lda.fit
plot(lda.fit)
#Computing Discriminant Scores Manually
predict(lda.fit) 
#Predicting Group Membership
predict(lda.fit)
result = predict(lda.fit)$class
result = cbind(result)
prior = cbind(train.f)
out = data.frame(prior, result)
out
confusion = table(train.f, result)
confusion
classification = table(prior, result)
diag(prop.table(confusion))
sum(diag(prop.table(confusion)))
prop.table(confusion)
#Visualizing Separation
plot(lda.fit, dimen = 1)
plot(lda.fit, type = "density", dimen = 1)


#Quadratic Discriminant Analysis
library(MASS)
qda.fit <- qda(train.f ~ verbal + quant, data = iq.train)
qda.fit
result = predict(qda.fit)$class
result = cbind(result)
prior = cbind(train.f)
out = data.frame(prior, result)
out
confusion.qda = table(train.f, result)
confusion.qda



#PCA
x <- c(0, 0.9, 1.80, 2.60, 3.30, 4.40, 5.20, 6.10, 6.50, 7.40)
y<-c(5.90,5.40,4.40,4.60,3.50,3.70,2.80,2.80,2.40,1.50)
pca.data <- data.frame(x, y)
pca.data
plot(x, y)
A <- cov(pca.data) # we will build the covariance matrix of x and y
A
var(x)
var(y)
diag(A)
cov(x,y)

library(rela)
paf.pca = paf(pca.data,eigcrit=1, convcrit=.001)
pca.data.matrix <- as.matrix(pca.data)
paf.pca = paf(pca.data.matrix,eigcrit=1, convcrit=.001)
paf.pca
cortest.bartlett(pca.data.matrix, n = 10)
pca <- princomp(covmat = A)
summary(pca)
fit.pca <- prcomp( ~ x + y)
fit.pca
pca$loadings  #generate loadings of the PCA 

(0.878)^2 + (-0.479)^2
(0.479)^2 + (0.878)^2

eigen(A)

comp.1 = 0.8778562*(x-3.82) -0.4789243*(y-3.7)
comp.1

comp.2 = 0.4789243*(x-3.82) +0.8778562*(y-3.7)
comp.2

var(comp.1)
var(comp.2)
screeplot(pca, type="lines", col=3)

head(USArrests)
cor(USArrests)
cor(USArrests$Murder, USArrests$Assault)
pairs(USArrests)
library(GGally)
ggpairs(USArrests)

usa.comp <- prcomp(USArrests, scale=TRUE)
usa.comp

library(FactoMineR)
library(factoextra)
scree <- fviz_eig(usa.comp)
scree
usa.comp.cov <- prcomp(USArrests, scale=FALSE)
usa.comp.cov
scree.cov <- fviz_eig(usa.comp.cov)
scree.cov


#EFA 
cormatrix
efa.2 <- factanal(covmat = cormatrix, factors = 2, n.obs = 1000)
efa.2
efa.3 <- factanal(covmat = cormatrix, factors = 3, n.obs = 1000)
efa.3
efa.4 <- factanal(covmat = cormatrix, factors = 4, n.obs = 1000)
efa.4
pca <- princomp(covmat = cormatrix)
loadings(pca)
eigen(cormatrix)
pca <- princomp(covmat = cormatrix)
plot(pca, type = "lines")

library(psych)
Holzinger.9
library(psych)
cor.plot(Holzinger.9,numbers=TRUE)

library(psych)
fa <- factanal(covmat = Holzinger.9, factors = 2, n.obs = 145,
                 rotation = "varimax")
fa
factor.2 = 0.376*0.376 + 0.219*0.219 + 0.293*0.293 +
  0.112*0.112 + 0.205*0.205 + 0.114*0.114 + 0.624*0.624 +
  0.864*0.864 + 0.635*0.635
factor.2
ev <- eigen(Holzinger.9)
ev
library(psych)
cluster.plot(fa)



# Cluster analysis
library(car)
attach(iris)
iris.data <- data.frame(Sepal.Length, Sepal.Width,
                          Petal.Length, Petal.Width)
some(iris.data)
iris.2 <- iris[,-5]  #the "???5" subtracts the fifth variable in the data,but includes the first 4.
species <- iris[,5]

pairs(iris.data, col = species,
        + lower.panel = NULL)

par(xpd = TRUE)
legend(x = 0.05, y = 0.4, cex = 2,
         + legend = as.character(levels(species)),
         + fill = unique(species))
par(xpd = NA)
set.seed(20)
k.means.fit <-kmeans(iris.data, 3, nstart = 20)  #The 3 above tells R how many clusters we are seeking
k.means.fit
species
table(species, k.means.fit$cluster)#We can generate the confusion matrix


# Hierarchical Cluster Analysis
d <- dist(iris.data, method = "euclidean")
d
clust.single <- hclust(d, method = "single")
clust.average <- hclust(d, method = "average")
plot(clust.single)
plot(clust.average)


#Nonparametric Tests
#Mann-Whitney U Test
grades.data <- read.table("grade_data.txt", header = T)
grades.data
attach(grades.data)
f.grade <- factor(grade)
mw.test <- wilcox.test(studytime ~ f.grade)
mw.test

#Kruskal-Wallis Test
achiev <- read.table("achiev.txt", header = T)
head(achiev)
attach(achiev)
f.teach <- factor(teach)
kruskal.test(ac ~ f.teach)
library(PMCMR)
posthoc.kruskal.nemenyi.test(ac, teach, method = "Tukey")

#Paired comparison: Wilcoxon Signed-Rank Test and Friedman Test
learn <- read.table("learning.txt", header = T)
learn
trial.1 <- c(10, 12.1, 9.2, 11.6, 8.3, 10.5)
trial.2 <- c(8.2, 11.2, 8.1, 10.5, 7.6, 9.5)
wilcox.test(trial.1, trial.2, paired = TRUE)

attach(learn)
fried.test <- friedman.test(time ~ trial|rat)
fried.test

wilcox.test(trial.1, trial.2, paired = TRUE)
trial.3 <- c(5.3, 9.1, 4.6, 8.1, 5.5, 8.1)
wilcox.test(trial.1, trial.3, paired = TRUE)

#Sign test
binom.test(5, 10)

