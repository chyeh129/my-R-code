library(readxl)
library(scatterplot3d)
library(MASS)
library(poLCA)


data(cheating)
f <- cbind(LIEEXAM,LIEPAPER,FRAUD,COPYEXAM)~1
ch2 <- poLCA(f,cheating,nclass=2)

data(carcinoma)
f <- cbind(A,B,C,D,E,F,G)~1
lca2 <- poLCA(f,carcinoma,nclass=2) # log-likelihood: -317.2568
lca3 <- poLCA(f,carcinoma,nclass=3) # log-likelihood: -293.705
lca4 <- poLCA(f,carcinoma,nclass=4,nrep=10,maxiter=5000) # log-likelihood: -289.2858


data(values)
f <- cbind(A,B,C,D)~1
M0 <- poLCA(f,values,nclass=1) # log-likelihood: -543.6498
M1 <- poLCA(f,values,nclass=2) # log-likelihood: -504.4677
M2 <- poLCA(f,values,nclass=3,maxiter=8000) # log-likelihood: -503.3011



##
## One draw from a three-category multinomial distribution.
##
p1 <- c(0.7,0.2,0.1)
rmulti(p1)
##
## 10,000 draws from a three-category multinomial distribution.
##
n <- 10000
p2 <- matrix(p1,nrow=n,ncol=length(p1),byrow=TRUE)
rmdraws <- rmulti(p2)
table(rmdraws)/n # should be approximately 0.7, 0.2, 0.1
##
## 10,000 draws from a mixture of three groups of a
## four-category multinomial distribution.
##
group.p <- matrix(c(0.5,0.3,0.2),nrow=n,ncol=3,byrow=TRUE)
group <- rmulti(group.p)
p3 <- t(matrix(NA,nrow=n,ncol=4))
p3[,group==1] <- c(0.7,0.1,0.1,0.1)
p3[,group==2] <- c(0.1,0.7,0.1,0.1)
p3[,group==3] <- c(0.1,0.1,0.1,0.7)
p3 <- t(p3)
rmdraws3 <- rmulti(p3)
table(group,rmdraws3)
table(group,rmdraws3)/rowSums(table(group,rmdraws3))
