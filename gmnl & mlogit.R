install.packages("devtools")
library("gmnl")
library("mlogit")
data("TravelMode", package = "AER")

TM <- mlogit.data(TravelMode, choice = "choice", shape = "long",alt.levels = c("air", "train", "bus", "car"), chid.var = "individual")

TM <- mlogit.data(TravelMode, choice = "choice", shape = "long",alt.levels = c("air", "train", "bus", "car"))
mixl <- gmnl(choice ~ vcost + travel + wait, data = TM, model = "mixl", ranp = c(travel = "n"), R = 50)
summary(mixl)




data("TravelMode", package = "AER")
with(TravelMode, prop.table(table(mode[choice == "yes"])))
head(TravelMode)

## Examples using the Fishing data set from the AER package
data("TravelMode", package = "AER")
library(mlogit)
TM <- mlogit.data(TravelMode, choice = "choice", shape = "long",alt.levels = c("air", "train", "bus", "car"), chid.var = "individual")
## Not run:
## S-MNL model, ASCs not scaled
smnl <- gmnl(choice ~ wait + vcost + travel + gcost| 1, data = TM,model = "smnl", R = 100,notscale = c(1, 1, 1, rep(0, 4)))
summary(smnl)
## MIXL model with observed heterogeneity
mixl.hier <- gmnl(choice ~ vcost + gcost + travel + wait | 1 | 0 | income + size - 1,data = TM,model = "mixl", ranp = c(travel = "t", wait = "n"), mvar = list(travel = c("income","size"), wait = c("income")), R = 30,haltons = list("primes"= c(2, 17), "drop" = rep(19, 2)))
summary(mixl.hier)
## Examples using the Electricity data set from the mlogit package
data("Electricity", package = "mlogit")
Electr <- mlogit.data(Electricity, id.var = "id", choice = "choice", varying = 3:26, shape = "wide", sep = "")
## Estimate a MIXL model with correlated random parameters
Elec.cor <- gmnl(choice ~ pf + cl + loc + wk + tod + seas| 0, data = Electr,subset = 1:3000,model = 'mixl',R = 10,panel = TRUE,ranp = c(cl = "n", loc = "n", wk = "n", tod = "n", seas = "n"), correlation = TRUE)
summary(Elec.cor)
cov.gmnl(Elec.cor)
se.cov.gmnl(Elec.cor)
se.cov.gmnl(Elec.cor, sd = TRUE)
cor.gmnl(Elec.cor)
## Estimate a G-MNL model, where ASCs are also random
Electr$asc2 <- as.numeric(Electr$alt == 2)
Electr$asc3 <- as.numeric(Electr$alt == 3)
Electr$asc4 <- as.numeric(Electr$alt == 4)
Elec.gmnl <- gmnl(choice ~ pf + cl + loc + wk + tod + seas + asc2 + asc3 + asc4 | 0,data = Electr,subset = 1:3000,model = 'gmnl',R = 30,panel = TRUE,notscale = c(rep(0, 6), 1, 1, 1),ranp = c(cl = "n", loc = "n", wk = "n", tod = "n", seas = "n",asc2 = "n", asc3 = "n", asc4 = "n"))
summary(Elec.gmnl)
## Estimate a LC model with 2 classes
Elec.lc <- gmnl(choice ~ pf + cl + loc + wk + tod + seas| 0 | 0 | 0 | 1,data = Electr,subset = 1:3000, model = 'lc',panel = TRUE, Q = 2)
summary(Elec.lc)
## Estimate a MM-MIXL model
Elec.mm <- gmnl(choice ~ pf + cl + loc + wk + tod + seas| 0 | 0 | 0 | 1, data = Electr,subset = 1:3000,model = 'mm',R = 30, panel = TRUE, ranp = c(pf = "n", cl = "n", loc = "n", wk = "n", tod = "n",seas = "n"),Q = 2, iterlim = 500)
summary(Elec.mm)


rm(list = ls(all = TRUE))   # Clean objects
library("gmnl")             # Load gmnl package
library("mlogit")           # Load mlogit package
# Load data and put it into the required format
data("Electricity", package = "mlogit")
Electr <- mlogit.data(Electricity,  id = "id",  choice = "choice",  varying = 3:26,  shape = "wide", sep = "")
# Estimate a LC-MNL model with 3 classes
install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.1-0.tar.gz", repos=NULL,type="source")
data("Electricity", package = "mlogit")
Electr <- mlogit.data(Electricity, id.var = "id", choice = "choice", varying = 3:26, shape = "wide", sep = "")
Elec.lc <- gmnl(choice ~ pf + cl + loc + wk + tod + seas | 0 | 0 | 0 | 1, data = Electr, subset = 1:3000, model = "lc", panel = TRUE, Q = 2)
summary(Elec.lc)
lc <- gmnl(choice ~ pf + cl + loc + wk + tod + seas | 0 | 0 | 0 | 1 , data = Electr, model = 'lc',  Q = 3,  panel = TRUE,method = "bhhh")





library("mlogit")
TM <- mlogit.data(TravelMode, choice = "choice", shape = "long",alt.levels = c("air", "train", "bus", "car"))
wide_TM <- reshape(TravelMode, idvar = c("individual", "income", "size"), timevar = "mode", direction = "wide")
wide_TM$chosen_mode[wide_TM$choice.air == "yes"] <- "air"
wide_TM$chosen_mode[wide_TM$choice.car == "yes"] <- "car"
wide_TM$chosen_mode[wide_TM$choice.train == "yes"] <- "train"
wide_TM$chosen_mode[wide_TM$choice.bus == "yes"] <- "bus"

library("plyr")
ddply(wide_TM, ~ chosen_mode, summarize, mean.income = mean(income))

ddply(wide_TM, ~ chosen_mode, summarize, mean.air = mean(vcost.air),mean.car = mean(vcost.car), mean.train = mean(vcost.train),mean.bus = mean(vcost.bus))

f1 <- choice ~ wait + vcost | income | travel
f2 <- choice ~ wait + vcost | income + 0 | travel
f2 <- choice ~ wait + vcost | income - 1 | travel
f3 <- choice ~ 0 | income + size | 0
f3 <- choice ~ 0 | income + size | 1
f4 <- choice ~ wait + vcost | 0
f4 <- choice ~ wait + vcost | 0 | 0
f4 <- choice ~ wait + vcost | -1 | 0
f5 <- choice ~ wait + vcost | 0 | 0 | income + size - 1
f6 <- choice ~ wait + vcost | 1 | 0 | 0 | income + size - 1
library("gmnl")
smnl.nh <- gmnl(choice ~ wait + vcost + travel | 1, data = TM,model = "smnl", R = 30, notscale = c(1, 1, 1, rep(0, 3)))

summary(smnl.nh)
smnl.het <- gmnl(choice ~ wait + vcost + travel | 1 | 0 | 0 | income - 1, data = TM, model = "smnl", R = 30, notscale = c(1, 1, 1, 0, 0, 0),typeR = FALSE)
summary(smnl.het)


library("lmtest")
waldtest(smnl.nh, smnl.het)
AIC(smnl.nh)
AIC(smnl.het)
BIC(smnl.nh)
BIC(smnl.het)
mixl.hier <- gmnl(choice ~ vcost + travel + wait | 1 | 0 | income +size - 1, data = TM, model = "mixl", ranp = c(travel = "t", wait = "n"), mvar = list(travel = c("income","size"),wait = c("income")), R = 50,haltons = list("primes" = c(2, 17), "drop" = rep(19, 2)))
summary(mixl.hier)



data("Electricity", package = "mlogit")
Electr <- mlogit.data(Electricity, id.var = "id", choice = "choice",varying = 3:26, shape = "wide", sep = "")
Elec.cor <- gmnl(choice ~ pf + cl + loc + wk + tod + seas | 0,data = Electr, subset = 1:3000, model = 'mixl', R = 50, panel = TRUE,ranp = c(cl = "n", loc = "n", wk = "n", tod = "n", seas = "n"),correlation = TRUE)
summary(Elec.cor)
vcov(Elec.cor, what = "ranp", type = "cov", se = "true")
vcov(Elec.cor, what = "ranp", type = "sd", se = "true")
vcov(Elec.cor, what = "ranp", type = "cor")

Electr$asc2 <- as.numeric(Electr$alt == 2)
Electr$asc3 <- as.numeric(Electr$alt == 3)
Electr$asc4 <- as.numeric(Electr$alt == 4)
Elec.gmnl <- gmnl(choice ~ pf + cl + loc + wk + tod + seas + asc2 +asc3 + asc4 | 0, data = Electr, subset = 1:3000, model = 'gmnl', R = 50, panel = TRUE, notscale = c(rep(0, 6), 1, 1, 1),ranp = c(cl = "n", loc = "n", wk = "n", tod = "n", seas = "n",asc2 = "n", asc3 = "n", asc4 = "n"))
summary(Elec.gmnl)
Elec.smnl.re <- gmnl(choice ~ pf + cl + loc + wk + tod + seas + asc2 +asc3 + asc4 | 0, data = Electr, subset = 1:3000, model = 'gmnl', R = 50,panel = TRUE, print.init = TRUE, notscale = c(rep(0, 6), 1, 1, 1),ranp = c(asc2 = "n", asc3 = "n", asc4 = "n"), init.gamma = 0, fixed = c(rep(FALSE, 16), TRUE), correlation = TRUE)
summary(Elec.smnl.re)


Elec.lc <- gmnl(choice ~ pf + cl + loc + wk + tod + seas | 0 | 0 | 0 | 1, data = Electr, subset = 1:3000, model = 'lc', panel = TRUE, Q = 2)
summary(Elec.lc)


Elec.mm <- gmnl(choice ~ pf + cl + loc + wk + tod + seas | 0 | 0 | 0 | 1,data = Electr, subset = 1:3000, model = 'mm', R = 50, panel = TRUE,ranp = c(pf = "n", cl = "n", loc = "n", wk = "n", tod = "n",seas = "n"), Q = 2, iterlim = 500)
summary(Elec.mm)


Elec.mm.c <- gmnl(choice ~ pf + cl + loc + wk + tod + seas | 0 | 0 | 0 | 1, data = Electr, subset = 1:3000, model = 'mm', R = 50, panel = TRUE,ranp = c(pf = "n", cl = "n", loc = "n", wk = "n", tod = "n",seas = "n"), Q = 2, iterlim = 500, correlation = TRUE)
summary(Elec.mm.c)
vcov(Elec.mm.c, what = "ranp", Q = 1, type = "sd", se = TRUE)

clogit <- gmnl(choice ~ pf + cl + loc + wk + tod + seas | 0,data = Electr, subset = 1:3000)
summary(clogit)
wtp.gmnl(clogit, wrt = "pf")
ElectrO <- mlogit.data(Electricity, id = "id", choice = "choice",varying = 3:26, shape = "wide", sep = "", opposite = c("pf"))
start <- c(1, 0, 0, 0, 0, 0, 0, 0)

wtps <- gmnl(choice ~ pf + cl + loc + wk + tod + seas | 0 | 0 | 0 | 1, data = ElectrO, model = "smnl", subset = 1:3000, R = 1,fixed = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),panel = TRUE, start = start, method = "bhhh", iterlim = 500)
summary(wtps)

-exp(coef(wtps)["het.(Intercept)"])
summary(wtps2)

n_ran <- 5
start3 <- c(1, coef(wtps), rep(0.1, .5 * n_ran * (n_ran + 1)), 0.1, 0)
wtps3 <- gmnl(choice ~ pf + cl + loc + wk + tod + seas | 0 | 0 | 0 | 1,data = ElectrO, subset = 1:3000, model = "gmnl", R = 50,fixed = c(TRUE, rep(FALSE, 22), TRUE), panel = TRUE, start = start3,ranp = c(cl = "n", loc = "n", wk = "n", tod = "n", seas = "n"),correlation = TRUE)

summary(wtps3)
plot(Elec.cor, par = "loc", effect = "ce", type = "density", col = "grey")
plot(Elec.cor, par = "loc", effect = "ce", ind = TRUE, id = 1:30)
bi.loc <- effect.gmnl(Elec.cor, par = "loc", effect = "ce")
summary(bi.loc$mean)
summary(bi.loc$sd.est)
wtp.loc <- effect.gmnl(Elec.cor, par = "loc", effect = "wtp", wrt = "pf")
summary(wtp.loc$mean)

summary(wtp.loc$sd.est)




