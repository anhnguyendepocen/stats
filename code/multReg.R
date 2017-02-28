# multiple regression

fname <- "http://www.gribblelab.org/stats/data/bball.csv"
bdata <- read.table(fname, sep=",", header=TRUE)
pairs(bdata, pch=16)
r <- cor(bdata)

# let's say we're interested in predicting FTP from the other variables

r <- r[7,-7]
N <- nrow(bdata)
# compute F statistic and then p value
F <- ((r^2)*(N-2))/(1-(r^2))
p <- 1-pf(F,1,N-2)

# or we can use the rcorr() function which is in the Hmisc package
# which computes r as well as p values for the significance test
# install.packages("Hmisc")
library(Hmisc)
rcorr(as.matrix(bdata))

# linear model
m0 <- lm(FTP ~ 1, data=bdata)
mall <- lm(FTP ~ GAMES + MPG + HGT + PPM + AGE + FGP, data=bdata)
# stepwise regression using step()
m2 <- step(m0, list(lower=m0, upper=mall), direction="both")
summary(m2)

# let's say we have a new potential recruit with:
newR <- data.frame(GAMES=64, PPM=0.4, MPG=20, HGT=200, FGP=60, AGE=23)

# and we want to predict FTP using our model:
newFTP <- predict(m2, newR, se.fit=TRUE, interval="prediction", level=0.95)
newFTP


# adding or deleting terms by hand using F-tests
# using add1() or drop1()

m0 <- lm(FTP ~ 1,data=bdata)
mall <- lm(FTP ~ GAMES + MPG + HGT + PPM + AGE + FGP, data=bdata)

# start with nothing in the model
# which to add?
add1(m0, formula(mall), test="F")
# add best single one
mbest <- lm(FTP ~ 1 + MPG, data=bdata)
# add next best single one
add1(mbest, formula(mall), test="F")
# add best single one
mbest <- lm(FTP ~ 1 + MPG + AGE, data=bdata)
# now do check for unique variance
drop1(mbest,test="F")
# all still contribute unique variance so we keep them all in the model
# add next one
add1(mbest, formula(mall), test="F")
# none are significant any more, we stop
# (FGP would be the next best one, but p = 0.1029, not < 0.05)

# some people use p<0.05 for entry into the model, and p>0.10 for removal
# (this is in fact the default in SPSS)


# use stepwise regression to automatically select best model
# note R uses AIC not F-tests
# http://en.wikipedia.org/wiki/Akaike_information_criterion

m0 <- lm(FTP ~ 1, data=bdata)
mall <- lm(FTP ~ GAMES + MPG + HGT + PPM + AGE + FGP, data=bdata)
mbest <- step(m0, list(lower=m0, upper=mall), direction="both")

# we can ask R to use F tests like this:

step(m0, list(lower=m0, upper=mall), direction="both", test="F")


