# one-way repeated measures ANOVA sample code

mydata <- read.table("http://www.gribblelab.org/stats/data/oneWayRepdata.csv", sep=",", header=TRUE)
mydata$treatment <- factor(mydata$treatment)
mydata$subject <- factor(mydata$subject)

# as a demonstration, run this as a between-subjects anova
# (we would not normally do this, but it is illustrative)

am1 <- aov(dv ~ treatment, data=mydata)
summary(am1)

# as a demo, run this as a two-factor anova,
# where treatment is one factor and subjects
# is another. Note: we are leaving out the
# interaction term

am2 <- aov(dv ~ treatment + subject, data=mydata)
summary(am2)

# run it using the standard notation in R
# for a repeated-measures factor, run in
# the univariate style

am3 <- aov(dv ~ treatment + Error(subject/treatment), data=mydata)
summary(am3)

################

# now run the anova using a multivariate approach
# (see Maxwell & Delaney for all the details of what this means)
# 
# the reason to do this is that this approach
# provides computation of sphericity, and the
# corrected versions of F tests
#
# First reorganize data into matrix where rows are subjects

response <- with(mydata,
	cbind(dv[treatment==1], dv[treatment==2], dv[treatment==3], dv[treatment==4]))

# now form the multivariate model using lm()

# this defines any between-subjects factors... since there
# aren't any, we just enter an intercept (1)
mlm1 <- lm(response ~ 1)

# now set up a variable to define the design of the study
# (one within factor)

rfactor <- factor(c("r1", "r2", "r3", "r4"))

# load the car Library which contains the useful function Anova()
# (note the capital A); type ?Anova for help on the Anova() function

library(car)

# now run the anova to convert the multivariate model mlm1 into
# an anova object

mlm1.aov <- Anova(mlm1, idata = data.frame(rfactor), idesign = ~rfactor, type = "III")
summary(mlm1.aov, multivariate=FALSE)

# perform follow up test comparing individual means
# e.g. resp1 vs resp2
# First do it using the usual F ratio for comparisons
# this uses the error term from the anova
# get df and error term from anova table (look at it)

df2 <- 27 # df for error term
sserr <- 28.6 # ss error term
mserr <- sserr / df2 # compute mserr
n <- 10 # num subjects per group
a <- 4 # num groups
mresp1 <- mean(response[,1]) # mean of resp1
mresp2 <- mean(response[,2]) # mean of resp2
sscomp <- (mresp1-mresp2)^2 # ss comparison
dfcomp <- 1 # df comparison
Fcomp <- (n*sscomp/2) / (mserr) # Fobs for comparison
pcomp <- pf(Fcomp, dfcomp, df2, lower.tail=FALSE) # pobs for comparison

# note this is uncorrected for type-I error
# for example, let's do a Tukey test (see pg. 550, eq 41)

qobs <- sqrt(2*Fcomp) # compute q value
# df for q are (a,(a-1)(n-1))
pt <- ptukey(qobs, 4, (4-1)*(n-1), lower.tail=FALSE)

# if you're concerned about violating the sphericity assumption,
# then a better approach would be a simple t-test of the difference between
# scores of each group. Eg. comparing resp1 vs resp2:

dscores <- response[,1] - response[,2]
t.test(dscores)

# or we could do it using fancy matrix multiplication:

mycontrast <- c(+1, -1, 0, 0)

# %*% is matrix multiplications
dscores <- response %*% mycontrast
t.test(dscores)

# note this is uncorrected for Type-I error. You can always transform a t into 
# an F (when dfnum=1), then into a q and do Tukey tests.

# if you're interested in complex comparisons, then just specify the
# contrast - e.g. compare mean 1 against average of means 2,3,4

mycontrast <- c(3, -1, -1, -1)
dscores2 <- response %*% mycontrast
t.test(dscores2)

################################## USING LMER ##################################

# lmer() allows fitting linear mixed-effects models. We model treatment
# as a fixed effect, and subjects as a random effect

library(lme4)

mydata <- read.table("http://www.gribblelab.org/stats/data/oneWayRepdata.csv", sep=",", header=TRUE)
mydata$treatment <- factor(mydata$treatment)
mydata$subject <- factor(mydata$subject)

# treatment is a fixed effect, subject is a random effect
lmerfit <- lmer(dv ~ treatment + (1|subject), data=mydata)
anova(lmerfit)

# now perform Tukey pairwise tests across levels of
# the fixed effect (treatment)

library(multcomp)

contr <- glht(lmerfit, linfct=mcp(treatment="Tukey"))
summary(contr)

