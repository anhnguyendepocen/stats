# some data

Fac <- factor(c(rep("A",5),rep("B",5),rep("C",5)))
DV <- c(2,3,2,4,2,3,2,4,5,4,6,4,4,5,8)

myaov <- anova(lm(DV ~ Fac))
myaov

# perform follow up test comparing individual means
# e.g. B vs C
# First do it using the usual F ratio for comparisons
# this uses the error term from the anova
# get df and error term from anova table

dfnum <- 1 # because we are doing a pairwise test between 2 means
dfdenom <- myaov$Df[2]
mserr <- myaov$"Mean Sq"[2]
n <- 5 # subjects per group
a <- 3 # number of groups

mA <- mean(DV[Fac=="A"]) # mean of A
mB <- mean(DV[Fac=="B"]) # mean of B
mC <- mean(DV[Fac=="C"]) # mean of C
sscomp <- (mB-mC)^2 # ss comparison
(Fcomp <- (n*sscomp/2) / (mserr)) # Fobs for comparison
(pcomp <- pf(Fcomp, dfnum, dfdenom, lower.tail=FALSE)) 

# note this is uncorrected for type-I error
# Let's do a Tukey test

qobs <- sqrt(2*Fcomp) # compute q value
# df for q are (a,(a-1)(n-1))
(pt <- ptukey(q=qobs, nmeans=a, df=(a-1)*(n-1), lower.tail=FALSE))

# R can do Tukey tests for us:
TukeyHSD(aov(DV ~ Fac))

