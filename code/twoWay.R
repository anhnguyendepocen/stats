# 2-way ANOVA demo code

# sum-to-zero convention for contrast weights
options(contrasts=c("contr.sum","contr.poly"))

# load in data file
fname <- "http://www.gribblelab.org/stats/data/2waydata.csv"
mydata <- read.table(fname, sep=",", header=T, stringsAsFactors=T)
myanova <- aov(DV~factorA*factorB,data=mydata)

summary(myanova)

# table of means
with(mydata, tapply(DV,list(factorA,factorB),mean))

# plot means
with(mydata, interaction.plot(factorA,factorB,DV,col=2:3,lty=1))

# Tukey tests of individual differences
TukeyHSD(myanova, "factorA:factorB")

# plot just the Tukey pairwise tests within the interaction
plot(TukeyHSD(myanova, "factorA:factorB"))
abline(v=0, lty=2)

# F-tests of individual contrasts
# SSpsi = n*psi**2 / sum(c**2)
# F = (SSpsi/dfpsi) / MSw

# test B1A1 vs B1A2
SSpsi = (5 * (3.0 - 4.2)**2) / ((+1)**2 + (-1)**2)
Fpsi = (SSpsi/1) / 0.750
p_psi = pf(Fpsi, 1, 24, lower.tail=FALSE) # uncorrected

# example of a complex contrast
# test avg(B2A1, B2A2, B2A3) vs B1A3
# constrast weights: 
# -1/3 -1/3 -1/3 +1
SSpsi = (5 * ((-1/3)*5.0 + (-1/3)*4.6 + (-1/3)*4.8 + (1)*5.6)**2) / ((-1/3)**2 + (-1/3)**2 + (-1/3)**2 + (1)**2)
Fpsi = (SSpsi/3) / 0.750
p_psi = pf(Fpsi, 3, 24, lower.tail=FALSE) # uncorrected

# illustration:
summary(aov(DV~factorA,data=mydata))
summary(aov(DV~factorB,data=mydata))
summary(aov(DV~factorA+factorB,data=mydata))
summary(aov(DV~factorA+factorB+factorA:factorB,data=mydata))


