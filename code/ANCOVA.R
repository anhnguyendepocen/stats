# ANCOVA example code

compensation <- read.table("http://www.gribblelab.org/stats/data/compensation.txt", header=T, sep="\t")

with(compensation, plot(Grazing, Fruit))
with(compensation, tapply(Fruit, Grazing, mean))
with(compensation, plot(Root, Fruit, pch=c(rep(1,20), rep(2,20))))
legend("topleft", legend=c("Ungrazed","Grazed"), pch=1:2)

# full and restricted models
mF <- lm(Fruit ~ Root + Grazing, data=compensation)
mR <- lm(Fruit ~ Root, data=compensation)
anova(mR, mF)

# look at full model coefficients
summary(mF)
(c <- coef(mF))

# plot raw data plus full model fits
abline(c[1], c[2], lty=1)
abline(c[1]+c[3], c[2], lty=2)

# out of interest, plot restricted model fit
(cr <- coef(mR))
abline(cr[1], cr[2], lty=3)

# put up a legend
legend("bottomright", legend=c("mF_grazed", "mF_ungrazed", "mR"), lty=1:3)

# predict values of Fruit if Root were the same (7)
d2 <- data.frame(Root=c(7,7), Grazing=factor(c("Grazed", "Ungrazed")))
adjmeans <- predict(mF, d2)
(tapply(adjmeans, d2$Grazing, identity))
abline(v=7, col="red")
abline(h=adjmeans[1], col="red")
abline(h=adjmeans[2], col="red")

rawmeans <- tapply(compensation$Fruit, compensation$Grazing, mean)

print(rawmeans)
print(adjmeans)

#############################################################################

# we can test whether Grazing has an effect, without including the covariate
mF <- lm(Fruit ~ Grazing, data=compensation)
mR <- lm(Fruit ~ 1, data=compensation)
anova(mR, mF)

# test the effect of Grazing, when the covariate is included
mF <- lm(Fruit ~ Grazing + Root, data=compensation)
mR <- lm(Fruit ~ Root, data=compensation)
anova(mR, mF)

# we can test whether a covariate should be included
mCovF <- lm(Fruit ~ Grazing + Root, data=compensation)
mCovR <- lm(Fruit ~ Grazing, data=compensation)
anova(mCovR, mCovF)

# we can test the homogeneity of slopes assumption
mFhs <- lm(Fruit ~ Root + Grazing + Root:Grazing, data=compensation)
mRhs <- lm(Fruit ~ Root + Grazing, data=compensation)
anova(mRhs, mFhs)

