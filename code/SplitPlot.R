# two-way repeated measures ANOVA

fname <- "http://www.gribblelab.org/stats/data/2wayrepdata.csv"
mdata <- read.table(fname,sep=",",header=TRUE)
dm <- as.matrix(mdata[1:5, 2:7])
mlm <- lm(dm ~ 1)
af <- factor(c("a1","a1","a1","a2","a2","a2")) 
bf <- factor(c("b1","b2","b3","b1","b2","b3")) 
myfac <- data.frame(factorA = af, factorB = bf) 
library(car)
mlm.aov <- Anova(mlm, idata = myfac, idesign = ~ factorA * factorB, type = "III") 
summary(mlm.aov, multivariate = FALSE) 

# split plot ANOVA

fname <- "http://www.gribblelab.org/stats/data/splitplotdata.csv"
mdata <- read.table(fname, sep = ",", header = TRUE)
dm <- as.matrix(mdata[1:10, 2:4])
mlm <- lm(dm ~ 1 + gender, data = mdata)
af <- factor(c("a1", "a2", "a3")) 
myfac <- data.frame(factorA = af) 
mlm.aov <- Anova(mlm, idata = myfac, idesign = ~ factorA, type = "III")
summary(mlm.aov, multivariate = FALSE)

