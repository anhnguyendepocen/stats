# EXAMPLE 1: Illy vs Lavazza

# our data: 3 taste tests: Correct, Correct, Incorrect
# our model of the underlying process: binomial distribution
# binomial takes three parameters: n, k, p
# n = number of trials
# k = number of successes
# p = probability of success on any given trial
n <- 3
k <- 2

# for our data, n=3, k=2, p=unknown
# p is the unknown parameter that represents my ability to correctly distinguish
# Illy coffee from Lavazza coffee. If p=0.50 then my chance of being correct on 
# any given taste-test trial is 50/50, which means I am guessing.
# A value of p=1.0 means I have perfect discrimination ability.
# So what is our estimate of the true value of p, given our data from 3 trials?

# Be Bayesian: prob(p | data) = ( prob(data | p) * prob(p) ) / ( prob(data) )
#              posterior      = ( likelihood     * prior   ) / ( evidence )

# let's plot our likelihood function
# first define a range of possible values of p
p_range <- seq(0, 1, .01)

# Our likelihood function is the probability density function (pdf)
# of the Binomial distribution. We can compute this in R using dbinom()
# x=2 is 2 successes, size=3 is 3 trials
my_likelihood <- dbinom(x=k, size=n, prob=p_range)

# plot it
par(mfrow=c(3,1))
plot(p_range, my_likelihood, type="l", col="blue", main="likelihood", lwd=2,
     xlab="p_range", ylab="pdf", yaxt="n")

# remember from last week: the MLE for p is k/n which is 2/3 in our case
abline(v=k/n, col="red", lty=2)

# Now to be Bayesian we need to incorporate a prior. What should it be?
# Let's first try a flat prior (a noninformative) prior.
# Let's also be clever and choose a mathematical function to represent our prior, that
# will make computing the posterior really easy. We'll choose the Beta distribution,
# since it's a conjugate prior for the Binomial distribution.
# The Beta distribution takes two parameters, alpha and beta. We can represent a flat
# prior using alpha=beta=1.0
prior_flat <- dbeta(p_range, shape1=1.0, shape2=1.0)

# normalize it so that its integral is 1.0
prior_flat <- prior_flat / sum(prior_flat)

# plot it
plot(p_range, prior_flat, type="l", col="blue", lwd=2, yaxt="n")

# Now let's compute the posterior. Since we chose a conjugate prior,
# we know from the calculus ninjas' hard work that the posterior is
# simply also a Beta distribution with:
# alpha_post = k + alpha_prior
# beta_post = n - k + beta_prior
# Let's get our posterior:
posterior1 <- dbeta(p_range, shape1=k+1, shape2=n-k+1)

# plot it
plot(p_range, posterior1, type="l", col="blue", lwd=2, yaxt="n")

# And we see that posterior1 is in fact the same as the likelihood.
# This is because the flat, uninformative prior, essentially had no effect.

# So let's try a different prior, one that reflects our prior belief that I am
# likely guessing. Why might we have such a belief? Maybe we have been doing
# Illy vs Lavazza taste-tests for many years with hundreds of people, and 
# we can base our prior on our previous population estimates of p.
# e.g. let's imagine we have tested 100 people over the past year, and for each,
# given them an Illy vs Lavazza taste-test, and for each, recorded the MLE for p.
# Here are our data:
# MLE of p, # people
# 0.1, 1
# 0.2, 1
# 0.3, 1
# 0.4, 6
# 0.5, 77
# 0.6, 8
# 0.7, 2
# 0.8, 1
# 0.9, 1

# plot those data
p_tested_range <- seq(0.1, 0.9, .1)
p_tested_data <- c(1,1,1,6,77,8,2,1,1)
par(mfrow=c(2,1))
plot(p_tested_range, p_tested_data, type="b", col="blue", yaxt="n")

# Now our task is to come up with a prior, to represent these data. Again,
# let's be clever and choose a conjugate prior for Binomial, the Beta distribution.
# Now we have to come up with alpha and beta values (the 2 parameters for Beta) that
# result in a curve that looks like our data.

# with some trial and error alpha=50 and beta=50 look good:
p_tested_pred <- dbeta(p_range, shape1=50, shape2=50)
plot(p_range, p_tested_pred, type="l", col="red", yaxt="n")

# Note there are also packages (look it up) where you can estimate params of a
# distribution from data (curve fitting). Here we just guessed a=50, b=50

# OK now we have our informative prior. Let's compute our posterior. Again, since we
# were clever and used a Beta function to represent the prior, and since the calculus
# ninjas worked out for us that a Beta distribution is a conjugate prior for the
# binomial, coming up with the posterior is simple:

alpha2 = k+50
beta2 = n-k+50
posterior2 <- dbeta(p_range, shape1=alpha2, shape2=beta2)

# We can compute the mode (the peak), again our calculus ninjas tell us that:
mode2 <- (alpha2-1) / (alpha2 + beta2 - 2)

# plot it
par(mfrow=c(3,1))
plot(p_range, posterior2, type="l", col="blue", main=paste("MLE=", round(mode2,3)), yaxt="n")
abline(v=0.5, col="black", lty=1)
abline(v=mode2, col="red", lty=2)

# not much different than the prior, in fact! The prior dominates the data, which
# by comparison, are small in number.

# What if we did more experiments on me, not just 3 tests, but 5 tests per day for
# a whole week:
n3 <- 5*7 # 35 total new trials
k3 <- 28  # I got 28 correct

# Now we can update the model. The previous posterior becomes our new prior, and the
# new data becomes our likelihood. We can compute our new posterior:

alpha3 <- k3 + alpha2
beta3 <- n3-k3 + beta2
posterior3 <- dbeta(p_range, shape1=alpha3, shape2=beta3)
mode3 <- (alpha3-1) / (alpha3 + beta3 - 2)
plot(p_range, posterior3, type="l", col="blue", main=paste("MLE=", round(mode3,3)), yaxt="n")
abline(v=0.5, col="black", lty=1)
abline(v=mode3, col="red", lty=2)

# a ha! with some new data, the data overcome the prior

# out of interest: what is the probability that p <=0.50?
# Two ways of doing this:

# 1. Since R has a function for computing p-values from distributions like beta:
(p05 <- pbeta(0.5, alpha3, beta3, lower.tail=TRUE))

# 2. We can also sample from the distribution, and count:
posterior_sample <- rbeta(10000, alpha3, beta3)
hist(posterior_sample, breaks=50, xlim=c(0,1), col="blue")
(p05_sample <- length(which(posterior_sample <= 0.50)) / length(posterior_sample))
abline(v=0.5, col="black", lty=1)



