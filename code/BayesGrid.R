# load in our discrete prior
pfile <- file("http://www.gribblelab.org/stats/code/prior.txt")
load(pfile)

# plot the prior
plot(prior$theta, prior$prob, type="p", col="red", pch=15)

# our data are:
# k=20 successes in n=40 trials
# let's model the likelihood function p(data | model) using binomial distribution

# plot the likelihood
plot(prior$theta, dbinom(20, 40, prior$theta), type="p", col="magenta", pch=15)

# OK so now let's compute our posterior distribution
# we'll iterate over all possible values of theta
# according to our discrete grid

# Bayes' Theorem tells us that
# p(theta | D) = p(D | theta) * p(theta) / sum(p(D|theta)*p(theta)) (sum over all theta)

# compute the likelihood*prior for all values of theta:
# (the numerator)
post_num <- dbinom(10, 20, prior$theta) * prior$prob

# the denominator is just the sum over all values of theta
post_denom <- sum(post_num)

# compute the posterior
posterior <- post_num / post_denom

# plot it
plot(prior$theta, posterior, type="p", col="blue", pch=15)


	
