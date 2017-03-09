# MLE examples

######################################################
# 1. fitting a bernoulli process (success/failure)
#    coin flips: is our coin fair?

# our observed data: 20 flips of a coin
flips <- c(1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0) # 12 heads, 8 tails

# our negative log-likelihood function
NLL <- function(theta, y) {
	NLL <- 0
	N <- length(y)
	for (i in 1:N) {
		if (y[i]==1) {p <- theta}    # heads
		if (y[i]==0) {p <- (1-theta)} # tails
		NLL <- NLL + log(p)
	}
	return(-NLL)
}

# perform the MLE with lower and upper bounds on theta
# (for 1D optim problems, R suggests to use the "Brent" method)
out <- optim(par=0.5, fn=NLL, method="Brent", lower=0.0001, upper=0.9999, y=flips)
(theta_mle <- out$par)

######################################################
# 2. fitting a normal distribution

# our observed data
x <- c(85, 84, 75, 93, 88, 82, 85, 94, 86, 76, 81, 98, 95, 82, 76, 91, 81, 82, 72, 94)

# the negative log-likelihood function
NLL <- function(theta, data) {
	mu <- theta[1]
	sigma <- theta[2]
	n <- length(data)
	NLL <- (n/2)*log(2*pi) - (n/2)*log(sigma**2) # from the gaussian pdf
	tmp <- 0
	for (i in 1:n) {
		tmp = tmp + (data[i]-mu)**2
	}
	NLL <- NLL + -(1/(2*(sigma**2)))*tmp
	return(-NLL)
}

# perform the MLE
out <- optim(par=c(100,10), fn=NLL, data=x)
(theta_mle <- out$par)


######################################################
# 3. estimating psychometric functions

# probability of responding "right" given an x position of the unseen arm is:
#
# p = 1/(1+exp(-y))
#
# where y = B0 + B1*x

# first define a logistic function
logistic <- function(y) {
	p = 1/(1+exp(y))
	return(p)
}

# write out NLL function
NLL <- function(B,X,R) {
	y = B[1] + B[2]*X
	p = logistic(y)
	NLL = -sum(log(p[R==1])) - sum(log(1-p[R==0]))
	return(NLL)
}

# load in some example data and plot it
pdata <- read.table("http://www.gribblelab.org/stats/data/psychometric_data.txt", sep=" ", header=FALSE)
X <- pdata$V2 # unseen arm position along the left-right axis
R <- pdata$V3 # subject's response (left=0, right=1)
plot(X, R+rnorm(length(R), 0, .01), type="p", col="blue")
grid()

# perform the MLE
out <- optim(par=c(-.1,.1), fn=NLL, X=X, R=R)
(Bfit <- out$par)

# plot the resulting psychometric function
Xp <- seq(-20,20,.1)
p = logistic(Bfit[1] + Bfit[2]*Xp)
lines(Xp, p, type="l", col="red")








