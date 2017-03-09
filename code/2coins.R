# the data
#
y1 = c(1,1,1,1,1,0,0)
y2 = c(1,1,0,0,0,0,0)
N1 = length(y1)
N2 = length(y2)

# the model
#
library(rjags)
jags = jags.model('2coins.bug', data = list('y1' = y1, 'y2' = y2, 'N1' = N1, 'N2' = N2), n.chains = 1, n.adapt = 200)

# run mcmc sampling, generate 1000 samples
# from the posterior for theta1 and theta2
post = coda.samples(jags, c('theta1', 'theta2'), 1000)
plot(post)

# plot credible differences in theta1 vs theta2
#
thetadif = post[[1]][,1] - post[[1]][,2]
hist(thetadif,25,main="posterior: theta1-theta2",ylim=c(0,180))
lines(c(0,0),c(0,130),col="black",lty=2,lwd=2)

# mean difference
#
meandif = mean(thetadif)
lines(c(meandif,meandif),c(0,130),col="blue",lty=1,lwd=2)

# 95% credible range for difference between
# theta1 & theta2
#
c95 = quantile(thetadif,c(.025, .975))
lines(c(c95[1],c95[1]),c(0,130),col="magenta",lty=1,lwd=2)
lines(c(c95[2],c95[2]),c(0,130),col="magenta",lty=1,lwd=2)

legend(x="topright", col=c("blue","magenta"), lwd=c(2,2), lty=c(1,1), legend=c(paste("mean = ",round(meandif,2)),paste("95%CR : ", round(c95[1],2), "-", round(c95[2],2))))

# prob thetadif > 0
#
prob_g_0 = length(which(thetadif>0)) / length(thetadif)
text(0, 150, paste("Pr (theta_dif > 0) = ", round(prob_g_0,2)), pos=4)
text(-.1, 145, paste("Pr (theta_dif <= 0) = ", round(1-prob_g_0,2)), pos=1)
