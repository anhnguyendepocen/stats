# NOTE: for some reason on the Mac, you cannot run doMC() functions in the
#       GUI version of R. Instead, you have to run R in a Terminal

library(doMC)
registerDoMC()

nreps <- 10000	# number of simulated experiments
n <- 10			# subjects per group
mupop <- 100	# population mean
sigmapop <- 30	# population sd
alpha <- 0.05	# our chosen alpha level for rejecting H0

# setup grouping variable
grp <- as.factor(c(rep("a",n),rep("b",n),rep("c",n)))

errPL <- 0			# type-I errors planned comps
errPH <- 0			# type-I errors chase biggest difference
gd <- rbind(c(1,2), c(1,3), c(2,3)) # ordering of difference tests
mcoptions <- list(preschedule=TRUE)
stime <- system.time({
out <- foreach (1:nreps, .combine=rbind, .options.multicore=mcoptions) %dopar%
{
	# randomly sample from same population
	g1 <- rnorm(n, mupop, sigmapop);
	g2 <- rnorm(n, mupop, sigmapop);
	g3 <- rnorm(n, mupop, sigmapop);

	dv <- c(g1,g2,g3)
	allg <- cbind(g1,g2,g3)

	# perform single factor between subjects anova
	aov.mod <- aov(dv ~ grp)
	aov.sum <- summary(aov.mod)
	p.val <- aov.sum[[1]]$"Pr(>F)"[1] # extract p-value
	msw <- aov.sum[[1]]$"Mean Sq"[2] # mean-square within
	dfdenom <- aov.sum[[1]]$"Df"[2]

	# precompute differences between means
	g12 <- mean(g1)-mean(g2)
	g13 <- mean(g1)-mean(g3)
	g23 <- mean(g2)-mean(g3)
	gdiffs <- c(g12,g13,g23)
	
	# planned comparisons
	# always test (g1 vs g2) and (g1 vs g3) 
	# if omnibus anova is significant
	pcomp <- c()
	if (p.val <= alpha) # if omnibus anova was significant
	{
		# test g1 vs g2
		Fcomp <- n*((g12)^2) / (2*msw)
		pcomp <- pf(Fcomp, 1, dfdenom, lower.tail=FALSE) 
		# count errors
		errPL <- errPL + (pcomp <= alpha);
		# test g1 vs g2
		Fcomp <- n*((g13)^2) / (2*msw)
		pcomp <- pf(Fcomp, 1, dfdenom, lower.tail=FALSE) 
		# count errors
		errPL <- errPL + (pcomp <= alpha);
	}

	# posthoc comparison
	# test the two biggest pairwise differences observed
	gdiffsS <- sort(abs(gdiffs), decreasing=TRUE, index.return=TRUE)
	# choose biggest difference
	pcomp <- c()
	if (p.val <= alpha) # if omnibus anova was significant
	{
		# test biggest difference
		Fcomp <- n*((gdiffsS$x[1])^2) / (2*msw)
		pcomp <- pf(Fcomp, 1, dfdenom, lower.tail=FALSE) 
		# count errors
		errPH <- errPH + (pcomp <= alpha);	
		
		# test second biggest difference
		Fcomp <- n*((gdiffsS$x[2])^2) / (2*msw)
		pcomp <- pf(Fcomp, 1, dfdenom, lower.tail=FALSE) 
		# count errors
		errPH <- errPH + (pcomp <= alpha);	
	}
	c(errPL, errPH)
}})

errPL <- sum(out[,1])
errPH <- sum(out[,2])
cat("errPL=",errPL/nreps,"; errPH=",errPH/nreps,"\n")
cat("ran on",getDoParWorkers(),"parallel cores\n")
print(stime)



