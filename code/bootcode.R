# BOOTSTRAPPING AND RESAMPLING TECHNIQUES

#########################################################################################
# 1. Estimating Population Parameters with Bootstrapping
#########################################################################################

# our sample
X = c(66,79,93,86,69,79,101,97,91,95,72,106,105,75,70,85,92,74,88,93)

# compute a statistic of interest
Xm = mean(X)
Xm

# use resampling to generate an empirical bootstrap distribution of that statistic

# how many simulated experiments?
boot_m = 10000

# create a list to store our bootstrap values
Xm_boot = array(NA, boot_m)

# do it
for (i in 1:10000) {
	Xb = sample(X, length(X), replace=TRUE)	# generate new sample
	Xm_boot[i] = mean(Xb)					# compute statistic of interest
}

# display results
hist(Xm_boot, 50, xlab="Mean", main="bootstrap")
abline(v=Xm, col="red", ,lwd=2)
abline(v=mean(Xm_boot), col="red", lty=2, lwd=2)
legend(x="topright", lty=c(1,2), col=c("red","red"), legend=c("sample","bootstrap"))

# compute 95% CI
CI95 = quantile(Xm_boot, probs=c(.025,.975))
abline(v=CI95[1], lty=2, col="blue", lwd=2)
abline(v=CI95[2], lty=2, col="blue", lwd=2)
legend(x="topleft", lty=2, col="blue", legend="95% CI")

#########################################################################################
# 2. Hypothesis Testing with Permutation Tests
#########################################################################################

# our control group
g_control <- c(87,90,82,77,71,81,77,79,84,86,78,84,86,69,81,75,70,76,75,93)

# our drug group
g_drug <- c(74,67,81,61,64,75,81,81,81,67,72,78,83,85,56,78,77,80,79,74)

# our statistic of interest here is the difference between means
(stat_obs <- mean(g_control) - mean(g_drug))

# how many simulated experiments?
n_perm = 10000

# create a list to store our permutation test values
stat_perm = array(NA, n_perm)

# now do a permutation test to simulate the null hypothesis,
# namely that the control and drug labels are random
g_control_n = length(g_control)
g_drug_n = length(g_drug)
g_bucket = c(g_control, g_drug)
g_bucket_n = length(g_bucket)
for (i in 1:n_perm) {
  # reconstitute both groups, ignoring original labels
  permuted_bucket <- sample(g_bucket,g_bucket_n,replace=FALSE)
  perm_control <- permuted_bucket[1:g_control_n]
  perm_drug <- permuted_bucket[(g_control_n+1):(g_control_n+g_drug_n)]
  stat_perm[i] <- mean(perm_control) - mean(perm_drug)
}

# visualize the empirical permutation distribution of our statistic of interest
hist(stat_perm, 50, xlab="mean(control) - mean(drug)", main="Permutation Test")
abline(v=stat_obs, col="red", lwd=2)

# how many times in the permutation tests did we observe a stat_perm
# as big or bigger than our stat_obs?
(p_perm <- length(which(stat_perm >= stat_obs)) / n_perm)
legend(x="topleft", lty=1, col="red", legend=paste("stat_obs: p = ", p_perm))


#########################################################################################
# 3. Power Calculations
#########################################################################################

# our two groups
g_control <- c(87,90,82,87,71,81,77,79,84,86,78,84,86,69,81,75,70,76,75,93)
g_drug <- c(74,73,81,65,64,72,76,81,81,67,72,78,83,75,66,78,77,80,79,74)

# do a Mann-Whitney U test (nonparametric version of a t-test)
out <- wilcox.test(g_control, g_drug)
w_obs <- out$statistic
p_obs <- out$p.value

n_boot <- 10000
w_boot = array(NA, n_boot)
p_boot = array(NA, n_boot)
for (i in 1:n_boot) {
	b_control <- sample(g_control,length(g_control),replace=TRUE)
	b_drug <- sample(g_drug,length(g_drug),replace=TRUE)
	out <- wilcox.test(b_control, b_drug)
	w_boot[i] <- out$statistic
	p_boot[i] <- out$p.value
}

(power <- length(which(p_boot <= .05)) / n_boot)

hist(log(p_boot), 50, main=paste("p_boot, power=", power), xlab="log(p_boot)")
abline(v=log(0.05), col="red", lty=1, lwd=2)
abline(v=log(p_obs), col="red", lty=2, lwd=2)
legend(x="topleft", col="red", lty=c(1,2), lwd=2, legend=c("p < .05", paste("p_obs (",round(p_obs,3),")")), box.lty=0)

### Parametric Bootstrap:

n_control <- length(g_control)
m_control <- mean(g_control)
sd_control <- sd(g_control)
n_drug <- length(g_drug)
m_drug <- mean(g_drug)
sd_drug <- sd(g_drug)

for (i in 1:n_boot){
	b_control <- rnorm(n_control, mean=m_control, sd=sd_control)
	b_drug <- rnorm(n_drug, mean=m_drug, sd=sd_drug)
	out <- wilcox.test(b_control, b_drug)
	w_boot[i] <- out$statistic
	p_boot[i] <- out$p.value
}

(power <- length(which(p_boot <= .05)) / n_boot)

hist(log(p_boot), 50, main=paste("p_boot, power=", power), xlab="log(p_boot)")
abline(v=log(0.05), col="red", lty=1, lwd=2)
abline(v=log(p_obs), col="red", lty=2, lwd=2)
legend(x="topleft", col="red", lty=c(1,2), lwd=2, legend=c("p < .05", paste("p_obs (",round(p_obs,3),")")), box.lty=0)
