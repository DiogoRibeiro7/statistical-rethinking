library(rethinking)
data(homeworkch3)

# 3H1
num_boys <- sum(c(birth1, birth2))
num_girls <- length(c(birth1, birth2)) - num_boys

p <- seq(from=0, to=1, length.out=1000)
posterior <- p^num_boys * (1-p)^num_girls
max_posterior <- max(posterior)
argmax_posterior <- which.max(posterior)

# 3H2
samples <- sample(p, 10000, prob=posterior, replace=TRUE)
for (prob in c(0.5, 0.89, 0.97)){
  print(HPDI(samples, prob=prob))
}

# 3H3, 4
posterior_predictive <- rbinom(10000, 200, samples)

# sum(birth1) = 51, which is so far left that it is not plotted!
pdf("output/chapter-03.output-01.pdf")
dens(posterior_predictive)
abline(v=c(111, sum(birth1)), col=c("red", "blue"))
dev.off()

# 3H5
boys_after_girls <- rep(0, 10000)
for (i in 1:10000) {
  simulated_birth1 <- rbinom(100, 2, samples)
  simulated_birth2 <- rbinom(100, 2, samples)
  boys_after_girls[i] <- sum(!simulated_birth1 & simulated_birth2)
}

# sum(!birth1 & birth2) = 39, which is so far right that it is not plotted!
pdf("output/chapter-03.output-02.pdf")
hist(boys_after_girls)
abline(v=sum(!birth1 & birth2), col="red")
dev.off()

# Conclusion: sexes of first and second births are correlated, which is not being modelled.
