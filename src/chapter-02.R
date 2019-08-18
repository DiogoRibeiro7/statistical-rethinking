library(rethinking)

n <- 9  # Number of data points
w <- 6  # Number of data points that are water

# 2.4.1 Grid Approximation

num_points <- 20
p_grid <- seq(from=0, to=1, length.out=num_points)
prior <- rep(1, num_points)
# prior <- ifelse( p_grid < 0.5, 0, 1)
# prior <- exp(-5*abs(p_grid - 0.5))
likelihood <- dbinom(w, size=n, prob=p_grid)
unstd_posterior <- prior * likelihood
posterior <- unstd_posterior / sum(unstd_posterior)

pdf("output/chapter-02.output-01.pdf")
plot(p_grid, posterior, type="b",
     xlab="Probability of water", ylab="Posterior")
mtext(sprintf("%d points", num_points))
dev.off()

# 2.4.2 Quadratic Approximation

globe.qa <- map(
  alist(
    w ~ dbinom(9, p),  # Binomial likelihood
    p ~ dunif(0, 1)    # Uniform prior
  ),
  data=list(w=6)
)

print(precis(globe.qa))
mean <- attr(globe.qa, "coef")
stdev <- sqrt(attr(globe.qa, "vcov"))

# Analytical calculation: Beta-Binomial conjugate model

pdf("output/chapter-02.output-02.pdf")
curve(dbeta(x, w+1, n-w+1), from=0, to=1,
      xlab="Probability of water", ylab="Posterior or Approximation")
curve(dnorm(x, mean, stdev), lty=2, add=TRUE)
mtext("(Beta) Posterior and (Normal) Quadratic Approximation")
legend(0.02, 2.7, legend=c("Posterior", "Quadratic Approximation"), lty=1:2)
dev.off()
