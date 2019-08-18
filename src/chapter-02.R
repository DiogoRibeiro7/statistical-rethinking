library(rethinking)

# 2.4.1 Grid Approximation

num_points <- 20
p_grid <- seq(from=0, to=1, length.out=num_points)
prior <- rep(1, num_points)
# prior <- ifelse( p_grid < 0.5, 0, 1)
# prior <- exp(-5*abs(p_grid - 0.5))
likelihood <- dbinom(6, size=9, prob=p_grid)
unstd_posterior <- prior * likelihood
posterior <- unstd_posterior / sum(unstd_posterior)

plot(p_grid, posterior, type="b",
     xlab="Probability of water", ylab="Posterior",
     main=sprintf("%d points", num_points))

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

# Analytical calculation
w <- 6
n <- 9
curve(dbeta(x, w+1, n-w+1), from=0, to=1)
curve(dnorm(x, mean, stdev), lty=2, add=TRUE)  # Quadratic approximation