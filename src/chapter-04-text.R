library(rethinking)

data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18, ]

# Grid approximation of posterior
mu.list <- seq(from=140, to=160, length.out=200)
sigma.list <- seq(from=4, to=9, length.out=200)
posterior <- expand.grid(mu=mu.list, sigma=sigma.list)
posterior$log.likelihood <- sapply(
  1:nrow(posterior),
  function(i) sum(dnorm(d2$height,
                        mean=posterior$mu[i],
                        sd=posterior$sigma[i],
                        log=TRUE))
)
posterior$log <- posterior$log.likelihood +
  dnorm(posterior$mu, 178, 20, TRUE) +
  dunif(posterior$sigma, 0, 50, TRUE)
posterior$probability <- exp(posterior$log - max(posterior$log))

sample.rows <- sample(1:nrow(posterior),
                      size=1e4,
                      replace=TRUE,
                      prob=posterior$probability)
sample.mu <- posterior$mu[sample.rows]
sample.sigma <- posterior$sigma[sample.rows]

# Quadratic approximation of posterior
model.1 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 20),
    sigma ~ dunif(0, 50)
    ),
  data=d2
)
print("Weak prior on mu:")
print(precis(model.1))

model.2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1),  # Stronger prior on mu
    sigma ~ dunif(0, 50)
  ),
  data=d2
)
print("Strong prior on mu:")
print(precis(model.2))
