library(tidyverse)

# Exercice 2

plot_density <- function(d, params, range = c(0, 100)) {
    x <- seq(range[1], range[2], length.out = 1000)
    y <- do.call(d, append(list(x = x), params))
    plot(x,y)
}

sample_from <- function(r, n, params) {
    do.call(r, append(list(n = n), params))
}

# Death rate is number of deaths per 100e6 passanger miles
af <- data.frame (
    year = c(1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985),
    fatal_accidents = c(24, 25, 31, 31, 22, 21, 26, 20, 16, 22),
    deaths = c(734, 516, 754, 877, 814, 362, 764, 809, 223, 1066),
    death_rate = c(0.19, 0.12, 0.15, 0.16, 0.14, 0.06, 0.13, 0.13, 0.03, 0.15)
)

prior <- list(shape = 6, rate = .25)
plot_density(dgamma, prior)

posterior <- list(
    prior$shape + sum(af$fatal_accidents),
    prior$rate + nrow(af)
)
plot_density(dgamma, posterior)

library(HDInterval)
theta <- sample_from(rgamma, 1000, posterior)
pred_fa <- rpois(1000, theta)
hdi(pred_fa)
quantile(pred_fa, c(0.025, 0.95 + 0.025))
hist(pred_fa)


# Now take into account exposure
af$mi <- (af$deaths / af$death_rate) * 100e6
mi_pred <- 8e11
# Miles are in 100 millions
af$mi <- af$mi / 1e11
mi_pred <- mi_pred / 1e11

plot(af$year, af$mi)

# So theta is thus the mean expected number of fatal accients in 100e9 miles
prior_expo <- list(shape = .9, rate = .2)
plot_density(dgamma, prior_expo)
summary(af$fatal_accidents / af$mi)
summary(sample_from(rgamma, 1000, prior_expo))

posterior_expo <- list(
    prior_expo$shape + sum(af$fatal_accidents / af$mi),
    prior_expo$rate + nrow(af)
)
plot_density(dgamma, posterior_expo)

fa_pred_expo <- rpois(1000, mi_pred * sample_from(rgamma, 1000, posterior_expo))
summary(fa_pred_expo)
hdi(fa_pred_expo)

# same thing but for deaths
plot(af$year, af$deaths)
summary(af$deaths)
var(af$deaths)
prior <- list(shape = 700/250, rate = 700/(250^2))
plot_density(dgamma, prior, range = c(0, 1000))
posterior <- list(
    shape = prior$shape + sum(af$deaths),
    rate = prior$rate + nrow(af)
)
plot_density(dgamma, posterior, range = c(0, 1000))
deaths_pred <- rpois(1000, sample_from(rgamma, 1000, posterior))
hdi(deaths_pred)
summary(deaths_pred)

plot(af$mi, af$deaths)
summary(af$deaths / af$mi)
sd(af$deaths / af$mi)
prior_expo <- list(shape = 125/50, rate = 125/2500)
plot_density(dgamma, prior_expo)
summary(sample_from(rgamma, n = 1000, prior_expo))
posterior_expo <- list(
    shape = prior_expo$shape + sum(af$deaths / af$mi),
    rate = prior_expo$rate + nrow(af)
)
plot_density(dgamma, posterior_expo, range = c(0, 200))

deaths_pred2 <- rpois(1000, mi_pred * sample_from(rgamma, 1000, posterior_expo))
hdi(deaths_pred2)



