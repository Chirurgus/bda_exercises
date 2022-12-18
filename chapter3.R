# Created by Oleksandr Sorochynskyi
# On 18/05/2021

roundnormlik <- function(x) {
    force(x)
    function(mu, sd) {
        prod(pnorm(x + 0.5, mu, sd) - pnorm(x - 0.5, mu, sd))
    }
}
x <- c(10, 10, 12, 11, 9)
lik <- roundnormlik(x)

library(tidyverse)

exact_lik_df <- expand_grid(
    mu = seq(5, 15, length.out = 100),
    sd = seq(0, 5, length.out = 100)
) %>%
    mutate(
        lik = Vectorize(lik, c("mu", "sd"))(mu, sd)
    )

expand_grid(
    mu = seq(5, 15, length.out = 100),
    sd = seq(0, 5, length.out = 100)
) %>%
    mutate(
        lik = 
    )


lik_df %>%
    ggplot(aes(x = mu, y = sd, z = lik)) +
        geom_contour() +
        geom_vline(xintercept = mean(x)) +
        geom_hline(yintercept = sd(x))


# exercice 6

N_range <- seq(75L, 1000L, by = 1L)
theta_range <- seq(0, 1, length.out = 100)

post <- function(N, theta, y) {
    # Normalizing constant coputed from later
    norm <- 1
    prod(dbinom(y, size = N, prob = theta)) / (N * norm)
}
post_n_theta <- Vectorize(post, vectorize.args = c("N", "theta"))

y <- c(53, 57, 66, 67, 72)
lik_df <- expand_grid(
    N = N_range,
    theta = theta_range
) %>%
    mutate(post = post_n_theta(N, theta, y))


lik_df %>%
    ggplot(aes(x = N, y = theta, z = post)) +
        geom_contour()

post_n <- function(N) {
    res <- integrate(function(theta) post_n_theta(N, theta, y), lower = 0, upper = 1)
    res$value
}
# Asumme 30k is max
sum(sapply(1:100, post_n)) / 3.42371062512831e-09

# exercice 8

data <- tribble(
    ~street,       ~bike_route, ~bycicles, ~other,
    "residential", TRUE,        16,        58,
    "residential", TRUE,        9,         90,
    "residential", TRUE,        10,        48,
    "residential", TRUE,        13,        57,
    "residential", TRUE,        19,        103,
    "residential", TRUE,        20,        57,
    "residential", TRUE,        18,        86,
    "residential", TRUE,        17,        112,
    "residential", TRUE,        35,        273,
    "residential", TRUE,        55,        64,
    "residential", FALSE,       12,        113,
    "residential", FALSE,       1,         18,
    "residential", FALSE,       2,         14,
    "residential", FALSE,       4,         44,
    "residential", FALSE,       9,         208,
    "residential", FALSE,       7,         67,
    "residential", FALSE,       9,         29,
    "residential", FALSE,       8,         154
)

y <- data %>%
    filter(bike_route) %>%
    { .$bycicles / (.$bycicles + .$other)}

z <- data %>%
    filter(!bike_route) %>%
    { .$bycicles / (.$bycicles + .$other)}

# Let's say theta_{b, v} is beta(a, b)
prior_v <- list(mu = 0.5, sd = .28)
prior_b <- list(mu = 0.5, sd = .28)

beta_dens <- function(x, mu, sd, ...) {
    var <- sd^2
    b <- (1 - mu) * ( mu * (1-mu) / var - 1 )
    a <- mu * (1 - mu) / var - b - 1
    dbeta(x, shape1 = a, shape2 = b, ...)
}



posterior_b <- function(mu, sd) {
    prod(beta_dens(y, mu = mu, sd= sd))
}
posterior_v <- function(mu, sd) {
    prod(beta_dens(z, mu = mu, sd= sd))
}
posterior_b <- Vectorize(posterior_b, vectorize.args = c("mu", "sd"))
posterior_v <- Vectorize(posterior_v, vectorize.args = c("mu", "sd"))

grid <- expand_grid(
    mu = seq(0.01, 1, length.out = 1000),
    sd = seq(0.01, 0.25, length.out = 100)
) %>% 
    mutate(
        dens_b = posterior_b(mu, sd),
        dens_v = posterior_v(mu, sd)
    )

grid %>%
    pivot_longer(
        cols = starts_with("dens"),
        names_to = "type",
        names_prefix = "dens_",
        values_to = "dens",
        names_transform = list(type = factor)
    ) %>%
    filter(type == "y") %>%
    ggplot(aes(x = mu, y = sd, z = dens)) +
        geom_contour()
        facet_grid(cols = vars(type))

sample_from_post <- function(n, post) {
    mu_accepted <- c()
    sd_accepted <- c()
    while (length(mu_accepted) < n) {
        mu <- runif(n * 1, min = 0.01, max = .3)
        sd <- runif(n * 1, min = 0.01, max = 0.15)
        y_dens <- post(mu, sd)
        y_dens <- y_dens / max(y_dens)
        accepted <- which(runif(length(mu)) <= y_dens)
        mu_accepted <- c(mu_accepted, mu[accepted])
        sd_accepted <- c(sd_accepted, sd[accepted])
        message(length(accepted))
    }
    res <- tibble(
        mu = mu_accepted,
        sd = sd_accepted
    )
    head(res, n)
}

post_sample <- bind_cols(
    sample_from_post(100, posterior_b),
    sample_from_post(100, posterior_v)
)

post_sample %>% summary()
mean(post_sample %>% {.$mu...1 - .$mu...3})



# exercice 11
library(mvtnorm)

# prior
mu <- c(0, 10)
sigma <- matrix(c(
    4,            0.5 * 2 * 10,
    0.5 * 2 * 10, 100),
    nrow = 2
)

bioassay <- data.frame(
    # dose (log g/ml)
    x = c(−0.86, −0.30, −0.05, 0.73),
    # number of animals
    n = c(5, 5, 5, 5),
    # number of deaths
    y = c(0, 1, 3, 5)
)

# compute density
logit <- function(p) { log(p / (1 - p)) }
logit_inv <- function(x) { 1/(1 + exp(-x)) }
post_dens <- function(alpha, beta, likelihood_only = FALSE, prior_only = FALSE) {
    prior <- dmvnorm(cbind(alpha, beta), mean = mu, sigma = sigma)
    likelihood <- mapply(
        a = alpha,
        b = beta,
        function(a, b) {
            probs <- logit_inv(a + b * bioassay$x)
            prod(dbinom(bioassay$y, size = bioassay$n, prob = probs))
        }
    )
    if (likelihood_only) {
        return(likelihood)
    }
    if (prior_only) {
        return(prior)
    }
    prior * likelihood
}

grid <- expand_grid(
    alpha = seq(-5, 10, length.out = 100),
    beta = seq(-10, 40, length.out = 100)
) %>%
    mutate(
        dens = post_dens(alpha, beta),
        prior = post_dens(alpha, beta, prior_only = TRUE),
        likli = post_dens(alpha, beta, likelihood_only = TRUE)
    )

grid_long <- grid %>%
    pivot_longer(
        cols = -c(alpha, beta),
        names_to = "distr", 
        values_to = "dens"
    )

ggplot(data.frame(), aes(x = alpha, y = beta, z = dens)) +
    geom_contour(data = grid_long %>% filter(distr == "dens"), color = "black") +
    geom_contour(data = grid_long %>% filter(distr == "prior"), color = "green") +
    geom_contour(data = grid_long %>% filter(distr == "likli"), color = "red") +
    coord_cartesian(xlim = c(-2, 5), ylim = c(0, 25))


# exercice 12
# Death rate is number of deaths per 100e6 passanger miles
af <- data.frame(
    year = c(1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985),
    fatal_accidents = c(24, 25, 31, 31, 22, 21, 26, 20, 16, 22),
    deaths = c(734, 516, 754, 877, 814, 362, 764, 809, 223, 1066),
    death_rate = c(0.19, 0.12, 0.15, 0.16, 0.14, 0.06, 0.13, 0.13, 0.03, 0.15)
)

dens <- function(a, b, distr = c("posterior", "likelihood", "prior")) {
    prior <- ifelse(b < 0, 1L, 0L) * dnorm(b, mean = 0, sd = 0.01) * dnorm(a, mean = 25, sd = 5)
    if (distr == "prior") {
        return(prior)
    }
    theta <- a + af$year * b
    lik <- ifelse(
        theta > 0,
        exp(-nrow(af) * theta + sum(af$fa) * log(theta) - sum(lfactorial(af$fa))),
        0
    )
    if (distr == "likelihood") {
        return(lik)
    }
    lik * prior
}

m <- glm(fatal_accidents ~ year, data = af, family = poisson)

af %>%
    ggplot(aes(y = fatal_accidents, x = year)) +
    geom_point()

summary(m)

grid_df <- expand_grid(
    a = 80 - 45 * seq(-2, 2, by = 0.1),
    b = -0.04 - 0.23 * seq(-2, 2, by = 0.01),
) %>%
    mutate(
        post = dens(a, b, distr = "posterior")
    )
grid_df %>% print(n = 100)

b_marginal <- grid_df %>%
    group_by(b) %>%
    summarize(post = sum(post))

b_marginal <- grid_df %>%
    group_by(b) %>%
    summarize(post = sum(post))

b_marginal %>%
    ggplot(aes(x = b, y = post)) +
        geom_line()

library(magrittr)

b <- sample(b_marginal$b, size = 10e3, replace = TRUE, prob = b_marginal$post)
a <- sapply(b, function(.b) grid_df %>% filter(b == !!.b) %$% sample(a, size = 1, replace = TRUE, prob = post))
a <- a + runif(10e3, min = -.1, max = .1)/2
b <- b + runif(10e3, min = -0.01, max = 0.01)/2

tibble(a=a, b=b) %>%
    ggplot(aes(x= a, y =b)) +
        geom_density2d()

hist(b)
hist(a)
hist(a + 1986* b)
summary(rpois(10e3, a + 1986 * b))
quantile(rpois(10e3, a + 1986 * b), c(0.975, 0.025))


    