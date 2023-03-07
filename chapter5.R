library(tidyverse)

sample_from_density <- function(n, dens, range, ...) {
    x_accepted <- c()
    while (length(x_accepted) < n) {
        x <- runif(n * 10, min = range[1], max = range[2])
        y_dens <- dens(x, ...)
        y_dens <- y_dens / max(y_dens)
        x_accepted <- c(x_accepted, x[which(runif(length(x)) <= y_dens)])
    }
    head(x_accepted, n)
}

sat_prep <- tribble(
    ~school, ~y, ~sd,
    "A",     28, 15,
    "B",     8,  10,
    "C",     -3, 16,
    "D",     7,  11,
    "E",     -1, 9,
    "F",     1,  11,
    "G",     18, 10,
    "H",     12, 18
)

tau_post <- function(tau) {
    tau_var <- 1 / (sat_prep$sd^2 + tau^2)
    v_mu <- 1 / sum(tau_var)
    mu_hat <- sum(sat_prep$y * tau_var) / sum(tau_var)
    sqrt(v_mu) * prod(sqrt(tau_var) * exp(-(sat_prep$y - mu_hat)^2 * tau_var / 2))
}
tau_post <- Vectorize(tau_post, "tau")

tau <- sample_from_density(1000, tau_post, c(0, 100))

mu_mean <- sapply(
    tau,
    function(x) {
        st <- 1 / (sat_prep$sd^2 + x^2)
        sum(st * sat_prep$y) / sum(st)
    }
)
mu_var <- sapply(
    tau,
    function(x) {
        st <- 1 / (sat_prep$sd^2 + x^2)
        1 / sum(st)
    }
)

mu <- rnorm(1000, mu_mean, sqrt(mu_var))

theta <- mapply(
    function(m, t) {
        mu_theta <- ((1 / sat_prep$sd^2) * m + (1 / tau^2) * mu) /
            ((1 / sat_prep$sd^2) + (1 / tau^2))
        var_theta <- 1 / ((1 / sat_prep$sd^2) + (1 / tau^2))
        rnorm(1000, mu_theta, sqrt(var_theta))
    },
    sat_prep$y,
    sat_prep$sd
)

# P that school's program is best
fct_count(as.character(), prop = TRUE)
best <- apply(theta, 1, which.max)
fct_count(sat_prep$school[best], prop = TRUE)

better <- outer(
    1:8, 1:8,
    Vectorize(function(i, j) {
        mean(theta[, i] > theta[, j])
    })
)

better2 <- outer(
    1:8, 1:8,
    Vectorize(function(i, j) {
        m <- sat_prep$y[j] - sat_prep$y[i]
        sd <- ifelse(i != j, sqrt(sat_prep$sd[j]^2 + sat_prep$sd[i]^2), 0)
        pnorm(0, m, sd)
    })
)

# exercic 6

y <- c(5.8, 6.6, 7.8, 5.6, 7.0, 7.1, 5.4)
nivada <- 13.9

prior1 <- function(x) {
    d <- dnorm(x, mean = 10, sd = 5) /
        pnorm(0, mean = 10, sd = 5, lower.tail = FALSE)
    ifelse(x >= 0, d, 0)
}
plot(prior1, xlim = c(0, 200))

post1 <- function(x) {
    prod(dnorm(y, mean = x, sd = 1)) * prior1(x)
}
post1 <- Vectorize(post1, "x")

plot(post1, xlim = c(0, 10))


prior2 <- function(x) {
    d1 <- post1(x)
    d2 <- dnorm(x, mean = 20, sd = 3) /
        pnorm(0, mean = 20, sd = 3, lower.tail = FALSE)
    d2 <- ifelse(x >= 0, d2, 0)
    d3 <- dnorm(x, mean = 2, sd = 3) /
        pnorm(0, mean = 2, sd = 3, lower.tail = FALSE)
    d3 <- ifelse(x >= 0, d3, 0)
    6 / 8 * d1 + 1 / 8 * d2 + 1 / 8 * d3
}
plot(prior2, xlim = c(0, 100))


post2 <- function(x) {
    prod(dnorm(y, mean = x, sd = 1)) * prior2(x)
}
post2 <- Vectorize(post2, "x")

plot(post2, xlim = c(0, 10))

# Exercice 8

l <- 0.9
prior <- function(x) {
    l * dnorm(x, 1, 0.5) + (1 - l) * dnorm(x, -1, 0.5)
}

y_mean <- -.25
y_sd <- sqrt(1 / 10)

posterior_numeric_nonorm <- function(x) {
    dnorm(y_mean, x, y_sd) * prior(x)
}
py <- integrate(posterior_numeric_nonorm, lower = -10, upper = 10)
posterior_numeric <- function(x) {
    posterior_numeric_nonorm(x) / py$value
}

plot(prior, xlim = c(-5, 5))
plot(posterior_numeric, xlim = c(-5, 5), add = TRUE, col = "red")
abline(v = 0)

posterior_calc_nonorm <- function(x) {
    m1 <- (-1 / .5^2 * y_mean / y_sd^2) / (1 / .5^2 + 1 / y_sd^2)
    m2 <- (1 / .5^2 * y_mean / y_sd^2) / (1 / .5^2 + 1 / y_sd^2)
    s <- 1 / (1 / .5^2 + 1 / y_sd^2)
    l / dnorm(y_mean, -1, s) * dnorm(x, m1, s) +
        (1 - l) / dnorm(y_mean, 1, s) * dnorm(x, m2, s)
}
const <- integrate(posterior_calc_nonorm, lower = -10, upper = 10)
posterior_calc <- function(x) {
    posterior_calc_nonorm(x) / const$value
}

plot(prior, xlim = c(-5, 5))
plot(posterior_calc, xlim = c(-5, 5), col = "blue")
abline(v = 0)

data <- tribble(
    ~street, ~bike_route, ~bicycle, ~other,
    "residential", TRUE, 16, 58,
    "residential", TRUE, 9, 90,
    "residential", TRUE, 10, 48,
    "residential", TRUE, 13, 57,
    "residential", TRUE, 19, 103,
    "residential", TRUE, 20, 57,
    "residential", TRUE, 18, 86,
    "residential", TRUE, 17, 112,
    "residential", TRUE, 35, 273,
    "residential", TRUE, 55, 64,
)

prior_ab <- function(a, b, log = FALSE) {
    if (log) {
        (-5 / 2) * log(a + b)
    } else {
        (a + b)^(-5 / 2)
    }
    # da <- dexp(a, rate = 1/0.001, log)
    # db <- dexp(b, rate = 1/0.001, log)
    # if (log) {
    #     da + db
    # } else {
    #     da * db
    # }
}

full_posterior <- function(theta, a, b) {
    stopifnot(length(theta) != nrow(data))

    llik <- dbinom(
        data$bicycle,
        size = data$bicycle + data$other,
        prob = theta,
        log = TRUE
    )
    lpost <- prior_ab(a, b, log = TRUE) +
        sum(llik) +
        dbeta(theta, a, b, log = TRUE)
    exp(lpost)
}

hyper_posterior <- function(a, b) {
    lg <- lgamma(a + b) - lgamma(a) - lgamma(b) +
        lgamma(a + data$bicycle) + lgamma(b + data$other) +
        (-1) * lgamma(a + b + data$other + data$bicycle)
    p <- prior_ab(a, b, log = TRUE) + sum(lg)
    exp(p - log(4.802023e-232) / 1.2)
}
hyper_posterior <- Vectorize(hyper_posterior)

integrate(
    function(x) {
        sapply(
            x,
            function(xi) {
                integrate(
                    function(y) {
                        hyper_posterior(xi, y)
                    },
                    lower = 0,
                    upper = Inf
                )$value
            }
        )
    },
    lower = 0,
    upper = Inf
)

expand_grid(
    x = seq(-10, 4, length.out = 100),
    y = seq(-2, 2, length.out = 100)
) %>%
    mutate(
        a = exp(x + y) / (exp(y) + 1),
        b = exp(x) / (exp(y) + 1),
        d = hyper_posterior(a, b)
    ) %>%
    ggplot(aes(x = x, y = y, z = d)) +
    geom_contour_filled()


expand_grid(
    a = seq(0, 10, length.out = 100),
    b = seq(0, 25, length.out = 100)
) %>%
    mutate(
        d = hyper_posterior(a, b)
    ) %>%
    ggplot(aes(x = a, y = b, z = d)) +
    geom_contour_filled()


sample_from_post <- function(n, post, d = 1, lower, upper, ntry = max(10, round(n * 10, -2))) {
    x_accepted <- array(NA, dim = c(0, d))
    while (nrow(x_accepted) < n) {
        x <- array(runif(ntry * d, min = lower, max = upper), dim = c(ntry, d))
        y_dens <- do.call(
            post,
            lapply(apply(x, 2, list), function(x) {
                x[[1]]
            })
        )
        y_dens <- y_dens / max(y_dens)
        accepted <- which(runif(nrow(x)) <= y_dens)
        x_accepted <- rbind(x_accepted, x[accepted, ])
        message(sprintf("Accepted %i samples, %i overall.\n", length(accepted), nrow(x_accepted)))
    }
    x_accepted[head(seq_len(nrow(x_accepted)), n), ]
}

x <- sample_from_post(1000, hyper_posterior, d = 2, lower = c(0, 0), upper = c(100, 100))
theta <- apply(
    x, 1,
    function(hyp) {
        rbeta(10, hyp[1] + data$bicycle, hyp[2] + data$other)
    }
)
theta <- t(theta)



apply(x, 2, summary)
apply(theta, 2, summary)
apply(theta2, 2, summary)
hist(theta[, 1])
hist(x[, 2])
plot(function(y) {
    dbeta(y, shape1 = mean(x[, 1]), shape2 = mean(x[, 2]))
})

sum(data$bicycle) / sum(data$bicycle + data$other)
data %>%
    mutate(
        post = apply(theta, 2, mean),
        empi = bicycle / (other + bicycle)
    )

prop_bicycles <- rbeta(nrow(x), x[, 1], x[, 2])
num_bycicles <- rbinom(nrow(x), size = 100, prob = prop_bicycles)
summary(num_bycicles)
quantile(num_bycicles, c(0, 1) + c(1, -1) * 0.05 / 2)
quantile(prop_bicycles, c(0, 1) + c(1, -1) * 0.05 / 2)

# Exercice 14

y <- data$bicycle + data$other

prior_ab <- function(a, b, log = FALSE) {
    if (log) {
        0
    } else {
        1
    }
}

full_posterior <- function(theta, a, b) {
    prod()
}

hyper_posterior <- Vectorize(function(a, b) {
    lprior <- prior_ab(a, b, log = TRUE)
    lp <- a * log(b) - (a + y) * log(b + 1) +
        lgamma(a + y) - lgamma(a)
    exp(lprior + sum(lp) - 5000 - log(1.4277e-223))
})

expand_grid(
    a = seq(0, 20, length.out = 100),
    b = seq(0, 0.25, length.out = 100)
) %>%
    mutate(
        d = hyper_posterior(a, b)
    ) %>%
    ggplot(aes(x = a, y = b, z = d)) +
    geom_contour_filled()

summary
