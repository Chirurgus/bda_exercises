# Created by Oleksandr Sorochynskyi
# On 13/04/2021

prob_N_not_normalized <- function(k) {
    (k >= 202) * (1/k) * 0.99^(k-1) * 0.01
}
normal_constant <- sum(prob_N_not_normalized(seq_len(1e6)))
prob_N <- function(k) {
    prob_N_not_normalized(k) / normal_constant
}

x <- 1:1000
y <- prob_N(x)
plot(x,y)

# mean
sum(x * prob_N(x))
sqrt(sum((x - mu)^2 * prob_N(x)))

prob_N_not_normalized2 <- function(k) {
    (k >= 202) * (k <= 1000) * 1/(k^2)
}
normal_constant2 <- sum(prob_N_not_normalized2(seq_len(1e6)))
prob_N2 <- function(k) {
    prob_N_not_normalized2(k) / normal_constant2
}

x <- 1:1000
y <- prob_N2(x)
plot(x,y)

# mean
sum(x * prob_N2(x))
sqrt(sum((x - mu)^2 * prob_N2(x)))


# Exercice 11

y_given_theta_dens <- function(x, theta) {
    1 / ( 1 + (x - theta)^2)
}
theta_prior_dens <- function(x) {
    ifelse(x >= 0 & x <= 100, 0.01, 0)
}

y_obs <- c(43, 44, 45, 46.5, 47.5)
x <- seq(0, 100, length.out = 1000)

unnorm_post_dens <- function(x) {
    apply(sapply(x, y_given_theta_dens, x = y_obs), 2, prod) *
    theta_prior_dens(x)
}
plot(unnorm_post_dens, xlim = c(0, 100))
norm_c <- integrate(unnorm_post_dens, lower = 0, upper = 100)

post_dens <- function(x) {
    unnorm_post_dens(x) / norm_c$value
}
integrate(post_dens, lower = 0, upper = 100)

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

theta <- sample_from_density(1000, post_dens, range = c(0, 100))
hist(theta, xlim = c(0,100))
unintegr_pred_dens <- function(theta, y_tilde) {
    y_given_theta_dens(y_tilde, theta) * post_dens(theta)
}
pred_dens <- function(y_tilde) {
    sapply(y_tilde, function(x) {
        integrate(unintegr_pred_dens, lower=0, upper = 100, y_tilde = x)$value
    })
}
plot(pred_dens, xlim = c(0, 100))
y_tilde <- sample_from_density(1000, pred_dens, c(0, 100))

y_tilde2 <- sapply(
    theta,
    function(th) {
        sample_from_density(1, y_given_theta_dens, range = c(0, 100), theta = th)
    }
)
hist(y_tilde2)

# exercice 11

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

# exercice 22

library(foreign)

polling_data <- read.dta(
        "data/pew_research_center_june_elect_wknd_data.dta"
    ) %>%
    as_tibble() %>%
    select(ideo, state, party, zipcode, sex, age, educ, income) %>%
    filter(!is.na(ideo))

election_data <- read_csv("data/2008ElectionResult.csv") %>%
    mutate(state = str_to_lower(state))

vl <- polling_data %>%
    group_by(state) %>%
    summarize(very_liberal = mean(ideo == "very liberal"))
vl %>% print(n = 50)

election_data %<>%
    left_join(vl) %>%
    filter(!is.na(very_liberal))

election_data %>%
    ggplot(aes(vote_Obama_pct, very_liberal * 100, label = abbreviate(state))) +
        geom_label()

library(magrittr)

election_data %<>%
    mutate(
        pop = vote_Obama + vote_McCain,
        prior = list(list(shape1 = 2, shape2 = 2))
    ) %>%
    mutate(
        posterior = mapply(
            function(n, s1, s2, p) {
                list(shape1 = s1 + n * p, shape2 = s2 + n * (1 - p))
            },
            n = pop,
            s1 = sapply(prior, getElement, "shape1"),
            s2 = sapply(prior, getElement, "shape2"),
            p = very_liberal,
            SIMPLIFY = FALSE
        )
    ) %>%
    mutate(mean_vl = sapply(posterior, function(ps) ps$shape1 / (ps$shape1 + ps$shape2)))


election_data %>%
    ggplot(aes(mean_vl * 100, vote_Obama_pct, label = abbreviate(state))) +
        geom_label()

