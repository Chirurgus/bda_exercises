x <- rnorm(10e3, 4, 1)
y <- rnorm(10e3, 3, 2)
summary(y/x)
sd(y/x)
z <- y/x

n <- seq_len(length(x))
xysd <- sapply(n, function(i) { sd(head(z, i)) })
plot(n, xysd, lty=1)

mysd <- function(m1, m2, sd1, sd2) {
    msq1 <- sd1^2 - m1^2
    msq2 <- sd2^2 - m2^2
    sqrt(msq1/msq2 - (m1/m2)^2)
}

mysd(4, 3, 1, 2)

# Exercice 10

bioassay <- data.frame(
    # dose (log g/ml)
    x = c(−0.86, −0.30, −0.05, 0.73),
    # number of animals
    n = c(5, 5, 5, 5),
    # number of deaths
    y = c(0, 1, 3, 5)
)

ab_hat <- function(x, n, y) {
    res <- glm(
        y/n ~ x,
        weights = n,
        family = binomial()
    )
    res$coefficients[c(2,1)]
}

ab <- ab_hat(bioassay$x, bioassay$n, bioassay$y)

info_matrix <- function(x, n, y, a, b) {
    e <- exp(a*x + b)
    l <- (n * e) / (1 + e)^2 
    m11 <- sum(l)
    m12 <- sum(x * l)
    m22 <- sum(x^2 * l)
    matrix(c(m11, m12, m12, m22), ncol = 2)
}
fi <- info_matrix(bioassay$x, bioassay$n, bioassay$y, ab[1], ab[2])
v <- solve(fi)

qnorm(c(0.025, 1 - 0.025), mean = ab[1], sd = sqrt(v[1,1]/nrow(bioassay)))
qnorm(c(0.025, 1 - 0.025), mean = ab[2], sd = sqrt(v[2,2]/nrow(bioassay)))

boot <- replicate(
    1e3,
    sample.int(nrow(bioassay), size = 1e3, replace = TRUE),
    simplify = FALSE
)
ab_boot <- sapply(
    boot,
    function(i) {
        d <- bioassay[i, ]
        ab_hat(d$x, d$n, d$y)
    }
)

quantile(ab_boot[1,], c(0.025, 1 - 0.025))
quantile(ab_boot[2,], c(0.025, 1 - 0.025))

# Exercice 15

q25 <- function(n) {
    ((1 - qnorm(0.25) * sqrt(4/(4*n + 1))) * (n + 1/4) - n) / sqrt(1/n)
}
q75 <- function(n) {
    ((1 + qnorm(0.25) * sqrt(4/(4*n + 1))) * (n + 1/4) - n) / sqrt(1/n)
}
# P true value inside
p_cover <- function(n) {
    1 - pnorm(q25(n)) * (1 - pnorm(q75(n)))
}
plot(p_cover, xlim = c(0, 100))
