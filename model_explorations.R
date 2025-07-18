## MA simulation

set.seed(123)
n = 1000
phi = 0.5
sigma = 1

e = rnorm(n, 0, sigma)
y = numeric(n)
y[1] = e[1]
for (i in 2:n) {
  y[i] = e[i] + phi * e[i-1]
}

par(mfrow = c(1, 1))
plot(y, type = 'l')

acf(y)
pacf(y)

# Fit model

X <- ts(y)

fit <- Arima(X, order = c(0, 0, 1))
summary(fit)

acf(residuals(fit), lag.max = 20, col = 'blue', lwd = 2)
pacf(residuals(fit), lag.max = 20, col = 'blue', lwd = 2)

## AR simulation

y = numeric(n)
phi = 0.5
sigma = 1

e = rnorm(n, 0, sigma)
y[1] = 0 + e[1]
for (i in 2:n) {
  y[i] = phi * y[i-1] + e[i]
}

plot(y, type = 'l', col = 'blue', lwd = 2, ylab = 'y', xlab = 't')
abline(h = 0, col = 'red', lty = 2)

acf(y, lag.max = 20, col = 'blue', lwd = 2)
pacf(y, lag.max = 20, col = 'blue', lwd = 2)

# Fit model

X <- ts(y)

fit <- Arima(X, order = c(1, 0, 0))
summary(fit)

acf(residuals(fit), lag.max = 20, col = 'blue', lwd = 2)
pacf(residuals(fit), lag.max = 20, col = 'blue', lwd = 2)

## ARMA simulation

y = numeric(n)
phi = 0.5
theta = 0.5
sigma = 1

e = rnorm(n, 0, sigma)
y[1] = 0 + e[1]
for (i in 2:n) {
  y[i] = phi * y[i-1] + e[i] + theta * e[i-1]
}

plot(y, type = 'l', col = 'blue', lwd = 2, ylab = 'y', xlab = 't')
abline(h = 0, col = 'red', lty = 2)

acf(y, lag.max = 20, col = 'blue', lwd = 2)
pacf(y, lag.max = 20, col = 'blue', lwd = 2)

# Fit model

X <- ts(y)

fit <- Arima(X, order = c(1, 0, 1))
summary(fit)

acf(residuals(fit), lag.max = 20, col = 'blue', lwd = 2)
pacf(residuals(fit), lag.max = 20, col = 'blue', lwd = 2)

## GARCH Simulation

library(rugarch)

# ARCH(1) Simulation

n <- 1000
alpha0 <- 0.2
alpha1 <- 0.5

y <- numeric(n)
sigma2 <- numeric(n)
e <- rnorm(n, 0, 1)

sigma2[1] <- 1
y[1] <- e[1] * sqrt(sigma2[1])
for (i in 2:n) {
  sigma2[i] <- alpha0 + alpha1*y[i-1]^2
  y[i] <- e[i] * sqrt(sigma2[i])
}

plot(y, type = 'l')
# no autocorrelation in the series
acf(y, lag.max = 20)
pacf(y, lag.max = 20)
# autocorrelation in the squared series
acf(y^2, lag.max = 20)
pacf(y^2, lag.max = 20)

# Fit Arima model
X <- ts(y^2)
fit <- Arima(X, order = c(1, 0, 0))
summary(fit)
acf(residuals(fit))
pacf(residuals(fit))

# Fit GARCH model
X <- ts(y)
spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
fit <- ugarchfit(spec = spec, data = X)
fit
acf(residuals(fit))
pacf(residuals(fit))


# GARCH(1, 1) Simulation

# y_t = e_t * sqrt(sigma_t^2)
# sigma_t^2 = alpha0 + alpha1*y_{t-1}^2 + beta1*sigma_{t-1}^2

n <- 1000
alpha0 <- 0.2
alpha1 <- 0.5
beta1 <- 0.3

y <- numeric(n)
sigma2 <- numeric(n)
e <- rnorm(n, 0, 1)

sigma2[1] <- 1
y[1] <- e[1] * sqrt(sigma2[1])
for (i in 2:n) {
  sigma2[i] <- alpha0 + alpha1*y[i-1]^2 + beta1*sigma2[i-1]
  y[i] <- e[i] * sqrt(sigma2[i])
}

plot(y, type = 'l')

# No autocorrelation in the series
acf(y, lag.max = 20)
pacf(y, lag.max = 20)
# autocorrelation in the squared series
acf(y^2, lag.max = 20)
pacf(y^2, lag.max = 20)

# Fit GARCH model

X <- ts(y)

spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0)))
fit <- ugarchfit(spec = spec, data = X)
fit

acf(residuals(fit))
pacf(residuals(fit))

## GARCH(1, 1) with AR(1) Simulation

# y_t = phi*y_{t-1} + epsilon_t
# epsilon_t = e_t * sqrt(sigma_t^2)
# sigma_t^2 = alpha0 + alpha1*epsilon_{t-1}^2 + beta1*sigma_{t-1}^2
# e_t ~ N(0, 1)

set.seed(123)
n <- 1000
alpha0 <- 0.2
alpha1 <- 0.5
beta1 <- 0.3
phi <- 0.5

y <- numeric(n)
sigma2 <- numeric(n)
e <- rnorm(n, 0, 1)
eps <- numeric(n)

sigma2[1] <- 1
eps[1] <- e[1] * sqrt(sigma2[1])
y[1] <- eps[1]
for (i in 2:n) {
  sigma2[i] <- alpha0 + alpha1*eps[i-1]^2 + beta1*sigma2[i-1]
  eps[i] <- e[i] * sqrt(sigma2[i])
  y[i] <- phi*y[i-1] + eps[i]
}

plot(y, type = 'l')

acf(y)
pacf(y)
acf(y^2)
pacf(y^2)

# Fit AR(1) Model

X <- ts(y)
fit <- Arima(X, order = c(1, 0, 0))
summary(fit)

acf(residuals(fit))
pacf(residuals(fit))
plot(residuals(fit))

# Fit GARCH(1, 1) AR(1) model

spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 0)))

fit <- ugarchfit(spec = spec, data = X)
fit

acf(residuals(fit))
pacf(residuals(fit))
plot(residuals(fit))

## GARCH(1, 1) with ARMA(1, 1) Simulation

# y_t = phi*y_{t-1} + theta*epsilon_{t-1} + epsilon_t
# epsilon_t = e_t * sqrt(sigma_t^2)
# sigma_t^2 = alpha0 + alpha1*epsilon_{t-1}^2 + beta1*sigma_{t-1}^2
# e_t ~ N(0, 1)

n <- 10000
alpha0 <- 0.2
alpha1 <- 0.5
beta1 <- 0.3
phi <- 0.25
theta <- 0.5
mu <- 2

y <- numeric(n)
sigma2 <- numeric(n)
e <- rnorm(n, 0, 1)
eps <- numeric(n)

sigma2[1] <- 1
eps[1] <- e[1] * sqrt(sigma2[1])
y[1] <- mu + eps[1]
for (i in 2:n) {
  sigma2[i] <- alpha0 + alpha1*eps[i-1]^2 + beta1*sigma2[i-1]
  eps[i] <- e[i] * sqrt(sigma2[i])
  y[i] <- mu + phi*y[i-1] + theta*eps[i-1] + eps[i]
}

plot(y, type = 'l')

acf(y)
pacf(y)
acf(y^2)
pacf(y^2)

# Fit ARMA(1, 1) Model

X <- ts(y)
fit <- Arima(X, order = c(1, 0, 1))
summary(fit)

acf(residuals(fit))
pacf(residuals(fit))

# Fit GARCH(1, 1) model

spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 1)))

fit <- ugarchfit(spec = spec, data = X)
fit
confint(fit)

acf(residuals(fit))
pacf(residuals(fit))




