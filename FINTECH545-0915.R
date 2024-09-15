library(MASS) 
data1 <- read_excel("problem1.xlsx")
x <- data1$x
N <- length(x)

## Problem1
# Manually calculate the first four moments
mean <- sum(x) / N
variance <- sum((x - mean)^2) / (N-1)
skewness <- sum(((x - mean) / sqrt(variance))^3) * N / (N-1) / (N-2)
kurtosis <- sum(((x - mean) / sqrt(variance))^4)/N 
  (N^2/(N-1)^3/(N^2-3*N+3))*((N*(N-1)^2+(6*N-9))*(sum((x - mean)^4)/N +3*(N-1))/N^3)-N*(6*N-9)


# Print calculated moments
cat("Mean:", mean, "\n")
cat("Variance:", variance, "\n")
cat("Skewness:", skewness, "\n")
cat("Kurtosis:", kurtosis, "\n")
# Calculate the first four moments using built-in functions
install.packages("moments")
library(moments)
mean_package <- mean(x)            # First moment: mean
variance_package <- var(x)          # Second moment: variance
skewness_package <- skewness(x)     # Third moment: skewness
kurtosis_package <- kurtosis(x)     # fourth moment: kurtosis
# Print built-in moments
cat("Built-in Mean:", mean_package, "\n")
cat("Built-in Variance:", variance_package, "\n")
cat("Built-in Skewness:", skewness_package, "\n")
cat("Built-in Kurtosis:", kurtosis_package, "\n")




## Problem2
# Load dataset
data2 <-read_excel("problem2.xlsx")

# 2.1.1 Fitting OLS model
ols_model <- lm(y ~ ., data = data2)
summary(ols_model)

# 2.1.2 For MLE, using optim function
log_likelihood <- function(params) {
  beta <- params[1]
  sigma <- params[2]
  x <- as.matrix(data2[ , c("x")])
  y <- data2$y
  n <- length(y)
  
  # Normal distribution likelihood
  residuals <- y - x %*% beta
  ll <- -n/2*log(2*pi) - n/2*log(sigma^2) - sum(residuals^2) / (2*sigma^2)
  return(-ll)  # Minimizing, so return negative log likelihood
}
# Initial guesses for beta values and sigma
init_params <- c(1,5)
# Optimize MLE
mle_fit <- optim(par = init_params, fn = log_likelihood)
mle_fit$par


# 2.2
log_likelihood_t <- function(params) {
  beta <- params[1]  # Coefficients for predictors
  sigma <- params[2]   # Standard deviation of errors
  nu <- params[3]      # Degrees of freedom for t-distribution
  
  x <- as.matrix(data2[, c("x")])  # Predictor matrix
  y <- data2$y  # Response variable
  
  # Calculate residuals (errors)
  residuals <- y - x %*% beta
  
# Log-likelihood using t-distribution (MASS::dt provides the density of the t-distribution)
ll <- sum(dt(residuals / sigma, df = nu, log = TRUE) - log(sigma))
}
# Initial guesses for beta value, sigma, and degrees of freedom (nu)
init_params <- c(0, 1, 5)
# Optimize MLE for t-distribution
mle_t_fit <- optim(par = init_params, fn = log_likelihood_t, method = "L-BFGS-B",
                   lower = c(-Inf, 0.001, 1),  # Lower bounds to prevent invalid values
                   upper = c(Inf, Inf, Inf))     # Upper bounds
cat("Estimated coefficients (Beta):", mle_t_fit$par[1], "\n")
cat("Estimated sigma:", mle_t_fit$par[2], "\n")
cat("Estimated degrees of freedom (nu):", mle_t_fit$par[3], "\n")

# 2.3
library(mvtnorm) # for multivariate normal calculations
library(ggplot2) # for plotting
data2x <-read_excel("C:/Users/Charles/Desktop/problem2_x.xlsx")
# Fit the multivariate normal distribution
mean_vec <- colMeans(data2x)
cov_mat <- cov(data2x)
# Number of observations and variables
n <- nrow(data2x)
p <- ncol(data2x)
# Calculate expected values and 95% confidence intervals for each observation
expected_values <- numeric(n)
confidence_intervals <- matrix(NA, nrow = n, ncol = 2)
for (i in 1:n) {
  x_i <- data2x[i, ]
  # Conditional distribution mean and covariance
  # Placeholder: Assuming mean and covariance are directly the expected values for simplicity
  conditional_mean <- mean_vec
  conditional_var <- cov_mat
  
  # 95% confidence interval (simple example)
  se <- sqrt(diag(conditional_var)) # standard errors
  z_value <- qnorm(0.975)
  lower_bound <- conditional_mean - z_value * se
  upper_bound <- conditional_mean + z_value * se
  
  expected_values[i] <- mean_vec[i] # Expected value (placeholder)
  confidence_intervals[i, ] <- c(lower_bound[i], upper_bound[i])
}
# Create a dataframe for plotting
plot_data <- data.frame(
  Observation = 1:n,
  Expected = expected_values,
  Lower = confidence_intervals[, 1],
  Upper = confidence_intervals[, 2],
  Observed = data2x[, 1] # Change column index to fit your specific analysis
)
# Plot the expected values, confidence intervals, and observed values
ggplot(plot_data, aes(x = Observation)) +
  geom_point(aes(y = x1), color = 'blue', shape = 16) +
  geom_line(aes(y = Expected), color = 'red') +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  labs(title = 'Conditional Distribution Plot',
       x = 'Observation',
       y = 'Value') +
  theme_minimal()



# Problem3
data3 <-read_excel("C:/Users/Charles/Desktop/problem3.xlsx")

# Fit AR and MA models
ar1_model <- arima(data3$x, order = c(1, 0, 0))
ar2_model <- arima(data3$x, order = c(2, 0, 0))
ar3_model <- arima(data3$x, order = c(3, 0, 0))

ma1_model <- arima(data3$x, order = c(0, 0, 1))
ma2_model <- arima(data3$x, order = c(0, 0, 2))
ma3_model <- arima(data3$x, order = c(0, 0, 3))

# Compare models
AIC(ar1_model, ar2_model, ar3_model, ma1_model, ma2_model, ma3_model)

