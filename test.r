# Write some basic R code to test the installation. 

# Load the library
install.packages("rstan")
library("rstan")

# Define the data
data <- list(N = 100, y = rnorm(100))

# Define the model
model <- stan_model(model_code = "data { int N; vector[N] y; } parameters { real mu; real<lower=0> sigma; } model { y ~ normal(mu, sigma); }")

# Fit the model
fit <- sampling(model, data = data, chains = 4, iter = 1000, seed = 123)

# Print the summary
print(fit)


