#ans-1(a)
1-punif(45,0,60)
#ans-1(b)
punif(30,0,60)-punif(20,0,60)

lambda<-1/2

#ans-2(a)
dexp(3,lambda)

#ans-2(b)
x_vals <- seq(0, 5, 0.0001)
y_vals <- dexp(x_vals, rate = lambda)

plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2,
     main = "Exponential Distribution (PDF)",
     xlab = "x", ylab = "Density")

#ans-c
prob_at_most_3 <- pexp(3, rate = lambda)
prob_at_most_3

#ans-d
cdf_vals <- pexp(x_vals, rate = lambda)

plot(x_vals, cdf_vals, type = "l", col = "red", lwd = 2,
     main = "Cumulative Exponential Distribution (CDF)",
     xlab = "x", ylab = "Cumulative Probability")

#ans-e
# Set the rate parameter λ = 1/2
lambda <- 1/2

# Set the rate parameter λ = 1/2
lambda <- 1/2

# (e) Simulate 1000 exponential distributed random numbers
set.seed(123)  # for reproducibility
simulated_data <- rexp(1000, rate = lambda)

# Plot histogram of the simulated data
hist(simulated_data, breaks = 30, col = "lightblue", border = "black",
     main = "Histogram of Simulated Exponential Data",
     xlab = "Repair Time (hours)", ylab = "Frequency")

# Parameters of the Gamma distribution
alpha <- 2    # shape parameter
beta <- 1/3   # rate parameter

# (a) (i) Find the value of the PDF at x = 3
pdf_at_3 <- dgamma(3, shape = alpha, rate = beta)
pdf_at_3

# (a) (ii) Probability that the lifetime is at least 1 unit of time: P(X >= 1)
prob_at_least_1 <- 1 - pgamma(1, shape = alpha, rate = beta)
prob_at_least_1

# (b) Find the value of c such that P(X <= c) >= 0.70
c_value <- qgamma(0.70, shape = alpha, rate = beta)
c_value

