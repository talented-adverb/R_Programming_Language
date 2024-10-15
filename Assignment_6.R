#1 i 

# library
library(pracma)

# Define the joint probability density function using vectorized conditions
f_joint <- function(x, y) {
  ifelse(x >= 0 & x <= 1 & y >= 0 & y <= 1,2 * (2 * x + 3 * y) / 5, 0)
}

# (i) Check if it is a valid joint density function
# The joint density must integrate to 1 over [0,1] x [0,1]
is_joint_density <- dblquad(f_joint, 0, 1, 0, 1)
is_joint_density  # Should return 1 if valid

#1 ii
# g(x) = integral of f(x, y) over y from 0 to 1
g_x <- function(x) {
  integral(function(y) f_joint(x, y), 0, 1)
}

# Find g(x) at x = 1
g_x_at_1 <- g_x(1)
g_x_at_1  # This will give the value of g(1)

#1 iii
h_at_0<-integral(function(x) f_joint(x,0),0,1)
h_at_0

#1 iv
expected_value_xy<- integral2(function(x, y) x * y * f_joint(x, y), 0, 1, 0, 1)
expected_value_xy

#2

#i
# Define the matrix for joint PMF
pmf_matrix <- matrix(c(0.05, 0.10, 0.15,
                       0.10, 0.20, 0.05,
                       0.15, 0.05, 0.10,
                       0.05, 0.05, 0.05), byrow = TRUE, nrow = 4, ncol = 3)

# Assign names to the matrix
rownames(pmf_matrix) <- c(0, 1, 2, 3)
colnames(pmf_matrix) <- c(0, 1, 2)

# (i) Display the joint mass function in rectangular (matrix) form
print(pmf_matrix)
# (ii) Check that it is a joint mass function or not?
pmf_sum <- sum(pmf_matrix)
print(pmf_sum)  # Should be 1 for a valid joint PMF
# (iii) Find the marginal distribution g(x) for x = 0, 1, 2, 3
g_x <- apply(pmf_matrix, 1, sum)
print(g_x)
# (iv) Find the marginal distribution h(y) for y = 0, 1, 2
h_y <- apply(pmf_matrix, 2, sum)
print(h_y)
# (v) Find the conditional probability at x = 0 given y = 1
p_x0_given_y1 <- pmf_matrix["0", "1"] / h_y["1"]
print(p_x0_given_y1)

# (vi) Find E(x), E(y), E(xy), Var(x), Var(y), Cov(x, y) and its correlation coefficient
# Defining the random variables
x_values <- 0:3
y_values <- 0:2

# Expectation of X and Y
E_x <- sum(x_values * g_x)
E_y <- sum(y_values * h_y)

# Expectation of XY
E_xy <- sum(row(pmf_matrix) * col(pmf_matrix) * pmf_matrix)

# Variance of X and Y
Var_x <- sum(((x_values - E_x)^2) * g_x)
Var_y <- sum(((y_values - E_y)^2) * h_y)

# Covariance of X and Y
Cov_xy <- E_xy - (E_x * E_y)

# Correlation coefficient
correlation_coefficient <- Cov_xy / (sqrt(Var_x * Var_y))

# Print results
cat("E(x) =", E_x, "\nE(y) =", E_y, "\nE(xy) =", E_xy, "\nVar(x) =", Var_x, "\nVar(y) =", Var_y, "\nCov(x, y) =", Cov_xy, "\nCorrelation Coefficient =", correlation_coefficient, "\n")
