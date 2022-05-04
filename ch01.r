# CHAPTER 1

# Figure 1.1
p <- 1/2
n <- seq(1, 10)
X <- p^n
barplot(X, names.arg=1:10)

# Binomial Theorem
n <- 10
k <- 2
choose(n,k)
factorial(k)

# R code to perform an inner product
x <- c(1,0,-1)
y <- c(3,2,0)
z <- t(x) %*% y
print(z)

# R code to compute the norm
x <- c(1,0,-1)
x_norm <- norm(x, type = "2")
# "2" specifies the “spectral” or 2-norm, which is the largest singular value of x
print(x_norm)

# R code to compute the weighted norm
W <- matrix(1:9, ncol = 3, nrow = 3)
x <- matrix(c(2,-1,1))
z <- t(x) %*% (W %*% x)
print(z)

# R code to compute a matrix inverse
X <- matrix(c(1,-2,0,3,7,1), nrow = 3, ncol = 2)
XtX <- t(X) %*% X
Xtxinv <- solve(XtX)
print(Xtxinv)

# R code to solve X beta = y
X <- matrix(c(1,-2,0,3,7,1), nrow = 3, ncol = 2)
y <- matrix(c(2,1,0))
beta <- solve(qr(X), y)
print(beta)
