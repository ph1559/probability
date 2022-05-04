#############
# Chapter 5.2 (Joint expectation)

## Correlation Coefficients

# R code to visualize correlation coefficients
install.packages("MASS")
library(MASS)

x <- mvrnorm(mu=c(0,0),Sigma=matrix(c(3,1,1,1), 2,2),n=10000)
plot(x[,1], x[,2])
rho = cor(x[,1], x[,2], method=c('pearson'))
print(rho)

# Chapter 5.6 (Vector of random variables)

## Mean Vector

# R code to compute a mean vector
x <- mvrnorm(mu=c(0,0),Sigma=matrix(c(1,0,0,1), 2,2),n=100)
mX <- colMeans(x)

## Covariance Matrix
x <- mvrnorm(mu=c(0,0),Sigma=matrix(c(1,0,0,1), 2,2),n=100)
covX <- cov(x)
print(covX)

## 2D Gaussian
install.packages("emdbook")
library(emdbook)
library(pracma)
X <- mvrnorm(mu=c(0,0),Sigma=matrix(c(0.25,0.3,0.3,1.0), 2,2),n=1000)
x1 <- seq(-2.5, 2.49, 0.01)
x2 <- seq(-3.5, 3.49, 0.01)
X1 <- meshgrid(x1,x2)$X
X2 <- meshgrid(x1,x2)$Y
empty_dim <- c(dim(X1), 2)
Xpos <- array(numeric(), empty_dim)
Xpos[,,1] <- X1
Xpos[,,2] <- X2

F <- dmvnorm(x=cbind(c(Xpos[,,1]), c(Xpos[,,2]))[1:250000,]
             ,mu=c(0,0)
             ,Sigma=matrix(c(0.25,0.3,0.3,1.0), 2,2))

plot(x[,1], x[,2])
contour(x1,x2,matrix(F, 500,500))

# Chapter 5.7 (Transformation of Multi-dimensional Gaussian)

## R code: Gaussian(0,1) --> Gaussian(mu,sigma)
install.packages("powerplus")
library("powerplus")
x <- mvrnorm(mu=c(0,0),Sigma=matrix(c(1,0,0,1), 2,2),n=1000)
mu = c(1,-2)
Sigma = matrix(c(3,-0.5,-0.5,1), 2,2)
Sigma2 = Matpow(covY,0.5)
y = Sigma2 %*% t(x) + t(repmat(mu, 1000, 1))

## R code: Gaussian(mu,sigma) --> Gaussian(0,1)
install.packages("powerplus")
library("powerplus")
y <- mvrnorm(mu=c(1,-2),Sigma=matrix(c(3,-0.5,-0.5,1), 2,2),n=100)
mY = colMeans(y)
covY <- cov(y)
covY2 <- Matpow(covY,-0.5)
x = covY2 %*% t(y-repmat(mY,100,1))

# Chapter 5.8 (Principal component analysis)

## R code to perform the principal component analysis
x <- mvrnorm(mu=c(1,-2),Sigma=matrix(c(3,-0.5,-0.5,1), 2,2),n=1000)
covX <- cov(x)
U <- eigen(covX)$vectors
print(U)

