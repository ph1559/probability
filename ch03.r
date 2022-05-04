#############
# Chapter 3.2 

## Histogram of the English alphabet

# R code to generate the histogram
f <- scan('./ch3_data_english.txt')
barplot(f/100 ~ letters, cex.names=0.8)   
grid()

## Histogram of the throwing a die

# R code generate the histogram
q <- sample(1:6, 100, replace = T)
hist(q + 0.5, 6)

## Histogram of an exponential random variable 

# R code used to generate the plots
lambda <- 1
k <- 1000
X <- rexp(k, rate=lambda)
hist(X, 200)

## Cross validation loss

# R code to perform the cross validation
lambda <- 1
n <- 1000
X <- rexp(n, rate=lambda)
m <- 6:200
J <- numeric(195)

for (i in 1:195) {
    num <- hist(X, breaks=seq(0, max(X), l=m[i]), plot=FALSE)$counts
    h <- n/m[i]
    J[i] = 2/((n-1)*h)-((n+1)/((n-1)*h))*sum((num/n)^2) 
}

plot(m, J, type="l", lwd=3)

#############
# Chapter 3.4 

## Mean of a vector 

# R code to compute the mean of a dataset
X <- runif(n=10000, min=0, max=1)
print(mean(X))

## Mean from PMF 

# R code to compute the expectation
p = c(0.25, 0.5, 0.25)
x = c(0, 1, 2)
EX = sum(p*x)
print(EX)

## Mean of a geometric random variable

# R code to compute the expectation
k = c(1:100)
p = 0.5 ^ k
EX = sum(p*k)
print(EX)

#############
# Chapter 3.5

## Bernoulli Random Variables
# R code to generate 1000 Bernoulli random variables
p <- 0.5
n <- 1
X <- rbinom(1000, n, p)
hist(X)

## Erdos Renyi Graph

# R code to generate Erdos Renyi Graph
library(pracma)
library(igraph)
A = runif(40*40)
A = matrix(A, 40, 40) < 0.3
A = triu(A, 1)
A = A + t(A)
G = graph_from_adjacency_matrix(A)
plot(G, axes=TRUE,  main="p = 0.3")

## Binomial Random Variables

# R code to generate 5000 Binomial random variables
p <- 0.5
n <- 10
X <- rbinom(5000, n, p)
hist(X)

## Binomial CDF 

# R code to plot CDF of a binomial random variable
p <- 0.5
n <- 10
x <- 0:n
plot(x, pbinom(x, size = n, prob = p), type="s")

## Poisson CDF

# R code to plot the Poisson PMF
library(pracma)
lambda_set = c(1, 4, 10)
p = zeros(21, 3)
k = seq(0, 20)
for (i in 1:3) {
    p[,i] = dpois(k, lambda_set[i])
}

plot(k, p[,1], type='h', lwd=3, col="blue")
grid()
legend(15, 0.36, legend=c("λ = 1", "λ = 4", "λ = 10"), col=c("blue", "darkgreen", "orange"), lwd=3)
lines(k, p[,1], type='p', cex=2, pch=16, col="blue")
lines(k, p[,2], type='h', lwd=3, col="darkgreen")
lines(k, p[,2], type='p', cex=2, pch=16, col="darkgreen")
lines(k, p[,3], type='h', lwd=3, col="orange")
lines(k, p[,3], type='p', cex=2, pch=16, col="orange")

# R code to plot the Poisson CDF 
library(pracma)
lambda_set = c(1, 4, 10)
p = zeros(21, 3)
k = seq(0, 20)
for (i in 1:3) {
    p[,i] = ppois(k, lambda_set[i])
}

plot(k, p[,3], type='s', lwd=3, col="orange")
grid()
legend(15, 0.36, legend=c("λ = 1", "λ = 4", "λ = 10"), col=c("blue", "darkgreen", "orange"), lwd=3)
lines(k, p[,3], type='p', cex=2, pch=16, col="orange")
lines(k, p[,2], type='s', lwd=3, col="darkgreen")
lines(k, p[,2], type='p', cex=2, pch=16, col="darkgreen")
lines(k, p[,1], type='s', lwd=3, col="blue")
lines(k, p[,1], type='p', cex=2, pch=16, col="blue")

## Poisson-Binomial Approximation

# R code to approximate binomial using Poisson
n = 5000
p = 0.01
lambda = n*p
x = 0:120
y = dbinom(x, n, p)
z = dpois(x, lambda)

plot(x, y, lwd=2, type="h", col="darkolivegreen3", xlab="k", ylab="Probability")
legend(63, 0.053, legend=c("Binomial, n=5000, p=0.01", "Poisson, λ = 50"), col=c("darkolivegreen3", "blue"), lwd=3)
grid()
lines(x, y, lwd=2, cex=1, type="p", col="darkolivegreen3")
lines(x, z, lwd=2, type="l", col="blue")

## Photon shot noise 

# R code to demonstrate the photon shot noise 
library(imager)
x = as.data.frame(load.image("cameraman.tif"))
x = xtabs(value ~ x+y, data=x)
width = sqrt(length(x))

alpha = 10
lambda = alpha * x
X1 = rpois(length(lambda), lambda)
X1 = array(X1, c(width, width))

alpha = 100
lambda = alpha * x
X2 = rpois(length(lambda), lambda)
X2 = array(X2, c(width, width))

alpha = 1000
lambda = alpha * x
X3 = rpois(length(lambda), lambda)
X3 = array(X3, c(width, width))

# Flip matrix since `image()` reads the matrix bottom up
flip_matrix = function(m) m[,nrow(m):1]

# Plot images together
layout(matrix(1:3, 1, 3), respect=TRUE)
image(flip_matrix(X1), col=gray.colors(255), main = expression(paste(alpha, " = 10")))
image(flip_matrix(X2), col=gray.colors(255), main = expression(paste(alpha, " = 100")))
image(flip_matrix(X3), col=gray.colors(255), main = expression(paste(alpha, " = 1000")))

