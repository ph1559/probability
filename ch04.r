#############
# Chapter 4.3 (Cumulative Distribution Function)

## CDF of a Gaussian Mixture

# R code to generate the PDF and CDF
library(EnvStats)

x = seq(-5, 10, (5+10)/1000)
x = sort(x)
f = dnormMix(x, 0, 1, 5, 1, p.mix=c(0.7))
plot(x,f, xlim=c(-5,10), ylim=c(0,0.4), type="n")
lines(x, f, lwd=4, col="blue")
polygon(c(min(x), x[x<=0.8]), c(f[x<=0.8], 0), col="lightblue")
grid()

F = pnormMix(x, 0, 1, 5, 1, p.mix=c(0.7))
plot(x, F, type="n")
lines(x, F, lwd=4, col="blue")
grid()

## CDF of a Uniform random variable

# R code to generate the PDF and CDF
x = seq(-5, 10, (5+10)/1500)
f = dunif(x, -3, 4)
F = punif(x, -3, 4)
# PDF
plot(x, f, type="n")
lines(x, f, lwd=5, col="blue")
grid()

# CDF
plot(x, F, type="n")
lines(x, F, lwd=5, col="blue")
grid()

## CDF of an exponential random variable

# R code to generate the PDF and CDF
x = seq(-5, 10, (5+10)/1500)
f = dexp(x, 1/2)
F = pexp(x, 1/2)
# PDF
plot(x, f, type="n")
lines(x, f, lwd=5, col="blue")
grid()

# CDF
plot(x, F, type="n")
lines(x, F, lwd=5, col="blue")
grid()

#############
# Chapter 4.5

## Generate a uniform random variable

# R code to generate 1000 uniform random numbers
a = 0; b = 1;
X = runif(1000, a, b)
hist(X)
grid()

## Mean, variance, median, mode of a uniform random variable

# R code to computer empirical mean, var, median, mode
library(pracma)
a = 0; b = 1;
X = runif(1000, a, b)
M = mean(X)
V = var(X)
Med = median(X)
Mod = Mode(X)

## Mean and Variance computation (not on "Code and Data" section of website, but written in textbook p.34)

# R code to compute mean and variance
unifstat = function(a, b) {
    M = (a+b)/2
    V = ((a-b)^2)/12
    return(list(mean = M, var = V))
}

a = 0; b = 1;
M = unifstat(a, b)$mean
V = unifstat(a, b)$var

## Probability of a uniform random variable

# R code to compute the probability P(0.2 < X < 0.3)
a = 0; b = 1;
F = punif(0.3, a, b) - punif(0.2, a, b)


## PDF of an exponential random variable

# R code to generate the PDF and CDF of an exponential random variable
lambda1 = 1/2
lambda2 = 1/5
x = seq(0, 1, (0+1)/1000)
# PDFs
f1 = dexp(x, 1/lambda1)
f2 = dexp(x, 1/lambda2)
plot(x, f2, type="n")
lines(x, f1, lwd=4, col="blue", lty=6)
lines(x, f2, lwd=4, col="red")
legend(0.8, 4.5, legend=c(expression(paste(lambda, "=5")), expression(paste(lambda, "=2"))), col=c("red", "blue"), lty=1:1)
grid()

# CDFs
F1 = pexp(x, 1/lambda1)
F2 = pexp(x, 1/lambda2)
plot(x, F2, type="n")
lines(x, F1, lwd=4, col="blue", lty=6)
lines(x, F2, lwd=4, col="red")
legend(0.8, 0.2, legend=c(expression(paste(lambda, "=5")), expression(paste(lambda, "=2"))), col=c("red", "blue"), lty=1:1)
grid()

#############
# Chapter 4.6

## Generate Gaussian PDF (not on "Code and Data" section on website, but written in textbook p.42)

# R code to generate a Gaussian PDF
x = seq(-10, 10, (10+10)/1000)
mu = 0; sigma = 1;
f = dnorm(x, mu, sigma)
plot(x, f, type="n")
lines(x, f, lwd=4, col="blue")
grid()

## PDF and CDF of a Gaussian random variable

# R code to generate standard Gaussian PDF and CDF
x = seq(-5, 5, (5+5)/1000)
f = dnorm(x)
F = pnorm(x)
# PDF
plot(x, f, type = "n")
lines(x, f, lwd=6)
grid()

# CDF
plot(x, F, type = "n")
lines(x, F, lwd=6)
grid()

## Verify standardised gaussian (not on "Code and Data" section on website, but written in textbook p.45)

# R code to verify standardised Gaussian
x = seq(-5, 5, (5+5)/1000)
mu = 3; sigma = 2;
f1 = dnorm((x-mu)/sigma, 0, 1) # Standardised
f2 = dnorm(x, mu, sigma) # Raw

# Skewness and kurtosis of a random variable

# R code to plot a Gamma distribution
x = seq(0, 30, (0+30)/1000)
theta = 1
plot(x, dgamma(x, 2, theta), type = "n")
lines(x, dgamma(x, 2, theta), lwd = 4)
lines(x, dgamma(x, 5, theta), lwd = 4, col = "#333333")
lines(x, dgamma(x, 10, theta), lwd = 4, col = "#666666")
lines(x, dgamma(x, 15, theta), lwd = 4, col = "#999999")
lines(x, dgamma(x, 20, theta), lwd = 4, col = "#CCCCCC")
legend(23, 0.36, legend=c("k = 2", "k = 5", "k = 10", "k = 15", "k = 20"), col=c("1", "#333333", "#666666", "#999999", "#CCCCCC"), lwd = 4,lty=1:1)
grid()

# R code to compute skewness and kurtosis
library(e1071)
X = rgamma(10000, 3, 5)
s = skewness(X)
k = kurtosis(X)

# Histogram of Z = X1 + X2 + X3 (not on "Code and "Data" section, but written in textbook p.50)

# R code to show the histogram of Z = X1 + X2 + X3
N = 10000
X1 = runif(N, 1, 6)
X2 = runif(N, 1, 6)
X3 = runif(N, 1, 6)
Z = X1 + X2 + X3
hist(Z, breaks=seq(2.5,18.5))
grid()

#############
# Chapter 4.8

## Generating Gaussians from uniform

# R code to generate Gaussian from uniform
library(HDInterval)
mu = 3
sigma = 2
U = runif(10000, 0, 1)
gU = sigma * inverseCDF(U, pnorm) + mu;
hist(U)
grid()

hist(gU, col="yellow")
grid()

## Generating exponential random variables from uniform random nums (not in "code" section, but in textbook p.63)

# R code to generate exponential random variables
lambda = 1
U = runif(10000, 0, 1)
gU = -(1/lambda)*log(1-U)

## Plotting PDFs based on transformed vars (not in "code" section, but in textbook p.65)

# R code to generate the desired random variables
U = runif(10000, 0, 1)
gU = rep(0, 10000)
gU[U >= 0.0 & U <= 0.1] = 1
gU[U > 0.1 & U <= 0.6] = 2
gU[U > 0.6 & U <= 0.9] = 3
gU[U > 0.9 & U <= 1] = 4
hist(gU)
grid()

#############
