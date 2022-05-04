#############
# Chapter 6.2 (Probability Inequality)

## Compare Chebyshev's and Chernoff's bounds

# R code to compare the probability bounds

library(pracma)

epsilon <- 0.1
sigma <- 1;
N <- logspace(1,3.9,50)
p_exact <- 1-pnorm(sqrt(N)*epsilon/sigma)
p_cheby <- sigma^2 / (epsilon^2*N)
p_chern <- exp(-epsilon^2*N/(2*sigma*2))

plot(log(N), log(p_exact), pch=1, col="orange", lwd=4, xlab="log(N)", ylab="log(Probability)")
lines(log(N), log(p_cheby), lty=6, col="green", lwd=4)
lines(log(N), log(p_chern), pch=19, col="blue", lwd=4)
legend(3, -25, c("Exact","Chebyshev","Chernoff"), fill=c("orange", "green", "blue"))

#############
# Chapter 6.3 (Law of Large Numbers)

## Weak law of large numbers

# R code to illustrate the weak law of large numbers

library(pracma)

p <- 0.5
Nset <- as.integer(round(logspace(2,5,100)))
x <- matrix(rep(0, 1000*length(Nset)), nrow=1000)
for (i in 1:length(Nset)) {
  N = Nset[i]
  x[,i] <- rbinom(1000, N, p) / N
}
Nset_grid <- repmat(Nset, m=1, n=1000)

semilogx(Nset_grid, x, col='black', pch=4)
lines(Nset, p + 3*(((p*(1-p))/Nset)^(1/2)), col='red', lwd=6)
lines(Nset, p - 3*(((p*(1-p))/Nset)^(1/2)), col='red', lwd=6)

#############
# Chapter 6.4 (Central Limit Theorem)

## PDF of the sum of two Gaussians

# Plot the PDF of the sum of two Gaussians

library(pracma)

n <- 10000
K <- 2
Z <- rep(0, n)

for (i in 1:K) {
  X <- runif(n, min=1, max=6)
  Z <- Z + X
}
hist(Z,breaks=(K-0.5):(6*K+0.5),freq=FALSE)

# Visualize convergence in distribution
N <- 10
N <- 50
x <- seq(0, N, (N/1000))
p <- 0.5
p_b <- dbinom(x, N, p)
p_n <- dnorm(x, N*p, (N*p*(1-p))**(1/2))

c_b <- pbinom(x, N, p)
c_n <- pnorm(x, N*p, (N*p*(1-p))**(1/2))

plot(x, p_n, lwd=6, type="l", col='red')
lines(x, p_b, lwd=2, col='black')
legend("topright", c('Binomial', 'Gaussian'), col=c('black', 'red'), lty=c(1, 1), lwd=3)

# Poisson to Gaussian: convergence in distribution
N <- 4
# N = 10
# N = 50

x <- seq(0, 2*N,(2*N/1000))
lambda <- 1
p_b <- dpois(x, N*lambda)
p_n <- dnorm(x, N*lambda, sqrt(N*lambda))

c_b <- ppois(x, N*lambda);
c_n = pnorm(x, N*lambda, sqrt(N*lambda));

plot(x, p_b, col="black", type="s")
lines(x, p_n, col="red", lwd=6)
legend("bottomright", c('Poisson', 'Gaussian'), fill=c('black', 'red'))

plot(x, c_b, col="black", type="s")
lines(x, c_n, col="red", lwd=6)
legend("bottomright", c('Poisson', 'Gaussian'), fill=c('black', 'red'))

# Visualize the Central Limit Theorem
N <- 10
x <- seq(0, N, length.out=1001)
p <- 0.5
p_b <- dbinom(x, N, p);
p_n <- dnorm(x, N*p, sqrt(N*p*(1-p)));

c_b <- pbinom(x, N, p);
c_n <- pnorm(x, N*p, sqrt(N*p*(1-p)));

x2 <- linspace(5-2.5,5+2.5,1001);
q2 <- dnorm(x2,N*p, sqrt(N*p*(1-p)));

plot(x, p_n, col="red")
polygon(c(min(x2), x2, max(x2)), c(0, q2, 0), col='lightblue')
lines(x, p_b, col="black", type="h")


# How moment generating of Gaussian approximates in CLT

library(pracma)

p <- 0.5
s <- seq(-10,10,length.out=1001)
MX <- 1-p+p*exp(s)
N <- 2
semilogy(s, (1-p+p*exp(s/N))**N, lwd=4, col="lightblue", xlim=c(-10,10), ylim=c(10**-2, 10**5))
mu <- p
sigma <- sqrt(p*(1-p)/N);
MZ <- exp(mu*s + sigma^2*s**2/2);
lines(s, MZ, lwd=5, lty=3);
legend("topleft", c('Binomial MGF', 'Gaussian MGF'), fill=c('lightblue', 'black'))

# Failure of Central Limit Theorem at tails

library(pracma)

x <- seq(-1,5,length.out=1001)
lambda <- 1

N <- 1
f1 <- (N**(1/2)/lambda)*dgamma((x+sqrt(N))/(lambda/sqrt(N)), N, lambda)
semilogy(x, f1, lwd=0.5, col='lightgray', xlim=c(-1,5), ylim=c(10**-6, 1))

N <- 10
f1 <- (N**(1/2)/lambda)*dgamma((x+sqrt(N))/(lambda/sqrt(N)), N, lambda)
lines(x, f1, lwd=4, col='gray')

N <- 100
f1 <- (N**(1/2)/lambda)*dgamma((x+sqrt(N))/(lambda/sqrt(N)), N, lambda)
lines(x, f1, lwd=4, col='darkgray')

N <- 1000
f1 <- (N**(1/2)/lambda)*dgamma((x+sqrt(N))/(lambda/sqrt(N)), N, lambda)
lines(x, f1, lwd=4, col='black')

g <- dnorm(x,0,1)
lines(x, g, lwd=4, lty=6, col='red')

legend("bottomleft", c('N=1', 'N=10', 'N=100', 'N=1000', 'Gaussian'), fill=c('lightgray', 'gray', 'darkgray', 'black', 'red'))

