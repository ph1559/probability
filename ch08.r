#############
# Chapter 8.1 Maximum-likelihood Estimation

## Visualizing the likelihood function

# R: Visualize the likelood function
library(pracma)
library(plot3D)

N = 50
S = 1:N
theta = seq(0.1, 0.9, (0.9+0.1)/100)
mesh = meshgrid(S, theta)
S_grid = mesh$X
theta_grid = mesh$Y
L = S_grid * log(theta_grid) + (N-S_grid) * log(1-theta_grid)
L = t(L)
persp3D(S, theta, L, theta=65, phi=15, border="black", lwd=0.3, bty="b2", xlab="S", ylab="θ", zlab="", ticktype="detailed")

# Slice through the log likelihood
N = 50
S = seq(from=1, to=N, by=0.1)
theta = seq(0.1, 0.9, (0.1+0.9)/1000)
mesh = meshgrid(S, theta)
S_grid = mesh$X
theta_grid = mesh$Y
L = S_grid * log(theta_grid) + (N-S_grid) * log(1-theta_grid)
L = t(L)
image(S, theta, L, col=rainbow(256), ylim=c(0.9, 0.1))

## Visualizing the likelihood function

# R code
library(pracma)

N = 50
S = 1:N
theta = seq(0.1, 0.9, (0.1+0.9)/100)
mesh = meshgrid(S, theta)
S_grid = mesh$X
theta_grid = mesh$Y
L = S_grid * log(theta_grid) + (N-S_grid) * log(1-theta_grid)
L_df = data.frame(L)
colnames(L_df) = S

plot(theta, L_df$"12", type="n")
grid()
lines(theta, L_df$"12", lwd=6)
title(expression(paste("L(", theta, " | S = 12)")))

## ML estimation for single-photon imaging

# R code
library(imager)
lambda = as.data.frame(load.image("cameraman.tif"))
lambda = xtabs(value ~ x+y, data=lambda)
T = 100
x = c()
for (i in 1:T) { 
    x = append(x, rpois(length(lambda), lambda))
}
x = array(x, c(256, 256, T))
y = (x>=1)
mu = apply(y, c(1,2), mean)
lambdahat = -log(1-mu)
fig1 = x[,,1]

# Flip matrix since `image()` reads the matrix bottom up
flip_matrix = function(m) m[,nrow(m):1]

# Figure 1: A single sample image
image(flip_matrix(fig1), col=gray.colors(255))

# Figure 2: ML Recovered Image
image(flip_matrix(lambdahat), col=gray.colors(255))

#############
# Chapter 8.2 Properties of the ML estimation

## Visualizing the invariance principle

# R code
N = 50
S = 20
theta = seq(0.1, 0.9, (0.1+0.9)/1000)
L = S * log(theta) + (N-S) * log(1-theta)
plot(theta, L, type="n", xlab=expression(theta), ylab=expression(paste("Log L(", theta, "|S = 20)")))
title("Bernoulli")
grid()
lines(theta, L, lwd=6, col="#8080BF")

h_theta = -log(1-theta)
plot(theta, h_theta, type="n", xlab=expression(theta), ylab=expression(paste(eta, " = h(", theta, ")")))
grid()
lines(theta, h_theta, lwd=6)

theta = seq(0.1, 2.5, (0.1+2.5)/1000)
L = S * log(1-exp(-theta)) - theta * (N-S)
plot(theta, L, type="n")
title("Truncated Poisson")
grid()
lines(theta, L, lwd=6, col="#0000BF")

#############
# Chapter 8.3 Maximum-a-Posteriori Estimation

## Influence of the priors

# R code
N = 1
sigma0 = 1
mu0 = 0.0
x = 5
# x = rnorm(N, 5, 1)
t = seq(-3, 7, length.out=1000)

q = numeric(1000)
for (i in 1:N) {
    b = abs(t-x[i])
    a = min(b)
    q[match(a, b)] = 0.1 
}

p0 = dnorm(t, mean(x), 1)
theta = (mean(x)*sigma0^2+mu0/N)/(sigma0^2+1/N)
p1 = dnorm(t, theta, 1)
prior = dnorm(t, mu0, sigma0)/10

plot(t, p1, type="n")
title("N = 5")
legend("topleft", legend=c("Likelihood", "Posterior", "Prior", "Data"), col=c("gray", "black", "orange", "dimgray"), lwd=3)
grid()
lines(t, p0, lwd = 5, col="gray")
lines(t, p1, lwd = 5, col="black")
lines(t, prior, lwd = 5, col="orange")
lines(t, q, type="h", lwd = 5, col="dimgray")

## Conjugate priors

# R code
sigma0 = 0.25
mu0 = 0.0

mu = 1
sigma = 0.25

Nset = c(0, 1, 2, 5, 8, 12, 20)
x0 = sigma * rnorm(100)
posterior = list()

for (i in 1:7) {
    N = Nset[i]
    x = x0[1:N]
    t = seq(-1, 1.5, 2.5/1000)

    p0 = dnorm(t, 0, 1)
    theta = mu*(N*sigma0^2)/(N*sigma0^2+sigma^2) + mu0*(sigma^2)/(N*sigma0^2+sigma^2)
    sigmaN = sqrt(1/(1/sigma0^2+N/sigma^2));
    posterior[[i]] = dnorm(t, theta, sigmaN)
}

plot(t, posterior[[7]], type="n", xlab="", ylab="")
grid()
lines(t, posterior[[1]], lwd=3, col="red")
lines(t, posterior[[2]], lwd=3, col="orange")
lines(t, posterior[[3]], lwd=3, col="yellow")
lines(t, posterior[[4]], lwd=3, col="green")
lines(t, posterior[[5]], lwd=3, col="turquoise")
lines(t, posterior[[6]], lwd=3, col="blue")
lines(t, posterior[[7]], lwd=3, col="purple")

legend(-0.9, 7, legend=c("N = 0", "N = 1", "N = 2", "N = 5", "N = 8", "N = 12", "N = 20"), col=c("red", "orange", "yellow", "green", "turquoise", "blue", "purple"), lty=1:1, lwd=3)

#############
