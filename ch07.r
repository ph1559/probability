#############
# Chapter 7.1 Principles of Regression

## Linear regression: Straight Line

# R code to fit data points using a straight line
library(pracma)

N = 50
x = runif(N)

a = 2.5 # true parameter
b = 1.3 # true parameter
y = a*x + b + 0.2*rnorm(N) # Synthesize training data

X = cbind(x, rep(1, N))
theta = lsfit(X, y)$coefficients
t = linspace(0, 1, 200)
yhat = theta[2]*t + theta[1]
plot(x, y, pch=1, col="blue")
grid()
lines(t, yhat, col='red', lwd=4)
legend("bottomright", c("Best Fit", "Data"), fill=c("red", "blue"))

## Linear regression: Polynomial

# R code to fit data using a quadratic equation
N = 50
x = runif(N)
a = -2.5
b = 1.3
c = 1.2
y = a*x**2 + b*x + c + 0.2*rnorm(N)
X = cbind(rep(1, N), x, x**2)
theta = lsfit(X, y)$coefficients
t = linspace(0, 1, 200)
yhat = theta[1] + theta[2]*t + theta[3]*t**2
plot(x,y,pch=1)
grid()
lines(t,yhat,col='red',lwd=4)
legend("bottomleft", c("Fitted curve", "Data"), fill=c("red", "black"))

## Legendre Polynomial

# R code to fit data using Legendre polynomials
library(pracma)

N = 50
x = linspace(-1,1,N)
a = c(-0.001, 0.01, 0.55, 1.5, 1.2)
y = a[1]*legendre(0, x) + a[2]*legendre(1, x)[1,] + 
  a[3]*legendre(2, x)[1,] + a[4]*legendre(3, x)[1,] + 
  a[5]*legendre(4, x)[1,] + 0.2*rnorm(N)

X = cbind(legendre(0, x), legendre(1, x)[1,], 
           legendre(2, x)[1,], legendre(3, x)[1,],
           legendre(4, x)[1,]) # good

beta = mldivide(X, y)

t = linspace(-1, 1, 50)
yhat = beta[1]*legendre(0, x) + beta[2]*legendre(1, x)[1,] + 
  beta[3]*legendre(2, x)[1,] + beta[4]*legendre(3, x)[1,] + 
  beta[5]*legendre(4, x)[1,]

plot(x, y, lwd=3, pch=1, col="blue")
grid()
lines(t, yhat, lwd=5, lty=2, col="red")

## Auto-regressive model

# R code for auto-regressive model
library(pracma)

N = 500
y = cumsum(0.2*rnorm(N)) + 0.05*rnorm(N)

L = 100
c = c(0, y[0:(400-1)])
r = rep(0, L)
X = Toeplitz(c,r)
beta = mldivide(X, y[1:400])
yhat  = X %*% beta
plot(y[1:400], lwd=2)
grid()
lines(yhat[1:400], col="red", lwd=4)

## Robust regression by linear programming

# R code to demonstrate robust regression TODO
library(pracma)
N = 50
x = linspace(-1,1,N)
a = c(-0.001, 0.01, 0.55, 1.5, 1.2)
y = a[1]*legendre(0, x) + a[2]*legendre(1, x)[1,] + 
  a[3]*legendre(2, x)[1,] + a[4]*legendre(3, x)[1,] + 
  a[5]*legendre(4, x)[1,] + 0.2*rnorm(N)

idx = c(10, 16, 23, 37, 45)
y[idx] = 5

X = cbind(rep(1,N), x, x**2, x**3, x**4)
A = rbind(cbind(X, -1*diag(N)), cbind(-X, -1*diag(N)))
b = c(y, -y)
c = c(rep(0, 5), rep(1, N))
res = linprog(c, A, b, maxiter=1000000, lb=NULL, ub=NULL)
print(c)
print(A)
print(b)
beta = res.x
t = linspace(-1, 1, 200)

#############
# Chapter 7.2 Overfitting

## Overfitting example

# R: An overfitting example (TODO)

N = 20
x = sort(rnorm(N)*2-1)
a = c(-0.001, 0.01, 0.55, 1.5, 1.2)
y = a[1]*legendre(0, x) + a[2]*legendre(1, x)[1,] + 
  a[3]*legendre(2, x)[1,] + a[4]*legendre(3, x)[1,] + 
  a[5]*legendre(4, x)[1,] + 0.1*rnorm(N)

P = 20 
X = matrix(0, N, P+1)
for (p in 0:P) {
  tmp = matrix(legendre(p, x))[1,]
  X[,p+1] = tmp
}
beta = mldivide(X, y)

t = linspace(-1, 1, 50)
Xhat = matrix(0, length(t), P+1)
for (p in 0:P) {
  tmp = matrix(legendre(p, t))[1,]
  Xhat[,p+1] = tmp
}
yhat = mldivide(Xhat, beta)
plot(x, y)
lines(t, yhat)

## Learning curve

# R

#############
# Chapter 7.3 Bias and Variance

## Mean estimator

# R code to visualize the average predictor
library(pracma)
N = 20
x = linspace(-1,1,N)
a = c(0.5, -2, -3, 4, 6)
yhat = matrix(0,50,100)
plot(NULL, xlim=c(-1,1), ylim=c(-3,3))
for (i in 1:100) {
  y = a[1] + a[2]*x + a[3]*x**2 + a[4]*x**3 + a[5]*x**4 + 0.5*rnorm(N)
  X = cbind(rep(1,N), x, x**2, x**3, x**4)
  
  theta = lsfit(X, y)$coefficients[2:6]
  t = linspace(-1, 1, 50)
  Xhat = cbind(rep(1, N), t, t**2, t**3, t**4)
  yhat[,i] = Xhat %*% theta
  lines(t, yhat[,i], col="gray")
}
lines(t, rowMeans(yhat), col="red", lwd=5)

#############
# Chapter 7.4 Regularization

## Ridge regression

# R code to demonstrate a ridge regression example
N = 20
x = linspace(-1,1,N)
a = c(0.5, -2, -3, 4, 6)
y = a[1] + a[2]*x + a[3]*x**2 + a[4]*x**3 + a[5]*x**4 + 0.2*rnorm(N)

d = 20
X = matrix(0, N, d)
for (p in 1:d) {
  X[,p] = x**p
}
lambd = 0.1
A = rbind(X, (lambd)*diag(d)**(1/2))
b = c(y, rep(0, d))
theta = lsfit(A, b)$coefficients[2:21]

t = linspace(-1, 1, 500)
Xhat = matrix(0, 500,d)
for (p in 1:d) {
  Xhat[,p] = t**p
}
yhat = Xhat %*% theta
plot(x, y)
grid()
lines(t, yhat, col="darkgray", lwd=4)
legend("bottomleft", c("Data", "Fitted curve"), pch=c(1, NA), lty=c(NA, 1))

## LASSO regression (TODO)

# R
library(pracma)
library(CVXR)

data = scan("ch7_data_crime.txt")
data = matrix(data, nrow=7)
data = t(data)
y = data[,1]
X = data[,3:8]
N = dim(X)[0]
d = dim(X)[1]

lambd_set = logspace(-1,8,50)
theta_store = matrix(0, d,50)
for (i in 1:50) {
  lambd = lambd_set[i]
  theta = Variable(d)
  lambd = lambd_set[i]
  objective = Minimize(sum_squares(X %*% theta-y) 
                         + lambd*norm1(theta))
  # objective   = cvx.Minimize( cvx.sum_squares(X*theta-y) \
  prob = Problem(objective)
  result = solve(prob)
  theta_store[,i] = result$getValue(theta)
}
plot(NULL, xlim=c(10**(-2), 10**8), ylim=c(-2,14))
for (i in 1:d) {
  lines(log(lambd_set), theta_store[i,])
}

## LASSO vs Ridge (TODO)

# R code to demonstrate overfitting and LASSO

# Setup the problem

install.packages("CVXR")
library(pracma)
library(CVXR)

N = 20
x = linspace(-1,1,N)
a = c(1, 0.5, 0.5, 1.5, 1)
y = a[1]*legendre(0,x) + a[2]*legendre(1,x)[1,] +
  a[3]*legendre(2,x)[1,] + a[4]*legendre(3,x)[1,] +
  a[5]*legendre(4,x)[1,] + 0.25*rnorm(N)

# Solve LASSO using CVX
d = 20
lambd = 1
X = matrix(0, N, d)
for (p in 1:d) {
  X[,p] = legendre(p,x)[1,]
}
theta = Variable(d)
objective = Minimize(sum_squares(X %*% theta-y)
                      + lambd*norm1(theta))
prob = Problem(objective)
result = solve(prob)
thetahat = result$getValue(theta)

# Plot the curves
t = linspace(-1, 1, 500)
Xhat = matrix(0, 500, d)
for (p in 1:P) {
  Xhat[,p] = legendre(p,t)[1,]
}
yhat = Xhat %*% thetahat
plot(x, y)
lines(t, yhat)
