#############
# Chapter 10.2 Mean and correlation functions

## Example 10.5

# R code for Example 10.5
x = matrix(0, 1000, 20)
t = seq(-2, 2, (2+2)/999)

for (i in 1:20) {
    x[,i] = runif(1) * cos(2 * pi * t)
}

matplot(t, x, lwd=2, col="gray")
grid()
lines(t, 0.5*cos(2*pi*t), lwd=4, col="darkred")

## Example 10.6

# R code for Example 10.6
x = matrix(0, 1000, 20)
t = seq(-2, 2, (2+2)/999)

for (i in 1:20) {
    x[,i] = cos(2*pi*t+2*pi*runif(1))
}

matplot(t, x, lwd=2, col="gray")
grid()
lines(t, 0*cos(2*pi*t), lwd=4, col="darkred")

## Example 10.8

# R code for Example 10.7
x = matrix(0, 21, 20)
t = 0:20

for (i in 1:20) {
    x[,i] = runif(1) ^ t
}

matplot(t, x, pch=1, col="gray", lwd=2)
grid()

for (i in 1:ncol(x)) {
    lines(t, x[,i], type='h', col="gray", lwd=2)
}
lines(t, 1/(t+1), col="darkred", lwd=2, pch=1, type="p")
lines(t, 1/(t+1), col="darkred", lwd=2, type="h")

## Example 10.11

# R code for Example 10.11: Plot the time function
t = seq(-2, 2, len = 1000)
x = matrix(0, 1000, 20)

for (i in 1:20) {
    x[,i] = runif(1) * cos(2*pi*t)
}

matplot(t, x, lwd=2, col="gray", type="l", lty="solid", xaxp=c(-2,2, 8))
grid()
lines(t, 0.5*cos(2*pi*t), lwd=5, col="darkred")
points(numeric(20), x[501,], lwd=2, col="darkorange")
points(numeric(20) + 0.5, x[626,], lwd=2, col="blue", pch=4)

# R code for Example 10.11: Plot the autocorrelation function
t = seq(-1, 1, len=1000)
R = (1/3)*outer(cos(2*pi*t), cos(2*pi*t))
image(t, t, R, col=topo.colors(255), xlab="t_1", ylab="t_2")

## Example 10.12

# R code for Example 10.11: Plot the time function
t = seq(-2, 2, len = 1000)
x = matrix(0, 1000, 20)

for (i in 1:20) {
    x[,i] = cos((2*pi*t) + (2*pi*runif(1)))
}

matplot(t, x, lwd=2, col="gray", type="l", lty="solid", xaxp=c(-2,2, 8))
grid()
lines(t, 0*cos(2*pi*t), lwd=5, col="darkred")
points(numeric(20), x[501,], lwd=2, col="darkorange")
points(numeric(20) + 0.5, x[626,], lwd=2, col="blue", pch=4)

# R code for Example 10.12: Plot the autocorrelation function
t = seq(-1, 1, len=1000)
R = toeplitz(0.5*(cos(2*pi*t)))
image(t, t, R, col=topo.colors(255), ylim=c(1,-1), xlab="t_1", ylab="t_2")

#############
# Chapter 10.3 Wide sense stationary processes

## Example 10.14

# R code to demonstrate autocorrelation

# Figure 1
Xa = rnorm(1000)
Xa2 = rnorm(1000)

plot(Xa, type="l", lwd=2, col="blue")
grid()
lines(Xa2, lwd=2)

# Figure 2
N = 1000
T = 1000
X = matrix(rnorm(N*T), N, T)
xc = matrix(0, N, 2*T-1)

for (i in 1:N) {
    xc[i,] = ccf(X[i,], X[i,], lag.max=2*T-1, pl=FALSE)$acf/T
}

plot(xc[1,], type="l", lwd=2, col="darkblue")
lines(xc[2,], lwd=2)

#############
# Chapter 10.5 Wide sense stationary processes

## Example 10.15

# R code for Example 10.15
t = seq(-10, 10, by=0.001)
L = length(t)
X = rnorm(L)
h = 10 * sapply((1 - abs(t)), max, 0) / 1000
Y = convolve(X, h, type=c("circular"))

# Figure 1
plot(t, X, lwd=1, col="gray", type="l")
grid()
legend(6, 3.8, legend=c("X(t)", "μ_x(t)", "Y(t)", "μ_y(t)"), col=c("gray", "black", "darkorange", "yellow"), lty=c(1, 1, 1, 3), lwd=c(1, 4, 3, 4), bg="white")
abline(h=0, lwd=4, lty=1, col="yellow")
lines(t, Y, lwd=3, col="darkorange")
abline(h=0, lwd=4, lty=3)

# Figure 2
h2 = convolve(h, h, type="open")
Rx = numeric(40001)
Rx[20001] = 0.2
plot(seq(-20, 20, by=0.001), Rx, lwd=2, col="gray", type="l", xlim=c(-2,2), ylim=c(-0.05, 0.2))
grid()
legend(-2, 0.19, legend=c("R_x(t)", "R_y(t)"), col=c("gray", "darkorange"), lty=1:1, lwd=2)
lines(seq(-20, 20, by=0.001), h2, lwd=2, col="darkorange")

#############
# Chapter 10.6 Optimal linear filter

## Solve the Yule Walker equation

# R code to solve the Yule Walker Equation

y = scan("./ch10_LPC_data.txt")
K = 10
N = 320
y_corr = ccf(y, y, lag.max=2*length(y)-1, pl=FALSE)$acf
R = toeplitz(y_corr[N + 1:K-1])
lhs = y_corr[N + 1:K]
h = solve(R, lhs)

# Figure 1
plot(y, lwd = 4, col="blue", type="l")
grid()
legend(10, 0.1, legend=c("Y[n]"), col=c("blue"), lty=1:1, lwd=4)

# Figure 2
plot(y_corr, lwd = 4, type="l")
grid()
legend(10, 0.9, legend=c("R_y[k]"), col=c("black"), lty=1:1, lwd=4)

## Predict sample

# R code to predict the samples
y = scan("./ch10_LPC_data_02.txt")
K = 10
N = length(y)

y_corr = ccf(y, y, lag.max=2*N-1)$acf[,,1]
R = toeplitz(y_corr[N + 1:K-1])
lhs = y_corr[N + 1:K]
h = solve(R, lhs)

z = y[311:320]
yhat = numeric(340)
yhat[1:320] = y

for (t in 1:20) {
    predict = t(z) * h
    z = c(z[2:10], predict)
    yhat[320+t] = predict
}

plot(yhat, lwd=3, col="red", type="l")
grid()
legend(10, 0.9, legend=c("Prediction", "Input"), col=c("red", "gray"), lty=c(1, 2), lwd=2)
lines(y, lwd=4, col="gray", lty=3)

## Wiener filter 

# R code for Wiener filtering
library(pracma)
y = scan("./ch10_LPC_data_02.txt")
w = 0.05 * rnorm(320)
x = y + w

Ry = ccf(y, y, lag.max=2*length(y)-1, pl=FALSE)$acf
Rw = ccf(w, w, lag.max=2*length(w)-1, pl=FALSE)$acf
Sy = fft(Ry)
Sw = fft(Rw)
H = Sy / (Sy + Sw)

a = x[1:639]
a[is.na(a)] = 0
Yhat = H * fft(a[1:639])
yhat = Re(ifft(as.vector(Yhat)))

# Figure 1
plot(x, lwd=5, col="gray", type="l")
grid()
legend(10, -0.7, legend=c("Noisy Input X[n]", "Wiener Filtered Yhat[n]", "Ground Truth Y[n]"), col=c("gray", "red", "black"), lty=c(1, 1, 2), lwd=2)
lines(yhat[1:320], lwd=2, col="red")
lines(y, lwd=2, lty=3)

# Figure 2
plot(Rw, lwd=4, col="blue", label="h[n]", type="l")
legend(500, 0.85, legend=c("h[n]"), col=c("blue"), lty=c(1), lwd=c(4))
grid()

## Wiener deblurring

# R code to solve the Wiener deconvolution problem
library(pracma)

conv_same = function(x, y) {
    s = length(y) / 2
    e = length(x) + s - 1
    return (convolve(x, y, type="open")[s:e])
}

y = scan("./ch10_wiener_deblur_data.txt")
g = ones(32, 1)/32
w = 0.02 * rnorm(320)
s = length(y)/2
e = length(g) + s - 1
x = conv_same(y, g) + w

Ry = ccf(y, y, lag.max=2*length(y)-1, pl=FALSE)$acf
Rw = ccf(w, w, lag.max=2*length(w)-1, pl=FALSE)$acf
Sy= fft(Ry)
Sw = fft(Rw)
a = g[1:639]
a[is.na(a)] = 0
G = fft(a[1:639])

H = (Conj(G) * Sy) / (abs(G) ^ 2 * Sy + Sw)
b = x[1:639]
b[is.na(b)] = 0
Yhat = H * fft(b[1:639])
yhat = Re(ifft(as.vector(Yhat)))

plot(x, lwd=4, col="gray", type="l", ylim=c(-0.7, 0.7))
grid()
legend(150, -0.45, legend=c("Noisy Input X[n]", "Wiener Filtered Yhat[n]", "Ground Truth Y[n]"), col=c("gray", "red", "black"), lty=c(1, 1, 2), lwd=2)
lines(16:(320+15), yhat[1:320], col="red", lwd=2)
lines(1:320, y, lwd=2, lty=3)

#############

