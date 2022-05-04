### CH 9.1 confidence interval

### COmpute confidence interval
alpha = 0.05
mu = 0
sigma = 1
epsilon = qnorm(0.975, mu, sigma)
epsilon

## Constructing confidence interval from data
x <- c(72,69,75,58,67,70,60,71,59,65)
N = length(x)
Theta_hat = mean(x)
S_hat = sd(x)
nu = length(x) - 1
alpha = 0.05
z = qt(1-alpha/2, nu)
CI_L = Theta_hat-z*S_hat/sqrt(N)
CI_U = Theta_hat+z*S_hat/sqrt(N)
CI_L
CI_U

## Bootstrap median
X <- c(72, 69, 75, 58, 67, 70, 60, 71, 59, 65)
N = length(X)
K = 1000
Thetahat = rep(0, K)
for (i in 0:K) {
    idx = sample(N,5)
    Y = X[idx]
    Thetahat[i] = median(Y)
}
M = mean(Thetahat)
V = var(Thetahat)
M
V

## HYpothesis testing (z_value)
Theta_hat = 0.29                     # Your estimate
theta     = 0.25                     # Your hypothesis
N         = 1000                     # Number of samples
sigma     = sqrt(theta*(1-theta)) # Known standard deviation
Z_hat = (Theta_hat - theta)/(sigma / sqrt(N))
Z_hat

## HYpothesis testing critical value
alpha = 0.05
z_alpha = qnorm(1-alpha, 0, 1)
z_alpha

## Hypothesis testing p_value
p = pnorm(-1.92,0,1)
p


## Plot an ROC curve
#install.packages("matlab", repo="http://cran.r-project.org", dep=T)
library(matlab)

sigma = 2 
mu = 3
alphaset <- linspace(0,1,1000)
PF1 = rep(0, 1000) 
PD1 = rep(0, 1000)
PF2 = rep(0, 1000)
PD2 = rep(0, 1000)
for (i in 0:1000) {
    alpha = alphaset[i]
    PF1[i] = alpha
    PD1[i] = alpha
    PF2[i] = alpha
    PD2[i] = 1-pnorm(qnorm(1-alpha)-mu/sigma)
}


#roc_plot <- rbind(, plot(PF2,PD2))
#roc_plot
plot(PF1,PD1,type = "l", col = "red")
lines(PF2, PD2, col = "black")

## Compute area under curve 
auc1 = sum(PD1 * c(0, diff(PF1)))
auc2 = sum(PD2 * c(0, diff(PF2)))
auc1
auc2

## Another ROC curve
sigma = 2
mu = 3
PF1 = rep(0, 1000)
PD1 = rep(0, 1000)
PF2 = rep(0, 1000)
PD2 = rep(0, 1000)

alphaset <- linspace(0,0.5,1000)
for (i in 0:1000) {
    alpha = alphaset[i]
    PF1[i] = 2*alpha
    PD1[i] = 1-(pnorm(qnorm(1-alpha)-mu/sigma) - pnorm(-qnorm(1-alpha)-mu/sigma))
}
alphaset = linspace(0,1,1000)
for (i in 0:1000) {
    alpha = alphaset[i]
    PF2[i] = alpha
    PD2[i] = 1-pnorm(qnorm(1-alpha)-mu/sigma)
}
plot(PF1, PD1, type = "l", col = "red")
lines(PF2, PD2, col = "black")



## Roc with real data
scores <- c(0.8271, 0.6045, 0.7916, 0.1608, 0.6112, 0.2555, 0.5682, 0.0599, 0.6644, 0.1129, 0.0615, 0.3525, 0.3227, 0.4334, 0.2281, 0.722, 0.2353, 0.285, 0.4107, 0.2008, 0.3712, 0.4235, 0.4876, 0.4235, 0.5751, 0.6734, 0.7356, 0.7138, 0.3874, 0.2404, 0.1663, 0.1663, 0.285, 0.3684, 0.1738, 0.4364, 0.722, 0.4675, 0.2353, 0.172, 0.1779, 0.4434, 0.2769, 0.0689, 0.2141, 0.2712, 0.2633, 0.4806, 0.0885, 0.2555, 0.5682, 0.285, 0.8422, 0.5281, 0.6303, 0.9325, 0.0622, 0.8823, 0.6707, 0.8917, 0.6489, 0.5552, 0.751, 0.2331, 0.2933, 0.6045, 0.6303, 0.9585, 0.9343, 0.3227, 0.7982, 0.221, 0.9391, 0.5079, 0.7379, 0.875, 0.4705, 0.4434, 0.5652, 0.8659, 0.897, 0.9713, 0.5652, 0.518, 0.4039, 0.9435, 0.5781, 0.5947, 0.397, 0.7916, 0.722, 0.7916, 0.285, 0.7659, 0.7379, 0.7138, 0.4876, 0.6303, 0.5311, 0.3525)

labels = append(rep(1,50), rep(0, 50))
tau = linspace(0,1,1000)
PF = rep(0, 1000)
PD = rep(0, 1000)
for (i in 0:1000) { 
    idx = scores <= tau[i]
    predict = rep(0,100)
    predict[idx] = 1
    true_positive  = 0
    true_negative  = 0
    false_positive = 0
    false_negative = 0
    
    for (j in 0:100) {
        a = predict[j]
        b = labels[j]
        cond1 = FALSE
        cond2 = FALSE
        if (a == 1) cond1 = TRUE
        
        
        if (b == 1) {
            cond2 = TRUE
        }
        if (cond1 == cond2 && cond1 == TRUE) {
            true_positive  = true_positive + 1
        }
        if (predict[j]==1  && labels[j]==0) {
            false_positive = false_positive + 1
        }
        if (predict[j]==0 && labels[j]==1) {
            
            false_negative = false_negative + 1
        }
        if (predict[j]==0 && labels[j]==0) {
            
            true_negative  = true_negative + 1
        }
    }
    PF[i] = false_positive/50
    PD[i] = true_positive/50
}


plot(PF, PD)
