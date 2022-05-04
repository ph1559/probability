#############
# Chapter 3.2 

## Histogram of the English alphabet

# Python code to generate the histogram
import numpy as np
import matplotlib.pyplot as plt
f = np.loadtxt('./ch3_data_english.txt')
n = np.arange(26)
plt.bar(n, f/100)
ntag = ['a','b','c','d','e','f','g','h','i','j','k','l','m',\
    'n','o','p','q','r','s','t','u','v','w','x','y','z']
plt.xticks(n, ntag)
plt.show()

## Histogram of the throwing a die

# Python code generate the histogram
import numpy as np
import matplotlib.pyplot as plt
q = np.random.randint(7,size=100)
plt.hist(q+0.5,bins=6)
plt.show()

## Histogram of an exponential random variable

# Python code used to generate the plots
import numpy as np
import matplotlib.pyplot as plt
lambd = 1
k = 1000
X = np.random.exponential(1/lambd, size=k)
plt.hist(X,bins=200);
plt.show()

## Cross validation loss

# Python code to perform the cross validation
import numpy as np
import matplotlib.pyplot as plt
lambd = 1
n = 1000
X = np.random.exponential(1/lambd, size=n)
m = np.arange(5,200)
J = np.zeros((195))

for i in range(0,195):
    hist,bins = np.histogram(X,bins=m[i])
    h = n/m[i]
    J[i] = 2/((n-1)*h)-((n+1)/((n-1)*h))*np.sum((hist/n)**2)

plt.plot(m,J);
plt.show()

#############
# Chapter 3.5 

## Bernoulli Random Variables

# Python code to generate 1000 Bernoulli random variables
import numpy as np
import matplotlib.pyplot as plt
p = 0.5
n = 1
X = np.random.binomial(n,p,size=1000)
plt.hist(X,bins='auto')
plt.show()

## Erdos Renyi Graph

# Python code to generate Erdos Renyi Graph
import numpy as np
import matplotlib.pyplot as plt
import networkx as nx
A = np.random.rand(40, 40) < 0.3
A = np.triu(A, 1)
A = A + A.T
G = nx.convert_matrix.from_numpy_matrix(A)
fig, ax = plt.subplots()
nx.draw(G, with_labels=True, ax=ax)
limits = plt.axis("on")
ax.tick_params(left=True, bottom=True, labelleft=True, labelbottom=True)
plt.show()

## Binomial Random Variables

# Python code to generate 5000 Binomial random variables
import numpy as np
import matplotlib.pyplot as plt
p = 0.5
n = 10
X = np.random.binomial(n,p,size=5000)
plt.hist(X,bins='auto');
plt.show()

## Binomial CDF

# Python code to plot CDF of a binomial random variable
import numpy as np
import matplotlib.pyplot as plt
import scipy.stats as stats
p = 0.5
n = 10
rv = stats.binom(n,p)
x  = np.arange(11)
F  = rv.cdf(x)
plt.step(x, F)
plt.show()

## Poisson CDF

# Python code to plot the Poisson PMF
import numpy as np
import matplotlib.pyplot as plt
from scipy.special import factorial
lambda_set = [1, 4, 10]
p = np.zeros((20, 3))
k = np.arange(0, 20)
for i in range(0, 3):
    lambd = lambda_set[i]
    p[:,i] = lambd**k/factorial(k)*np.exp(-lambd)

plt.plot(k, p[:,0], 'bo')
plt.vlines(k, 0 , p[:,0], colors='b', lw=2)
plt.plot(k, p[:,1], 'go')
plt.vlines(k, 0 , p[:,1], colors='g', lw=2)
plt.plot(k, p[:,2], 'yo')
plt.vlines(k, 0 , p[:,2], colors='y', lw=2)
labels = ["λ = 1", "λ = 4", "λ = 10"]
plt.legend(labels=labels)
plt.grid()
plt.show()

# Python code to plot the Poisson CDF
import numpy as np
import matplotlib.pyplot as plt
from scipy.special import factorial
lambda_set = [1, 4, 10]
p = np.zeros((20, 3))
k = np.arange(0, 20)
for i in range(0, 3):
    lambd = lambda_set[i]
    p[:,i] = lambd**k/factorial(k)*np.exp(-lambd)

plt.step(k, np.cumsum(p[:,0]), 'b')
plt.step(k, np.cumsum(p[:,1]), 'g')
plt.step(k, np.cumsum(p[:,2]), 'y')
plt.plot(k, np.cumsum(p[:,0]), 'bo')
plt.plot(k, np.cumsum(p[:,1]), 'go')
plt.plot(k, np.cumsum(p[:,2]), 'yo')
labels = ["λ = 1", "λ = 4", "λ = 10"]
plt.legend(labels=labels)
plt.grid()
plt.show()

## Poisson-Binomial approximation

# Python code to approximate binomial using Poisson
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import binom
from scipy.stats import poisson
n = 5000 
p = 0.01
lambd = n*p
x = np.arange(0, 120)
y = binom.pmf(x, n, p)
z = poisson.pmf(x, lambd)

plt.plot(x, y, 'go')
plt.vlines(x, 0, y , color='g', lw=2)
plt.plot(x, z, color='b')
labels = ["Binomial, n = 5000, p = 0.01", "Poisson, λ = 50"]
plt.legend(labels=labels)
plt.grid()
plt.show()

## Photon Shot Noise

# Python to demonstrate the photon shot noise
import numpy as np
import matplotlib.pyplot as plt
import cv2
x = cv2.imread('cameraman.tif', 0)/255
width = len(x)

alpha = 10
lam = alpha * x
X1 = np.random.poisson(lam=lam, size=(width, width))

alpha = 100
lam = alpha * x
X2 = np.random.poisson(lam=lam, size=(width, width))

alpha = 1000
lam = alpha * x
X3 = np.random.poisson(lam=lam, size=(width, width))

plt.figure(1)
plt.imshow(X1, cmap='gray')
plt.title("α = 10")
plt.figure(2)
plt.imshow(X2, cmap='gray')
plt.title("α = 100")
plt.figure(3)
plt.imshow(X3, cmap='gray')
plt.title("α = 1000")
plt.show()
