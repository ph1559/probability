#############
# Chapter 1.1

## Visualizing a geometric series

# Python code to generate a geometric sequence
import numpy as np
import matplotlib.pyplot as plt
p = 1/2
n = np.arange(1,10)
X = np.power(p,n)
plt.bar(n,X)
plt.show()

#############
