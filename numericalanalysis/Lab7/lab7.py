import numpy as np
import scipy as sp

def Create2by2Matrix(lambda_a, lambda_b):
    P = np.array([[0.5, 0.5], [0.2, 0.8]])
    D = np.array([[lambda_a, 0.0], [0.0, lambda_b]])
    Pinv = np.linalg.inv(P)

    return np.matmul(np.matmul(P, D), Pinv)

lambda_a = 11.999

lambda_b = 12

A = Create2by2Matrix(lambda_a, lambda_b)

x = np.ones(2)

def PowerIterate(A, x):
    b = np.matmul(A, x)
    b_norm =  sp.linalg.norm(b)
    lambda_ish = np.dot(x, b)
    xk = b/b_norm
    return xk, lambda_ish

xk, lambda_1 = PowerIterate(A, x)
k = 0
while abs(lambda_1 - lambda_b) > 0.0001:
    k += 1
    xk, lambda_1 = PowerIterate(A, xk)
    print(lambda_1)
print(k)


# Answer to question 3:
# The number of iterations needed until we converge increases.
# I.e. if we let lambda_a and  lambda_b equal to 11 and 12 respectively, we get
# k = 97 iteration with a tolerance of 0.0001. Instead, if we let lambda_a = 11.9,
# we suddenly need 733 iterations (all else the same).
# If we go even further and let lambda_a = 11.999, convergence only happens after 17665 iterations.
