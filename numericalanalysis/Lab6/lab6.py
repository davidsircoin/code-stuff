import numpy as np
import scipy as sp
from math import pi
import matplotlib.pyplot as plt
import lab5 as l5


def main():
    pass
## PART 1.1 ##

## PART 1.2 ## 

A = np.array([[10,7,8,7], [7,5,6,5], [8,6,10,9], [7,5,9,10]])
Ainv = np.linalg.inv(A)
A_norm1 = sp.linalg.norm(A, 1)

## PART 1.3 ##

#a.
kappaA_normfro = np.linalg.cond(A, 'fro')

#b.
Ainv_norm1 = np.linalg.norm(Ainv,1)
kappaA_norm_1 = A_norm1 * Ainv_norm1

#c.
A_singval = sp.linalg.svdvals(A)

kappaA_norm2 = max(A_singval)/min(A_singval)

print(f'a: {kappaA_normfro}')
print(f'b: {kappaA_norm_1}')
print(f'c: {kappaA_norm2}')

## PART 2.1 ##
#yes.

## PART 2.2 ##


def assembly_of_A(N, h): # Finite Difference Scheme for second order derivative
    A = np . zeros (( N , N ) )

    A [0 , 0 ] =-2 + h**2
    A [0 , 1 ] = 1

    for i in range (1 , N - 1 ) :
        A [i , i - 1 ] = 1
        A [i , i ] = -2 + h**2
        A [i , i + 1 ] = 1

    A [ N -1 , N - 2 ] = 1
    A [ N -1 , N - 1 ] = -2 + h**2
    A = (1/h**2)*A
    return A

for N in range(10, 100):
    h = pi/2/(N + 1)
    A = assembly_of_A(N,h)


# Answer: It seems to be increasing.

## PART 3 ##
N = 20
h = pi/2/(N+1)
A = assembly_of_A(N, h)

print(np.linalg.eigvals(A))

# Answer: It's definitely negative definite

## PART 4 ##


## PART 5 ##

plt.spy(A)
plt.show()


#################################################


def cg(A, b, x_cg, maxit):
    rk = b - np.dot(A,x_cg)
    p = rk
    for k in np.arange(maxit):
        alpha = np.dot(rk,rk)/np.dot(p,np.dot(A,p))
        x_cg = x_cg + alpha*p
        rkp1 = rk - np.dot(alpha*A,p)
        beta = np.dot(rkp1,rkp1)/np.dot(rk,rk)
        p = rkp1 + beta*p
        rk = rkp1
    return x_cg

N = 100
h = pi/(2*(N+1))
rightc = 2
leftc = 1

b = - l5.assembly_of_F(N, h, leftc, rightc)
A = - l5.assembly_of_A(N, h)
interval, x_analytical = l5.ComputeAnalyticalSolution(N, leftc, rightc)



fig, ax = plt.subplots()

for i in range(1, 102, 4):
    x_cg = np.ones(N + 2)
    x_cg[0] = leftc
    x_cg[-1] = rightc
    x_cg[1:-1] = cg(A, b, x_cg[1:-1], i)
    ax.plot(interval, x_cg, label = f'maxit = {i}')


ax.plot(interval, x_analytical, label = 'Analytical')

fig.legend()

plt.show()

# ANSWERS TO QUESTIONS LAB 6 PART 2

# After 100 iterations we are very close to the original solution

# It seems way more efficient.

