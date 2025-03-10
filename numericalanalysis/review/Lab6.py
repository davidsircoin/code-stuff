import scipy
import numpy as np
import matplotlib.pyplot as plt
from math import pi

##########
# Part 1 #
##########

# 1.1 See picture1

# 1.2 
A = np.array([[10, 7, 8, 7], [7, 5, 6, 5], [8, 6, 10, 9], [7, 5, 9, 10]])
print('1-norm:', scipy.linalg.norm(A, 1))
print('infinity-norm:', scipy.linalg.norm(A, np.inf))

# It's the same as I got by hand

# 1.3

# a)
print('Condition number via cond:', np.linalg.cond(A))

# b)
print('Condition number via norm:', np.linalg.norm(np.linalg.inv(A))*np.linalg.norm(A)) #2-norm

# c)
print('Condition number via svdvals:', max(scipy.linalg.svdvals(A))/min(scipy.linalg.svdvals(A)))

# 2.1
# I'm not sure which matrix A is meant, but I guess it's the one we created to solve the BVP. It's symmetrical

# 2.2
def get_A(N):

    h = (pi/2)/(N+1)
    A = np.zeros((N,N))

    # Assembly
    A[0,0]=-(2/h**2)+1
    A[0,1]=1/h**2

    for i in range (1,N-1):
        A[i,i-1]=1/h**2
        A[i,i]=-(2/h**2)+1
        A[i,i+1]=1/h**2
        
    A[N-1,N-2]=1/h**2
    A[N-1,N-1]=-(2/h**2)+1

    return A

def cond(A):
    return np.linalg.cond(A)

N = [i for i in range(3, 20)]
C = [cond(get_A(k)) for k in N]

'''
plt.plot(N, C)
plt.show()
'''
# When N increases so does the condition number.

# 2.3
A = get_A(10)
print(np.linalg.eigvals(A))
# Since all eigenvalues are negative the matrix A is negative definite

# 2.4
# By just multiplying the matrix with -1 we go from positive definite to negative definite or vice versa.

# 2.5
plt.spy(A)
plt.show()

##########
# Part 2 #
##########

# 1.
def get_F(N, leftbc, rightbc):

    h = (pi/2)/(N+1)
    F = np.zeros(N)

    # Assembly
    F[0]=-leftbc/h**2

    for i in range (1,N-1):
        F[i]=0
    
    F[N-1]=-rightbc/h**2

    return F

def get_A_F_SPD(N, leftbc, rightbc):
    A = get_A(N)
    F = get_F(N, leftbc, rightbc)
    if max(np.linalg.eigvals(A)) < 0:
        return -1*A, -1*F
    else:
        return A, F

# 2.
def cg(A, b, x_cg, maxit):
    rk = b - np.dot(A, x_cg)
    p = rk
    for k in np.arange(maxit):
        alpha = np.dot(rk, rk)/np.dot(p, np.dot(A,p))
        x_cg = x_cg + alpha*p
        rkp1 = rk - np.dot(alpha, np.dot(A,p))
        beta = np.dot(rkp1, rkp1)/np.dot(rk, rk)
        p = rkp1 + beta*p
        rk = rkp1
    return x_cg

# 3.
def ComputeAnalyticalSolution(N, leftbc, rightbc):
    h=(pi/2)/(N+1)
    x=np.linspace(0 ,(pi/2), N+2)
    y=leftbc*np.cos(x)+rightbc*np.sin(x)
    return x,y

N = 100
leftbc = 1
rightbc = 2

A, b = get_A_F_SPD(N, leftbc, rightbc)
x_cg = np.zeros(N)

y_h_int = cg(A, b, x_cg, 99)
y_h = np.zeros(N+2)

y_h[0]=leftbc
y_h[1:N+1]= y_h_int
y_h[-1]=rightbc

x, y = ComputeAnalyticalSolution(N, leftbc, rightbc)

plt.plot(x, y_h, label = 'Conjugate Gradient Method')
plt.plot(x, y, label = 'Analytical')
plt.show()

# 4.
# At 99 iterations I barely see any difference, which is less than Jacobi that required 150 iterations for N = 10. 
# Thus CG is much more efficent than jacobi.
