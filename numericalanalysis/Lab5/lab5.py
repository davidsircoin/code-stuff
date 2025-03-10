import numpy as np
import matplotlib . pyplot as plt
from math import pi
import scipy as sp
import timeit as tt

def ComputeAnalyticalSolution(N , leftbc , rightbc ):
    h = ( pi / 2 ) / ( N + 1 )
    x = np . linspace (0 ,( pi / 2 ) ,N + 2 )
    y = np.cos(x) + 2*np.sin(x)
    return x , y


def ComputeNumericalSolution1(N , leftbc , rightbc ):
    h = ( pi / 2 ) / ( N + 1 )
    x = np . linspace (0 ,( pi / 2 ), N + 2 )
    A = np . zeros (( N , N ) )
    F = np . zeros ( N )
    # Assembly
    A [0 , 0 ] =-2 + h**2
    A [0 , 1 ] = 1
    F [ 0 ] = 0 - leftbc/h**2
    for i in range (1 , N - 1 ) :
        A [i , i - 1 ] = 1
        A [i , i ] = -2 + h**2
        A [i , i + 1 ] = 1
        F [ i ] = 0

    A [ N -1 , N - 2 ] = 1
    A [ N -1 , N - 1 ] = -2 + h**2
    F [ N - 1 ] = 0 - rightbc/h**2
    A = (1/h**2)*A
    y_h_int = np . linalg . solve (A , F )
    y_h = np . zeros ( N + 2 )
    y_h [ 0 ] = 1
    y_h [ 1 : N + 1 ] = y_h_int
    y_h [ - 1 ] = 2
    return x , y_h , h


leftbc = 1
rightbc = 2
N = 3
x , y = ComputeAnalyticalSolution(N , leftbc , rightbc)
x_h , y_h , h = ComputeNumericalSolution1(N , leftbc , rightbc )
plt.plot (x ,y , label = 'analytical')
plt.plot ( x_h , y_h , label = 'numerical')
plt.legend()
print(y_h)
plt.show()


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

def assembly_of_F(N, h, leftbc, rightbc):
    F = np.zeros(N)
    F[0] = 0 - leftbc/h**2
    for i in range(1, N-1):
        F[i] = 0
    F[N-1] = 0 - rightbc/h**2
    return F


def ComputeNumericalSolution(N, h, leftbc , rightbc, matrix, column_vec):
    y_h_int = np . linalg . solve (matrix, column_vec)
    y_h = np . zeros ( N + 2 )
    y_h [ 0 ] = leftbc
    y_h [ 1 : N + 1 ] = y_h_int
    y_h [ - 1 ] = rightbc

    return y_h


def ComputeNumericalSolutionLU(N, h, leftbc , rightbc, matrix, column_vec):
    y_h_int = sp.linalg.lu_solve(matrix, column_vec)
    y_h = np . zeros ( N + 2 )
    y_h [ 0 ] = leftbc
    y_h [ 1 : N + 1 ] = y_h_int
    y_h [ - 1 ] = rightbc

    return y_h

def main_2():

    N = 10
    leftc = 1
    def rightc(t): return 2*np.sin(pi*t)
    h = pi/2/(N + 1)

    x_exact, y_exact = ComputeAnalyticalSolution(N, leftc, rightc(1/2))

    A = assembly_of_A(N, h)
    F = assembly_of_F(N,h, leftc, rightc(1/2))

    # Solving via Gauss-Elim

    starttime = tt.default_timer()
    for t in range(1,100):
        F = assembly_of_F(N, h, leftc, rightc(t))
        ComputeNumericalSolution(N, h, leftc, rightc(t), A, F)
    endtime = tt.default_timer()

    print(f'Solvetime for GJ-elim is {endtime - starttime}')
   
    # Solving via LU-factorization

    starttime = tt.default_timer()
    LU, P = sp.linalg.lu_factor(A)
    for t in range(1,100):
        F = assembly_of_F(N, h, leftc, rightc(t))
        ComputeNumericalSolutionLU(N, h, leftc, rightc(t), (LU, P), F)
    endtime = tt.default_timer()
    
    print(f'Solvetime for LU-method is {endtime - starttime}')

    # Solving via Jacobi Method
    
    N = 5
    h = (pi/2)/(N+1)
    
    A = assembly_of_A(N, h)
    B = assembly_of_F(N, h, leftc, rightc(1/2))
    D = np.diagflat(np.diag(A))
    LU = D - A
    Dinv = np.linalg.inv(D)
    R = np.matmul(Dinv,LU)
    x0 = np.ones(N)
    y_k = x0

    fig, ax = plt.subplots(1)

    for i in list(range(100)):
        y_k =  np.matmul(R,y_k) + np.matmul(Dinv, B)
        y_jacobi = np.zeros(N+2)
        y_jacobi[0] = leftc
        y_jacobi[-1] = rightc(1/2)
        y_jacobi[1 : N+1] = y_k
        xinterval = np.linspace(0, pi/2, N+2)
        if i > 80:
            ax.plot(xinterval, y_jacobi, label = 'Jacobi k = ' + str(i))
        else:
            ax.plot(xinterval, y_jacobi)
    

    ax.plot(x_exact, y_exact, label = 'Analytical')
    fig.legend()
    plt.show()

    N = 10
    h = (pi/2)/(N+1)
    
    A = assembly_of_A(N, h)
    B = assembly_of_F(N, h, leftc, rightc(1/2))
    D = np.diagflat(np.diag(A))
    LU = D - A
    Dinv = np.linalg.inv(D)
    R = np.matmul(Dinv,LU)
    x0 = np.ones(N)
    y_k = x0

    fig, ax = plt.subplots(1)

    for i in list(range(130)):
        y_k =  np.matmul(R,y_k) + np.matmul(Dinv, B)
        y_jacobi = np.zeros(N+2)
        y_jacobi[0] = leftc
        y_jacobi[-1] = rightc(1/2)
        y_jacobi[1 : N+1] = y_k
        xinterval = np.linspace(0, pi/2, N+2)
        if i > 110:
            ax.plot(xinterval, y_jacobi, label = 'Jacobi k = ' + str(i))
        else:
            ax.plot(xinterval, y_jacobi)
    

    ax.plot(x_exact, y_exact, label = 'Analytical')
    fig.legend()
    plt.show()


main_2()


#### ANSWERS TO ALL QUESTIONS

# 1.2.2 The matrix looks like the following:
#
# [[-1.84578743  1.          0.        ]
# [ 1.         -1.84578743  1.         ]
# [ 0.          1.         -1.84578743]]


# 1.2.3 The error is the biggest close to the middle of the interval. As this is where 
# the BCs have the least influence over the approximation.

# 1.2.5 np.linalg.solve()



# 2.2.5 The time is drastically less, since we only apply the algorithm on A once. For each new solution b,
# we have to Gauss-eliminate anew, we only need to LU-factorize once.

# 2.2.6 


# 3.1 It doesn't

# 3.4 After 95 iterations the solution seems acceptable

# 3.5 For N = 10 it seems the number of iterations needed is around 120



