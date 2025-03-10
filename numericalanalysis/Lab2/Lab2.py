import numpy as np
import scipy as sc
import matplotlib.pyplot as plt

print('Part 1')
def f0(x):
    return x**3 - x -1

def sgn(function_value):
    if function_value > 0:
        return '+'
    elif function_value < 0:
        return '-'
    else: return 0


def bisection_method(f, a, b, tolerance):
    e = tolerance
    c = (a +b)/2
    if abs(f(c)) < tolerance:
        return c
    elif sgn(f(c)) != sgn(f(a)):
        return bisection_method(f,a,c,e)
    elif sgn(f(c)) != sgn(f(b)):
        return bisection_method(f,c,b,e)
    
print('Answer: The bisection method returns c =', bisection_method(f0,0,2,1/50))
print('\n')

print('Part 2.a')
def newtons_method(guess, f, df, e):
    x0 = guess

    while abs(f(x0)) > e:
        xk = x0 - f(x0)/df(x0)
        x0 = xk
    return x0

def deltaf0(x):
    return 3*x**2- 1

print(f'Answer: Newton\'s method with e = 0.001 gives xk =\
{newtons_method(1.5, f0, deltaf0, 10**(-3))}')
print('\n')

print('Part 2.b')

def f1(x):
    return x**2 - 1 

def deltaf1(x):
    return 2*x

guess1, guess2, guess3 = 1.5, 5, 0
guess4, guess5, guess6 = 3,-12, -2
print(f'guess1= 1.5 returns \
{newtons_method(guess1, f1, deltaf1, 10**(-3))}\n')
print(f'guess2=5 returns \
{newtons_method(guess2, f1, deltaf1, 10**(-3))}\n')
print(f'guess3 = 0 returns ZeroDivisionError because df(0)=2*0=0\n')
print(f'guess4=3 returns \
{newtons_method(guess4, f1, deltaf1, 10**(-3))}\n')
print(f'guess5 =-12 returns \
{newtons_method(guess5, f1, deltaf1, 10**(-3))}\n')
print(f'guess6 =-2 returns \
{newtons_method(guess6, f1, deltaf1, 10**(-3))}\n')

