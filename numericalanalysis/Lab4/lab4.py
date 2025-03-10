import numpy as np
import matplotlib.pyplot as plt

def dydx1(y, t):
    return -0.5*y

fig, ax1 = plt.subplots(nrows=1,ncols=1)
def euler_forward(dydx,y0,h,xmin,xmax):
    values = [y0]
    for n in np.arange(xmin, xmax, h):
        y_nplus1 = y0 + h*(dydx(y0, n))
        y0 = y_nplus1 
        values.append(y_nplus1)
    return values

x = np.arange(0,10.1,1)
y = euler_forward(dydx1, 1, 1, 0, 10)
ax1.plot(x, y, color = 'red', label = f'explicit, h = {1}')
x = np.arange(0,10,0.01)
ax1.plot(x, np.exp(-0.5*x), color = 'green', label = 'exp(x)')


#####Implicit Euler#####
def euler_backward(dydx, y0, h, xmin,xmax):
    values = [y0]
    x = np.arange(xmin,xmax,h)
    yold = y0
    for n in x:
        y_nplus1 = yold/(1+0.5*h)
        yold = y_nplus1
        values.append(y_nplus1)
    return values
y = euler_backward(dydx1, 1, 1, 0,10)
ax1.plot(np.arange(0,10.1, 1), y, label = f'implicit, h = {1}')
fig.legend()
plt.show()

def plot_all_values(xmin,xmax, T, spacing):
    fig, ((ax1,ax2,ax3)) = plt.subplots(nrows=1,ncols=3)
    ex = np.arange(0,T, 0.01)
    ax1.set_title('exp(x)')
    ax1.plot(ex, np.exp(-0.5*ex))
    for h in np.arange(xmin, xmax, spacing):
        explicit = euler_forward(dydx1, 1, h, 0, T)
        implicit= euler_backward(dydx1, 1, h, 0, T)
        x = np.arange(0,T+h,h)
        ax2.set_title('Forward')
        ax3.set_title('Backward')
        ax2.plot(x, explicit)
        ax3.plot(x,implicit, label = f'h = {h}')
    fig.legend()        
plot_all_values(0.1,1.1, 10, 0.5)
plt.show()
plot_all_values(3.7,4.2, 300, 0.1)

plt.show()



def RK2(dydx, y0, h, T): 
    values = [y0]
    x = np.arange(0,T+h, h)
    for t in np.arange(0, T, h):
        k1 = dydx(y0,t)
        k2 = dydx(y0+h*0.5*k1, t+h*0.5)
        yplus1 = y0 + h*k2
        y0 = yplus1
        values.append(yplus1)
    return x, values

fig, ax = plt.subplots(1,2)

x,y = RK2(dydx1, 1, 0.1, 10)
ex = np.arange(0,10.1, 0.1)
ey = euler_forward(dydx1, 1, 0.1, 0, 10)

ax[0].set_title('h=0.1')
ax[0].plot(x,y, label = 'RK2')
ax[0].plot(ex,ey, label = 'Euler')
ax[0].plot(x, np.exp(-0.5*x), color = 'green', label = 'exp(x)')
x,y = RK2(dydx1, 1,0.05, 10)
ex = np.arange(0, 10.05, 0.05)
ey = euler_forward(dydx1, 1, 0.05, 0, 10)
ax[1].set_title('h= 0.05')
ax[1].plot(x,y, label = 'RK2')
ax[1].plot(ex,ey, label = 'Euler')
ax[1].plot(x, np.exp(-0.5*x), color = 'green', label = 'exp(x)')

fig.legend()
plt.show()

def dydx2(y, t):
    return y - t**2

x,y = RK2(dydx2, 1, 0.1, 10)

def exact_solution(x):
    return 2+2*x+x**2 -np.exp(x)

fig, ((ax1, ax2)) = plt.subplots(nrows=1, ncols=2)

exact_y = exact_solution(x)
ax1.plot(x, y, label = 'RK2, h =0.1')
ax1.plot(x,exact_y, label = 'exact')
ey = euler_forward(dydx2, 1, 0.1, 0,10)
ax1.plot(x, ey, label = 'EF')

x,y = RK2(dydx2,1,0.05, 10)
ey = euler_forward(dydx2, 1, 0.05,0,10)
exacty= exact_solution(x)
ax2.plot(x,y,label = 'RK2 h = 0.05')
ax2.plot(x,exacty, label = 'exact')
ax2.plot(x, ey, label = 'EF')
ax2.legend()
ax1.legend()
plt.show()

## Error Analysis
fig, ax = plt.subplots(1,2)
intervals_n = list(range(1000))
dxvalues = [1/n for n in intervals_n[6::]]
error_values = []

print('\nERROR ANALYSIS FOR EXP(X)')
for h in [0.1, 0.05]:
    x, rk2 = RK2(dydx1,1, h, 10)
    eulerf = euler_forward(dydx1, 1, h, 0,10)
    x_fine = np.arange(0,10, 0.00001)
    exact = np.exp(-0.5*x_fine)
    rk2atT = rk2[-1]
    eulerfatT = eulerf[-1]
    exactatT = exact[-1]
    abs_err = np.abs(exactatT - rk2atT)
    rel_err = abs_err/exactatT
    error_values.append(rel_err)
    print(f'RELATIVE ERROR RK2 (dx = {h}): \t', rel_err)
    print(f'ABSOLUTE ERROR RK2 (dx = {h}): \t', abs_err)
    abs_err = abs(eulerfatT - exactatT)
    rel_err = abs_err/exactatT
    print(f'ABSOLUTE ERROR EULER F (dx = {h}): \t', abs_err)
    print(f'RELATIVE ERROR EULER F (dx = {h}): \t', rel_err)

print('\n ERROR ANALYSIS FOR Y = 2 +2T +T² -E^t')
for h in [0.1, 0.05]:
    x, rk2 = RK2(dydx2,1, h, 10)
    eulerf = euler_forward(dydx2, 1, h, 0,10)
    x_fine = np.arange(0,10, 0.00001)
    exact = exact_solution(x_fine)
    rk2atT = rk2[-1]
    eulerfatT = eulerf[-1]
    exactatT = exact[-1]
    abs_err = np.abs(rk2atT- exactatT)
    rel_err = abs_err/exactatT
    error_values.append(rel_err)
    print(f'RELATIVE ERROR RK2 (dx = {h}): \t', rel_err)
    print(f'ABSOLUTE ERROR RK2 (dx = {h}): \t', abs_err)
    abs_err = abs(eulerfatT - exactatT)
    rel_err = abs_err/exactatT
    print(f'ABSOLUTE ERROR EULER F (dx = {h}): \t', abs_err)
    print(f'RELATIVE ERROR EULER F (dx = {h}): \t', rel_err)


def plotstuff(x_axis, y_axis, name):
    fig, ax = plt.subplots(1,2)
    fig.suptitle(f'f(t) = 2+2t+t^2 - e^t')
    ax[0].plot(x_axis, y_axis)
    ax[0].set_xlabel('dx')
    ax[0].set_title('Change in error plot')

    ax[1].loglog(x_axis, y_axis)
    ax[1].set_xlabel('log(dx)')
    ax[1].set_title('Log-Log plot')

    plt.show()
    return None


#plotstuff(dxvalues, error_values, 'loglog')


#### Answers to all Questions####
### Answer 1.4: The error seems very comparable. In fact it looks the same.
### Answer 1.5: Right after h = 4 we get unstable behaviour.
### Answer 2.3: The error of RK2 is about a fourth of the previous one, which checks out since the order of RK2 is h²
###             Same with Euler forward, the error is halved, which checks out since the order is linear.



