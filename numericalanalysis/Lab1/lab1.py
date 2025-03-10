import scipy as sc 
import numpy as np
import matplotlib.pyplot as plt

### Part 1.1
print('Part 1')
print('Definite integral of x^3 in [0,1].')

### Part 1.2
print('\n')
print('Part 2')
print('Using the trapezoidal rule we get 0.26')

### Part 1.3
print('\n')
print('Part 3')
def trapezoidal_int(x_min, x_max, function, N):
    dx = (x_max - x_min)/N
    x = np.linspace(0,1, N+1)
    f = function(x)
    return (dx/2)* f[0] + dx*(np.sum(f[1:N])) + (dx/2)* f[N]

print('Answer:\n',
'def trapezoidal_int(x_min, x_max, function, N): \n \
    dx = (x_max - x_min)/N \n \
    x = np.linspace(0,1, N+1) \n \
    f = function(x) \n \
    return (dx/2)* f[0] + dx*(np.sum(f[1:N])) + (dx/2)* f[N]')

### Part 1.4
print('\n')
print('Part 4')

def cubic(x):
    return x**3
N = 5
print('Answer:', trapezoidal_int(0,1, cubic, N))

### Part 1.5
print('\n')
print('Part 5')
correct_value = 0.25
result = trapezoidal_int(0,1,cubic, N)
while abs(result - correct_value) > 0.001:
    N += 1
    result = trapezoidal_int(0,1,cubic, N)

print('Answer:', 'N =', N)

### Part 1.6
print('\n')
print('Part 6')
x = np.linspace(0,1,6)
f = cubic(x)
print('Answer:', np.trapz(f,x))

### Part 1.7
print('\n')
print('PART 7')
print('scipy.simps answer:', sc.integrate.simps(f,x)) 
print('scipy.trapz answer:', sc.integrate.trapz(f,x))
print('The accuracy using Simpson\'s rule is higher than when  using \
the trapezoidal rule.')
      
### Roundation Errors

### Part 1
print('\n', 'Part 1.a')
print(0.1*(0 + 1.7 + 1.9 + 2.3 + 1.7 + (1/2)*1.2))
print('\n', 'Part 1.b')
print('The worst case scenario is when \epsilon is approaching 0.05, \
then the maximum error is given by', 0.1*5*0.05, 'Therefore, the worst \
case scenario is given by error < 0.025')

### Part 2
print(0.1 + 0.1 + 0.1 == 0.3) # False
print('The reason has to do with 0.1 not having a simple decimal expansion in binary.\
Meanwhile 0.1+0.1 == 0.2 returns True, since that operation is much nicer in binary.')

### Part 3
print('\n', 'Part 3')
print('machine epsilon:', format(np.finfo(float).eps, '.20f'))
print(0.25 + np.finfo(float).eps/2 == 0.25)

### Part 4
print('\n', 'Part 4')
c =  0.250000000000002-0.250000000000001
print('by hand we get the difference to be 0.0000000000000001')
print('difference in python is', format(c, '.20f'))
print('Error is given by', format(abs(c-10**(-15))/10**(-15), '.20f'))

print('Truncation Error:')

### Part 1.a
print('\n', 'Part 1a-e')
absolute_err = abs(0.25 - trapezoidal_int(0,1, cubic, 5))
relative_err = absolute_err/0.25
print(f'absolute: {absolute_err}')
print(f'relative: {relative_err}')

    
def plotstuff(x_axis, y_axis, name):
    fig, ax = plt.subplots(1,2)
    fig.suptitle(f'f(x) = {name}')
    ax[0].plot(x_axis, y_axis)
    ax[0].set_xlabel('dx')
    ax[0].set_title('Change in error plot')

    ax[1].loglog(x_axis, y_axis)
    ax[1].set_xlabel('log(dx)')
    ax[1].set_title('Log-Log plot')

    plt.show()
    return None

ranges = list(range(2000))
ranges = np.array(ranges[1:])

errvec = []

for i in ranges:
    errvec.append(abs(0.25 - trapezoidal_int(0,1, cubic, i))/0.25)

dxvec = [1/n for n in ranges]

errvec = np.array(errvec)

plotstuff(dxvec, errvec, 'x³')

p = np.polyfit(np.log(dxvec), np.log(errvec), 1)
print(p[0])

### Part 2
print('\n', 'Part 2a-b')
def quartic(x):
    return x**4

exact_value = 1/5
N = range(2000)
intervals = list(N)[100::2]
simp_err_values = []
for interval in intervals:
    x = np.linspace(0,1,interval+1)
    f = quartic(x)
    simp_err_values.append(abs(exact_value - sc.integrate.simps(f, x))/exact_value)


e = np.finfo(float).eps
i = 2
x = np.linspace(0,1,i)
f = quartic(x)
relative_error = abs(exact_value - sc.integrate.simps(f,x))/exact_value



x_axis = [1/n for n in intervals]
y_axis = np.array(simp_err_values)

p = np.polyfit(np.log(x_axis), np.log(y_axis), 1)

print('Answer a): The exact value of the integral is 0.2\n')
print(f'Answer b): The order is given by {p[0]}\n')
plotstuff(x_axis, y_axis, 'x⁴')

print('Part 2.cd')
print(f'Answer 2c): The roundation errors only start to\
take over after the number of intervals approaches the thousands, around N = 1500\
we can start to notice fluctuations in the graph')
print('OBS: I don\'t know why but you really have to zoom in to start to notice the changes...')
print(f'Answer d): Already at N > 4000, you can start noticing that it takes way longer.')




