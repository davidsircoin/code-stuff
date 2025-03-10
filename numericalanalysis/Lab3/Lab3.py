# !/ usr / bin / python3
import numpy as np
import matplotlib . pyplot as plt
import scipy as sp

##### PART 1
tempdata = np.genfromtxt('/home/davidsircoin/vscode/python/numerical_analysis/Lab3/stockholm-historical-temps-monthly-3/csv/stockholm_monthly_mean_temperature_1756_2020_adjust.csv', delimiter=',')
year = tempdata [ 1 : len(tempdata), 0]
meantemperature = tempdata [ 1 : len (tempdata), -1]
#startindex table:
#250 -> 2006 
#224 -> 1980

## x for plotting
xsin = np . linspace (0 ,1 , 100 )
## nof interpolatin points
N = 30
## x - coords for interpolation points
xcoarse = np . linspace (0 ,1 , N )
# exact function
ysin = np . sin ( 2 * np.pi * xsin )
# exact function in interpolation points
ysincoarse = np . sin ( 2 * np.pi * xcoarse )
# linear interpolation
flinsin = sp.interpolate . interp1d ( xcoarse , ysincoarse , kind = 'linear')
ylininterpsin = flinsin ( xsin )
# Cubic spline interpolation
fcubsin = sp.interpolate . interp1d ( xcoarse , ysincoarse , kind = 'cubic')
ycubinterpsin = fcubsin ( xsin )
# Plotting
plt . plot ( xcoarse , ysincoarse , '*' , label = 'x_k')
plt . plot ( xsin , ysin , label = 'Exact')
plt . plot ( xsin , ylininterpsin , label = 'Linear')
plt . plot ( xsin , ycubinterpsin , label = 'Cubic Spline')
plt . legend ()
plt . show ()

# Code for Linear interpolation:
def linear_interpolation(startindex):
    x = year[startindex:]
    y = meantemperature[startindex:]
    x_fine = np.linspace(x[0], x[-1], 10000)
    linear_int = sp.interpolate.interp1d(x,y, kind = 'linear')
    y_linear = linear_int(x_fine)

    plt.scatter(x,y, color='red', label='x_k')
    plt.plot(x_fine, y_linear, label = 'Linear')
    plt.legend()
    plt.savefig('./Lab3/davidfigs/linear_interpolation.png')
    plt.show()

### Code for Lagrange interpolation
def lagrange_interpolation(startindex):   
    x = year[startindex:]
    y = meantemperature[startindex:]
    x_fine = np.linspace(x[0], x[-1], 10000)
    nice_x = list(range(len(x)))
    x_new = np.linspace(nice_x[0], nice_x[-1], 10000)
    lagrange = sp.interpolate.lagrange(nice_x,y)
    y_lagrange = lagrange(x_new)

    plt.scatter(x,y, color='red', label='x_k')
    plt.plot(x_fine, y_lagrange, label = 'Lagrange')
    plt.legend()
    plt.savefig(f'./Lab3/davidfigs/lagrange_interpolation_SI-{startindex}.png')
    plt.show()
    return lagrange

def cubic_splines(startindex):
    x = year[startindex:]
    y = meantemperature[startindex:]
    x_fine = np.linspace(x[0], x[-1], 10000)
    splines = sp.interpolate.interp1d(x,y, kind = 'cubic')
    y_cubic = splines(x_fine)
    plt.scatter(x,y, color='red', label='x_k')
    plt.plot(x_fine, y_cubic, label = 'Cubic')
    plt.legend()
    plt.savefig(f'./Lab3/davidfigs/cubic_splines_SI-{startindex}.png')
    plt.show()


linear_interpolation(259)

poly = lagrange_interpolation(259)
poly2c = np.polynomial.Polynomial(poly.coef[::-1])

poly = lagrange_interpolation(240)
poly2d = np.polynomial.Polynomial(poly.coef[::-1])

cubic_splines(249)
cubic_splines(240)

cubic_splines(259)

print('PART 1')
print('Answer 1.1:\t At N = 9 you can\'t tell the difference.')
print('Answer 1.2\t Around N = 30 then the linear and exact start to match one-to-one')
print('Answer 1.3:\t The areas where the second derivative is high/low is where we get \
the biggest error')

print('PART 2')
print('Answer 2.2:\t')
print(f'Answer 2.3: The lagrange polynomial when startindex = 259: \n {poly2c}.\n')
print(f'Answer 2.4: When startindex = 240, we get the polynomial \n {poly2d}.\n\n \
Here the coefficients become really big or really small, \
and the polynomial is fluctuates a lot between each datapoint. This is caused \
by runge\'s phenomenon, which says that the error can increase dramatically \
if the highest order derivative is very big. \
Therefore the accuracy is really bad in comparison to linear interpolation.\n')
print('Answer 2.6: ')

### PART 2

print('Part 2.1.1')
print('Answer: The order is 2')
print('part 2.1.2')
print('Answer: y(t)= (5/2)*t^2 +1')
print('part 2.1.3')
print('Answer:y(0)=35')

print('part 2.2.1')
fig, (ax0, ax1, ax2, ax3) = plt.subplots(ncols = 4)
sinder = 2*np.pi*np.cos(2*np.pi*xsin)
ax0.plot(xsin, sinder, label = 'Reference dy/dx', color = 'red')
print('part 2.2.2')
for i in [10,20,30]:
    N = i
    dx = 1/N
    xcoarse = np.linspace(0,1,N)
    def f(x): return np.sin(2*np.pi*x)
    xplusdx = np.array([n + dx for n in xcoarse])
    dsin = (f(xplusdx) - f(xcoarse))/dx
    ax1.plot(xcoarse, dsin)


print('Answer 2.2.2: Definitely a phase-error')
print('part 2.2.3')

print('part 2.2.2')
for i in [10,20,30]:
    N = i
    dx = 1/N
    xcoarse = np.linspace(0,1,N)
    def f(x): return np.sin(2*np.pi*x)
    xminusdx = np.array([n-dx for n in xcoarse])
    dsin = (f(xcoarse)- f(xminusdx))/dx
    ax2.plot(xcoarse, dsin)




print('Answer 2.2.3: The difference seems to be in the fact that with the forward \
difference, we get a phase backwards the coarser we go. Meanwhile, the opposite \
happens when using the backward difference.')
print('part 2.2.4')


for i in [10,20,30]:
    N = i
    dx = 1/N
    xcoarse = np.linspace(0,1,N)
    xminusdx = np.array([n-dx for n in xcoarse])
    xplusdx = np.array([n + dx for n in xcoarse])
    def f(x): return np.sin(2*np.pi*x)
    dsin = (f(xplusdx) - f(xminusdx))/dx
    ax3.plot(xcoarse, dsin,  label = f'N: {N}')

ax0.set_title('Reference dy/dx')
ax1.set_title('Forward difference')
ax2.set_title('Backward difference')
ax3.set_title('Center difference')
fig.subplots_adjust(left=0,right=1)
fig.legend(loc='upper left')

plt.show()

print('Answer:')
print('part 2.2.5-6')
print('Answer: The order is 2, it is noticeable from my plots that this \
should be the case, as the accuracy is pretty close to the actual result \
at small values of N.')
