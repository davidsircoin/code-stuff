def inc(i): return i + 1

def multiplier(a, b):
    y = 0
    atimesb = 0
    while y < b:
        x = 0
        while x < a:
            atimesb = inc(atimesb)
            x = inc(x)
        y = inc(y)

    return atimesb

# We need three things, a counter to keep track of how 
# many times we've added a to itself (should be b times),
# and a counter to keep
# track of how many times to add 1 to a (should be a times).
# The first counter that achieves this is the 
# counter y, as we run the second while loop, we will
# add 1 to y until it stops being smaller than b, then 
# we'll have known that we've added a to itself b times.
# The exact same thing is happening in the second loop.

def insertionsort(A,n):
    for j in [1,2,3,4]:
        key = A[j]
        i = j - 1
        while i >= 0 and A[i] >= key:
            A[i+1] = A[i]
            i = i - 1
        A[i+1] = key
        print(j, A)
    return A

A = [5, 4, 3, 1, 2]

def function(n, N):
    print("n", n, "N", N)
    if n <= 1: return N
    N += 1
    function(n//3, N)

function(100,0)



