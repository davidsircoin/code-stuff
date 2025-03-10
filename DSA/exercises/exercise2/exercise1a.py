import math
stack = [5,4,3,2,1] 
queue = [5,4,3,2,1]
#queue = [95 - i for i in range(95)]
print(queue)
def drinkshrink(str):
    pass

def shirecat(stack, queue):
    i = 0
    while stack and queue:
        if queue[0] == stack[-1]:
            stack.pop()
        else: 
            queue.append(queue[0])
        queue.pop(0)
        i += 1
    print("done", "i=", i)
def heapsort():
    pass

print(shirecat(stack, queue))

A = [8,123,3,1,2,5,6,9]

def partition(A, p, r):
    pivot = A[r]
    i = p - 1
    for j in list(range(p,r)):
        if A[j] <= pivot:
            i = i+1
            A[i] = A[j]
        A[i+1] = A[r]
    return i + 1

def quicksort(A, p, r):
    if p < r:
        q = partition(A, p, r)
        quicksort(A, p, q-1)
        quicksort(A, q+1, r)
print(A)
quicksort(A, 1, len(A) -1)
print(A)


n = 100
rooms = {}
for i in range(1,n+1):
    rooms[i] = None


people_amount = int(math.log2(n))
            
from itertools import combinations

powerset = lambda s: [list(subset) for r in range(len(s) + 1) for subset in combinations(s, r)]

# Example usage:
my_list = [1, 2, 3]
print("Powerset of", my_list, ":", powerset(my_list))