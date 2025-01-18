def merge(A, low, middle, high):
    # Copy the sublists of A of size middle, and high - middle
    print("MERGE", low, middle, high)
    l1 = A[low : middle+1]   
    l2 = A[middle+1: high+1]
    i = low 
    while l1 and l2:
        if l1[0] <= l2[0]:
            A[i] = l1.pop(0)
        else: 
            A[i] = l2.pop(0)
        i += 1
    while l1:
        A[i] = l1.pop(0)    
        i += 1
    while l2: 
        A[i] = l2.pop(0)
        i += 1


def mergesort(A,low,high):
    print("MERGESORT", low, high)
    if low < high:
        middle = (low + high)//2
        mergesort(A, low, middle)
        mergesort(A, middle+1, high)
        merge(A, low, middle, high)
        print("MERGED", A[low:high+1])

import random

# Generate a list of 101 different integers in random order
#A = random.sample(range(1, 1000), 101)
A = [6,2,3]
print(A, len(A))

mergesort(A, 0, len(A))

print(A)