def findmax(mylist):
    if len(mylist) == 1: 
        return mylist[0]
    k = len(mylist) // 2 
    left = findmax(mylist[0:k])
    right = findmax(mylist[k:  ])
    return max([left, right])
A = [5,9,3,8,2]

B = [i for i in range(100)]
#print(findmax(A))
print(findmax(A))