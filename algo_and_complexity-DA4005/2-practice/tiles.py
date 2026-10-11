import copy
import time
import matplotlib.pyplot as plt 
import sympy as sym
from collections import Counter

q = sym.symbols("q")

def P(n):
    '''
    Our tiling polynomial constructed via the recurrence relation proven in the report. The degree of q corresponds to how many of the 2xn tilings have deg(q) L-trominoes.
    '''
    if n == 0: return 1
    elif n == 1: return 2
    elif n == 2: return 4*q + 7
    else: return 3*P(n-1) + (4*q +1)*P(n-2) + (2*q**2 -1) * P(n-3)

def computePolyDynamically(n):
    '''
    Dynamic implementation of P(n), we save each iteration in memory and just do the next calculation based on the previous two, taking the runtime down from exponential to polynomial in n.
    '''
    if n <= 3: return P(n)
    else:
        out = 0
        a, b, c =  4*q + 7, 2, 1
        for k in range(3,n+1):
            out = 3*a + (4*q +1)*b + (2*q**2 -1)*c
            c = b
            b = a
            a = out
    return out
            

def total_tilings(n):
    '''
    This equals the sum of all the below tilings, but we have to count b1, b2 and b3 twice by symmetry, and we also have to count b4 three times. 
    '''
    if n == 1: return 2
    else:
        return (2*(b1(n) + b2(n) + b3(n)) + 3*b4(n))

def b1(n): 
    '''
    This number corresponds to the number of size 2xn tilings that end in two monominoes, or one vertical domino in the right-most column. They are the same as the total number of tilings on a board with 2x(n-1) tiles. 
    '''
    if n == 1: return 1
    else: 
        return total_tilings(n-1)

def b2(n):
    '''
    b2(n: int) corresponds to the number of tilings where we have a horizontal domino in one corner and a monomino in the other. By symmetry they are the same, and they correspond to the sum of number of tilings on n columns that allow for this configuration to exist in the nth column, if we were to extend the board to the right by one and force it to be like we described.
    '''
    if n == 1: return 0
    else:
        return b4(n-1) + b2(n-1) + b1(n-1)

def b3(n):
    '''
    Same as b2, but now with an L-tromino filling up the last column (so a rotated L or flipped L). They are again the same by symmetry, and we note that the number of configurations ending up in this equals the number b1(n-1) + b2(n-1) + b4(n-1).
    '''
    if n == 1: return 0
    else: 
        return b1(n-1) + b2(n-1) + b4(n-1)

def b4(n):
    '''
    Again, same as b3, but now this corresponds to the number of tilings with the last column filled up by a monomino and an L-tromino, or two horizontal dominoes (in the obvious way that fills the right-most column). since this takes up a square of 4 cells, this number is equivalent to the number of total tilings on n-2 cells.
    '''
    if n == 1: return 0
    elif n == 2: return 1
    else:
        return total_tilings(n-2)




class Board:
    '''
    Monominoes will be denoted by a single 1, dominoes by 2's, and L-trominoes
    by 3's, all in the shape they represent. For example 2, 2 denotes a
    horizontal domino. There are essentially 7 pieces to tile with, when
    treating the rotations, and we can treat these as different pieces because
    then in the end we are just summing up the number of 1's, 2's and 3's.

    The possible pieces are enumerated as follows:
    
    "mono" = 1

    "dominoH" =  2 2 
                 
                 3
    "dominoV" =  3

                  4
    "tromino1" = 44

                 5
    "tromino2" = 55

                 66
    "tromino3" =  6

                 77
    "tromino4" = 7

    '''
    
    
    def __init__(self, n) -> None:
        self.size = n
        self.boards = {}
        
        for k in range(1,self.size  + 1):
            self.boards[k] = [] # Store the possible 2 x k boards in a list.
            
        self.boards[1].append([[1,1]])
        self.boards[1].append([[3,3]])        
    
    

    def generate2xkTilings(self,k):
        prevBoards = self.boards[k-1]
        kBoards = self.boards[k]

        for tiling in prevBoards:
            possibleExtensions = extend(tiling[-1])
            for extension in possibleExtensions:
                kBoards.append(tiling[:-1] + extension)
    
    def generateAllTilings(self):
        for k in range(2,self.size + 1):
            self.generate2xkTilings(k)
    
    
    def printAllTilings(self, k):
        listOfTilings = self.boards[k]

        for tiling in listOfTilings:
            l1 = [col[0] for col in tiling]
            l2 = [col[1] for col in tiling]
            print(l1)
            print(l2, "\n")

    
def extend(cl):
    '''
    this only cares about the last column, since that contains all information
    on how we are allowed to extend. There are in total 9 possible columns, and
    each gives between 2, 4 or 7 new tilings after extending. This can
    absolutely be shortened to less cases by exploiting symmetry, but that is
    as much work as just writing more boilerplate so eh.
    
                                   2
    An example would be the column 1, then all possible extensions of this tiling are 


    2 1     2 4     2 3     2 1
    1 1     4 4     1 3     2 2 
    
    Note that we change the bottom monomino into a new tile-piece, and that
    this still ends up with a valid board. Meanwhile, there is no way to change
    the top piece of our horizontal domino since that would leave the other
    domino-end behind it alone.
    
    '''
    
    if cl == [1,1]:
        return [ [[1,1], [1,1]], [[6,1], [6,6]], [[2,2], [2,2]], [[1,4], [4,4]],
                [[1,1],[3,3]], [[2,1], [2, 1]], [[1, 2], [1, 2]] ]

    elif cl == [2,1]:
        return [ [[2,1], [1,1]], [[2,4], [4,4]], [[2, 1], [3, 3]], 
                [[2, 2], [1, 2]] ]
    
    elif cl == [1,2]:
        return [ [[1, 2], [1, 1]], [[1, 2], [3, 3]], [[6, 2], [6, 6]],
                [[2, 2], [2, 1]] ]

    elif cl == [4,4]:
        return [ [[4, 4], [1, 1]], [[4, 4], [3, 3]]]

    elif cl == [6,6]: 
        return [ [[6, 6], [1, 1]], [[6, 6], [3, 3]] ]

    elif cl == [2,2]:
        return [ [[2, 2], [1, 1]], [[2, 2], [3, 3]] ]

    elif cl == [3,3]:
        return [ [[3, 3], [1, 1]], [[3, 3], [3, 3]], [[7,7], [7,1]],
                [[5,5],[1,5]] ]

    elif cl == [1,5]:
        return [ [[1, 5], [1, 1]], [[1, 5], [3, 3]], [[2, 5], [2, 1]], 
                [[6, 5], [6, 6]] ]

    elif cl == [7,1]:
        return [ [[7, 1], [1, 1]], [[7, 1], [3, 3]], [[7, 2], [1, 2]], 
                [[7, 4], [4, 4]] ]
    else: return []


def main():
    tic = time.perf_counter()
    myb = Board(10)
    myb.generateAllTilings()
    myb.printAllTilings(10)
    print("(n, T(n)): ", [(0,1)] + [(k,len(myb.boards[k])) for k in range(1, 11)], "\n")
    toc = time.perf_counter()

    polynomial = sym.expand(computePolyDynamically(10))

    print("P_10 (q):", polynomial, "\n")
    print("running time in seconds: ", toc - tic)
    





main()
