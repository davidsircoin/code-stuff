import sys

from tape import Tape, Symbol as Sym

# ALL functions defined on tapes, except for addBegin, rewind, and moveRightEnd assume
# that the input tape is at position BEGIN. Furthermore, each function rewinds the
# tape back to the right of BEGIN at the end of computation.
# The first BLANK on any tape will always only be followed by BLANKS. I.e., we never read a blank, go to the right, and write something there instead.


def moveRightEnd(tape): 
    """
    Moves to the first BLANK on the tape.
    """
    while tape.read() is not Sym.BLANK:
        tape.move_right()

def rewind(tape):
    """
    Goes back to the the first instance of BEGIN on the tape. After a tape is initialized, it will get a BEGIN at the start, and some tapes like Dummy, will have multiple BEGIN symbols on its tape, and that is so that it can be reused multiple times.
    """
    while tape.read() is not Sym.BEGIN:
        tape.move_left()
    tape.move_right()

def addBegin(tape):
    """
    adds a begin, it is straightforward for any tape that starts off blank. The input tape starts with the input so naturally it needs its own case
    """
    if tape.read() is Sym.ONE:
        tape.write(Sym.BEGIN)
        moveRightEnd(tape)
        tape.write(Sym.ONE)
        rewind(tape)
    else: 
        tape.write(Sym.BEGIN)
        tape.move_right()

def increment(tape):
    """
    Adds a ONE at the end of the tape.
    """
    moveRightEnd(tape)
    tape.write(Sym.ONE)
    rewind(tape)

def turnIntoCopy(source, out):
    """
    Essentially resets out by adding a new BEGIN at the right-end of the tape, then adds ONE to out for as many non-blank symbols there are in source. 
    """
    moveRightEnd(out)
    addBegin(out)
    while source.read() is not Sym.BLANK:
        out.write(Sym.ONE)
        out.move_right()
        source.move_right()
    rewind(source)
    rewind(out)

def add(term, sum):
    """
    sum is incremented by one for each element in term, effectively adding term to sum.
    """
    while term.read() is not Sym.BLANK:
        increment(sum)
        term.move_right()
    rewind(term)
    rewind(sum)

def cube(register, dummy, registerSquared, registerCubed):
    turnIntoCopy(register, dummy)
    
    moveRightEnd(registerSquared)
    addBegin(registerSquared)
    
    while dummy.read() is not Sym.BLANK: #loop squares register and stores it into registerSquared.
        add(register, registerSquared)
        dummy.move_right()
    rewind(dummy)

    moveRightEnd(registerCubed)
    addBegin(registerCubed)
    
    while dummy.read() is not Sym.BLANK: #this loop now multiplies register with registerSquared, effectively cubing it.
        add(registerSquared, registerCubed) 
        dummy.move_right()
    rewind(register)
    rewind(registerSquared)
    rewind(dummy)
    rewind(registerCubed)


def compare(cubed, input, ineqReg):
    """
    Check if cubed is smaller, i.e. cubed has passed a BLANK before input has.
    If they are the same, then when the while-loop stops, cubed will not have passed a BLANK.
    If cubed is greater, then cubed will not read a BLANK when input does, and we reject.

    Further, we store the outcome in ineqReg.
    """
    while input.read() is not Sym.BLANK: #go to the first BLANK of both input and cubed.
        input.move_right()
        cubed.move_right()
    
    moveRightEnd(ineqReg)

    if cubed.read() is Sym.BLANK: #If cubed is also at a blank, we want to check whether or not it's the first, if it is the first one, they are equal, otherwise cubed started reading blanks before input, and is therefore smaller.  
        cubed.move_left()
        if cubed.read() is not Sym.BLANK:
            ineqReg.write(Sym.EQUAL)
           
            rewind(input) 
            rewind(cubed) 
            rewind(ineqReg)
            return None

        elif cubed.read() is Sym.BLANK:
            ineqReg.write(Sym.LESS)

            rewind(input) 
            rewind(cubed) 
            rewind(ineqReg)
            return None

    elif cubed.read() is not Sym.BLANK:
        ineqReg.write(Sym.GREATER)
        rewind(input) 
        rewind(cubed) 
        rewind(ineqReg)
        return None
    


def main() -> None:
    # Store optional tapes here

    input = Tape.from_arguments(sys.argv) #This is the input tape, we will add BEGIN at the start of it, but otherwise only read off it.
    addBegin(input)

    dummy = Tape() # Used for iterating over another tape tp. We often reset dummy by adding BEGIN to the end of dummy, and then caopy input into the right of this BEGIN. 
    addBegin(dummy)

    croot = Tape() #This register is what gets cubed, the idea is to increment it by one each time cubing yields a smaller tape than input, otherwise we halt (because it's the same) or reject (because it's too large)
    addBegin(croot)

    squared = Tape() # Used as a stand-in for cubing croot. We store croot in dummy, croot squared is then stored in squared. This is used similarly to dummy.
    addBegin(squared)

    cubed = Tape() #this is the register used to cubed register
    addBegin(cubed)

    ineqReg = Tape() #Will only store symbols like LESS, GREATER and EQUAL. So if ineqReg.read() is LESS, that means that the number of a's in cubed is less than the number of a's in input. 
    addBegin(ineqReg)
    
    increment(croot)

    compare(croot, input, ineqReg)   

    while True:
        moveRightEnd(ineqReg)
        ineqReg.move_left()
        if ineqReg.read() is Sym.EQUAL:
            input.accept()
        elif ineqReg.read() is Sym.LESS:
            increment(croot)
            cube(croot, dummy, squared, cubed)
            compare(cubed, input , ineqReg)
        elif ineqReg.read() is Sym.GREATER:
            input.reject()


if __name__ == "__main__":
    main()
