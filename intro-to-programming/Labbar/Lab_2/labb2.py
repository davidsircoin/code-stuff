p = [2,0,1]
q = [-2,1,0,0,1]
p0 = [2,0,1,0]
q0 = [0,0,0]

def drop_zeroes(p_list):
    '''
    Return polynomial, rid of all zeroes after the last non-digit entry.
    '''
    zero_free_list = list(p_list)
    while zero_free_list != [] and zero_free_list[-1] == 0:
        zero_free_list.pop(-1)
    return(zero_free_list)

def eq_poly(p_list,q_list):
    '''
    Checks if two polynomials are the same. 
    '''
    new_p_list = drop_zeroes(p_list)
    new_q_list = drop_zeroes(q_list)
    return(new_p_list == new_q_list)

def poly_to_string(p_list):
    '''
    Return a string with a nice readable version of the polynomial given in p_list.
    '''
    terms = []
    degree = 0
    new_p_list = drop_zeroes(p_list)
    if new_p_list == []: 
        return '0'


    for coeff in new_p_list:
        if degree == 0:
            if coeff != 0:
                terms.append(str(coeff))
            else:
                pass

        elif degree == 1:
            if coeff not in [-1,1,0]:
                terms.append(str(coeff) + 'x')
            elif coeff == 1:
                terms.append('x')
            elif coeff == -1:
                terms.append('-x')
            else:
                pass
        else: 
            if coeff not in [-1,1,0]:
               terms.append(str(coeff) + 'x^' + str(degree))
            elif coeff == 1:
                terms.append('x^' + str(degree))
            elif coeff == -1:
                terms.append('-x^' + str(degree))
            else:
                pass
        degree += 1
    final_string = ' + '.join(terms) # The string ' + ' is used as "glue" between the elements in the string
    return final_string

def eval_poly(p_list, point):
    '''
    Return value of polynomial at a given point.
    '''
    terms = []
    degree = 0
    new_p_list = drop_zeroes(p_list)

    if new_p_list == 0:
        return '0'
    for coeff in new_p_list:
        if degree == 0:
            if coeff != 0:
                terms.append(coeff)
            else:
                pass

        elif degree == 1:
            if coeff not in [-1,1,0]:
                terms.append(coeff*(point))
            elif coeff == 1:
                terms.append(point)
            elif coeff == -1:
                terms.append(-point)
            else:
                pass
        else: 
            if coeff not in [-1,1,0]:
               terms.append(coeff*(point**degree))
            elif coeff == 1:
                terms.append(point**degree)
            elif coeff == -1:
                terms.append(-point**degree)
            else:
                pass
        degree += 1
    return sum(terms)

def neg_poly(p_list):
    '''
    Return additive inverse of a given polynomial.
    '''
    negp_list = []
    for coeff in p_list:
        if coeff != 0:
            negp_list.append(-coeff)
        else:
            negp_list.append(coeff)
    return(negp_list)

def add_poly(p_list, q_list):
    '''
    Return sum of two polynomial.
    '''
    new_p_list, new_q_list = equate_poly(p_list, q_list)
    sump_list = []

    i = 0
    while i < len(new_p_list):
        sump_list.append(new_p_list[i] + new_q_list[i])
        i += 1
    drop_zeroes(sump_list)
    return sump_list

def equate_poly(p_list, q_list):
    '''
    Returns polynomials as they are, but with added zeroes to the polynomial with the least number of entries, such that they both have the same amount of elements. Important for how the operator functions work.
    '''
    new_p_list = list(p_list)
    new_q_list = list(q_list)
    while len(new_p_list) > len(new_q_list):
        new_q_list.append(0)
    while len(new_p_list) < len(new_q_list):
        new_p_list.append(0)
    return new_p_list, new_q_list

def sub_poly(p_list, q_list):
    '''
    Returns the difference of two polynomials.
    '''
    q_list = neg_poly(q_list)
    equate_poly(p_list, q_list)
    sump_list = add_poly(p_list, q_list)
    return sump_list

