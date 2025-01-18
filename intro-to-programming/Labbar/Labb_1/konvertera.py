'''Man kan endast välja mellan F, C, K eller Q. Allt annat ger tillbaka samma funktion.'''
def menu_from(): 
    print('F. Fahrenheit')
    print('C. Celsius')
    print('K. Kelvin')
    print('Q. Avsluta')
    choose_from = input('Välj enheten du vill konvertera från:\n')
    return choose_from

'''Kolla dokumentation för menu_from.'''
def menu_to():    #Värt o nämna att om menu_to == menu_from så måste du ge ett nytt input.
    print('F. Fahrenheit')
    print('C. Celsius')
    print('K. Kelvin')
    print('Q. Avsluta')
    choose_to = input('Välj enheten du vill konvertera till:\n')
    return choose_to

#Värt o nämna att varje to_** kan utföra 2 konverteringar.
'''Här sker själva konverteringen.'''
def to_kelvin(choose_from, answer):
    t_kelvin = None
    if choose_from == 'F':
        t_kelvin = (answer-32)*5/9 +273.15
    if choose_from == 'C':
        t_kelvin = answer + 273.15
    return to_kelvin

'''Här sker själva konverteringen.'''
def to_fahrenheit(choose_from, answer):
    t_fahrenheit = None
    if choose_from == 'C':
        t_fahrenheit = answer*(9/5)+32
    if choose_from == 'K':
        t_fahrenheit = (answer-273.15)*9/5 + 32
    return t_fahrenheit

'''Här sker själva konverteringen.'''
def to_celsius(choose_from, answer):
    t_celsius = None
    if choose_from == 'F':
        t_celsius = (answer-32)*5/9
    if choose_from == 'K':
        t_celsius = answer -273.15
    return t_celsius

#Här sker allt, det loopas tills att man skriver in Q som input för choose_from eller choose_to. 
while True:
    choose_from = menu_from()

    while choose_from not in ('F', 'C', 'K', 'Q'):
        choose_from = menu_from
    if choose_from in ('F', 'C', 'K'):
        choose_to = menu_to()

    if choose_from == 'Q':
        break

    while choose_to not in ('F', 'C', 'K','Q') or choose_to == choose_from:
        choose_to = menu_to()

    if choose_to == 'F':
        answer = float(input('Ange en temperatur:\n'))
        t_fahrenheit = to_fahrenheit(choose_from, answer)
        print("Fahrenheit:", t_fahrenheit, '\n')

    if choose_to == 'C':
        answer = float(input('Ange en temperatur:\n'))
        t_celsius = to_celsius(choose_from, answer)
        print("Celsius:", t_celsius, '\n')

    if choose_to == 'K':
        answer = float(input('Ange en temperatur:\n'))
        t_kelvin = to_kelvin(choose_from, answer)
        print("Kelvin:", t_kelvin, '\n')

    if choose_to == 'Q':
        break