import matplotlib.pyplot as plt
import numpy as np


def data_to_dictionary(generated_lines):
    '''Returns a dictionary which holds the batch number as keys, each with a list as value, containing 3-tuples of float-values.'''

    data = {} 
    for line in generated_lines:
        if line == '\n':
            print("Warning: wrong input format for entry:", '\\n') #This solves the issue of parsing an empty row. Chose to have it outside of try-except as it's an error that can be caught right away.
            continue
        
        try:
            batch, variable_1, variable_2, measure_value = line.split(',') #Just a nicer parsing in my humble opinion.
            if batch not in data:
                data[batch] = []

            data[batch].append((float(variable_1), float(variable_2), float(measure_value))) #The '+=' can be pretty ambiguous in how one wants it to be used. Meanwhile, with append you don't have to guess at all.

        except ValueError: #This solves the issue of lines not being formatted correctly(i.e. not having float-convertible entries).
            print("Warning: wrong input format for entry:", batch, variable_1, variable_2, measure_value)
            continue

    return data


def plot_data(data, pdf_name):
    '''Returns a .pdf file with the same name as the samplefile with the points scattered as intended. More details below.'''
    
    #Here, we are setting up the graph, figure is the object we call on when i.e. giving the graph a title, or saving it as a file.
    figure, axes = plt.subplots() #Meanwhile, axes is what we're plotting on, If we wanted to change the aspect ratios of the axis, or name the points on the graph, then we need to call on axes.
    figure.suptitle('Sample:')

    axes.set_aspect('equal') 

    #I found a nice parametric representation of the unit circle via this site:
    #https://www.pythonpool.com/matplotlib-circle/ 

    theta = np.linspace(0, 2*np.pi, 150) #As I've gathered, linspace(a,b,c) creates an array of points that are evenly spaced. So 150 points between 0, 2pi will look very much like a smooth curve.

    a = np.cos( theta )
    b = np.sin( theta )
    
    axes.plot(a,b)

    #Here we are parsing through the dictionary, creating three lists, one for x-coordinated, one for y, and one for the names we want to give each point.
    for batch in data:
        points = data[batch]
        x = [p[0] for p in points]
        y = [p[1] for p in points]
        names = [p[2] for p in points]
        axes.scatter(x, y)
        
        #Here we are annotating each point with their name corresponding value found in 'names'.
        for i in range(len(data[batch])):
            axes.annotate(names[i], (x[i], y[i]), fontsize = 6)

    figure.savefig(pdf_name, format = 'pdf')


def compute_averages(data, batch):
    '''Return the average measure value of points in a given batch that satisfy lying inside a unit circle.'''
    sample_amount = 0
    measure_total = 0
    try:
        for (variable_1, variable_2, measure_value) in data[batch]:
            if variable_1**2 + variable_2**2 <= 1:
                measure_total += measure_value
                sample_amount += 1 

        average = measure_total/sample_amount
        return average

    except ZeroDivisionError: #Avoids computing the average when there are no points. 
        return None


def print_batch_average(data, batch):
    '''
    Prints all the averages for any given batch.
    '''
    average = compute_averages(data, batch)
    if average != None:
        print(batch, "\t", average)


def main():
    '''
    This is the main body of the program.
    '''
    while True: #Loop is just here for when the input is invalid. 
        filename = input('Which csv file should be analyzed?')
        pdf_name = filename.split('.')[0] + '.pdf' #My solution to naming the graph the same as filename but with .pdf instead of .csv

        try:
            with open(filename, 'r') as sample_file: 
                data = data_to_dictionary(sample_file)
            
            print("Batch\t Average")
            for batch in sorted(data): #Here is where we're sorting the data so that the batches appear in the correct order.
                print_batch_average(data, batch) 
            
            plot_data(data, pdf_name)
            print('A plot of the data can be found in ', pdf_name)
            break

        except FileNotFoundError or FileExistsError: #If filename is unvalid you get the following nice comment:
            print('Oops! The file entered was not found, try another one uWu') 
            pass

if __name__ == '__main__':
    main()
