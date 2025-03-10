import random as rd
    
class Station:
    dict = {} #This dictionary holds all the data for the
              #station-objects as values to their corresponding
              #name as their key.

    def __init__(self, name = None, delay= None):
        self.name = name
        self.delay = float(delay)
        self.south = {}
        self.north = {}

    def add_connections(self, destination, line, pole):
        '''
        Updates a stations south/north dictionary with a key:value pair
        in the form of {line : station}. 
        '''
        if pole == 'S':
            if line not in self.south.keys():
                self.south[line] = destination
            else:
                print(f'Station {self.name} already has a station \
assigned at this line in this direction, {self.south[line].name}.')
                raise Exception

        elif pole == 'N':
            if line not in self.north.keys():
                self.north[line] = destination
            else:
                print(f'Station {self.name} already has a station \
assigned at this line in this direction, {self.north[line].name}.') 
                raise Exception
            
        else:
            print('The direction needs to be either N or S')
            raise 
    

    def reach_from(self, memory = {}, n = 0):
        '''
        Depth-First-Search of all stations from start which
        returns a dictionary(memory) that contains all stations as
        keys and their number of steps away from start as values.
        '''
        if self not in memory or n < memory[self]:
            memory[self] = n
            reach = set(self.south.values()).union(set(self.north.values()))

            for neighbour in reach: 
                neighbour.reach_from(memory, n+1)

        return memory

class Train:

    list = [] #No need for a dictionary like in Stations.dict,
              #since the names of the trains correspond to their
              #place in the list.

    def __init__(self, name):
        self.name = name
        self.current_station = None
        self.direction = None
        self.line = None
        self.next_station = None
        #This is what tells the train if it's delayed or not
        self.delay_status = False 

    
    def __str__(self):
        '''
        Looks complicated, but this just what prints the train info.
        '''
        if self.delay_status == False:
            return(f'Train {self.name} on {self.line.upper()} line is at station \
{self.current_station.name} heading in {self.direction} direction.')
        
        return(f'Train {self.name} on {self.line.upper()} line is at station \
{self.current_station.name} heading in {self.direction} direction. (DELAY)')

    def add_info(self, rd_station):
        '''
        Takes a station chosen at random as an input. Sets the train's 
        line and next_station attributes according to the station chosen.
        '''
        #First we create a pool of options for the lines we are able
        #to choose from
        lines_available = set(rd_station.south.keys()).union(set(rd_station.north.keys()))
        self.current_station = rd_station
        rd_line = rd.choice(list(lines_available))
        self.direction = rd.choice(['South', 'North'])

        if self.direction == 'South':
            try:
                self.next_station = rd_station.south[rd_line]
            except KeyError:
                self.direction = 'North'
                self.next_station = rd_station.north[rd_line]

        elif self.direction == 'North':
            try: 
                self.next_station = rd_station.north[rd_line]
            except KeyError:
                self.direction = 'South'
                self.next_station = rd_station.south[rd_line]

        self.line = rd_line

    def simulate(self):
        '''
        Makes the trains go cho-cho. When called upon, 
        the train updates its current_station attribute
        to become its next_station. Then we find the next-next_station
        in the direction[line] value of our next(now current) station.
        '''
        line = self.line
        self.delay_status = False
        destination = self.next_station
        self.current_station = destination
        if self.direction == 'South':
            try:
                self.next_station = destination.south[line]
            except KeyError: #We only get a KeyError here if the 
                             #station is an end-station, which 
                             #always leads to the train 
                             #switching direction.
                self.direction = 'North'
                self.next_station = destination.north[line]
                pass

        elif self.direction == 'North':
            try: 
                self.next_station = destination.north[line]
            except KeyError:
                self.direction = 'South'
                self.next_station = destination.south[line]
                pass

def parse_through_file(filename, n):
    '''
    This function parses both the stations file and the connections
    file, hence why we take n as an input which makes sure the files 
    have the correct amount of "objects".
    '''
    try:
        filein = open_file(filename)
    except:
        print(f'Sorry, the file {filename} either does not exist or \
cannot be found.')
              
    list_out = []

    for row in filein:
        if row.count(',') != n:
            print(f'{filename} is not formatted properly')
            raise Exception
        
        list_out.append(row.split(','))

    return list_out


def open_file(filename):
    '''
    Opens the file and removes the end of line \\n 
    '''
    list_of_lines = []

    with open(filename, 'r') as file_in:
        for line in file_in:
            if line == '\n':
                continue

            list_of_lines.append(line.rstrip('\n'))

    return list_of_lines


def check_stations_and_connections(slist, clist):
    '''
    This acts as a check for whether or not the files opened are actually
    compatible. This will return False if and only if one station that 
    appears in one file does not appear in the other.
    '''
    set1 = set(i[0] for i in slist)
    set2 = set([i[0] for i in clist])
    for i in clist:
        if ' ' in i[1] or ' ' in i[2] or ' ' in i[3][0]:
            raise Exception
        else:
            set2.update(i[1])
    if set1 != set2:
        return False

def create_stations(slist):
    '''
    Here is where the station objects first get created, if the stations
    file isn't formatted correctly, it will get caught here. I.e. if: 
    delay value isn't a number;
    delay value not between 0 and 1.
    '''
    for item in slist:
        name = item[0]

        try:
            delay = float(item[1])

        except:
            print(f'Invalid format on delay value. (Actual: {item[1]})')

        if delay <= float(1) and delay >= float(0):
            station = Station(name, delay)
            Station.dict[station.name] = station

        else: 
            print(f'Station {name} has delay not in [0,1]. (Actual: {item[1]})')
            raise ValueError


def setup_connections(clist):
    '''
    When the stations are created and have their name and delay
    assigned, this function gets called on to assign each station
    with its corresponding connections.
    '''
    for i in clist:
        station = Station.dict[i[0]]
        destination = Station.dict[i[1]]
        line = i[2]
        pole = i[3]
        
        try:
            station.add_connections(destination, line, pole)

        except:
            raise Exception


def create_trains(n):
    '''
    Here we are creating the amount of train objects the user selected,
    as well as appending them to the Train.list .
    '''
    for i in range(n):
        i = i+1
        train = Train(i)
        rd_station = rd.choice(list(Station.dict.values()))

        train.add_info(rd_station)
        Train.list.append(train)
        
def run_simulation(train, station, destination):
    '''
    Tests the delay chance on each train. Only the ones that pass 
    get to advance. Otherwise their delay gets set to True, and 
    they will stay idle.
    '''
    delay_chance = float(station.delay)

    if rd.random() > delay_chance:
        train.delay_status = False
        train.simulate_train(destination)

    else:
        train.delay_status = True

def train_info():
    n = len(Train.list)
    while True:
        try: 
            m = int(input(f'Which train? [1-{n}]\n'))

        except ValueError:
            print('Be kind and select a number!')
            break

        try:
            1 / m #In case m == 0 (which would print info of train 'n')
            print(Train.list[m-1])
            break

        except:
            print(f'Please enter a number between 1 and {n}')
            pass


def route_info_menu():
    '''
    User-interaction for when route-info is chosen.
    '''
    while True:
        map = list(Station.dict.keys())
        print(map)
        start = input('Select a start station:\n')

        if start in map:
            start = Station.dict[start]
            break

        print('Please enter a station from the given menu!')

    while True:
        goal = input('Select an end station:\n')

        if goal in map:
            goal = Station.dict[goal]
            break

        print('Please enter a station from the given menu!')

    while True:
        try:
            steps = int(input('Select timesteps:\n'))
            if steps > 0:
                break

            print('I wasn\'t aware time travel was a thing already.')   

        except ValueError:
            print(f'I\'m going to need a positive whole number')

    return start, goal, steps


def goal_reachable_check(search_log, start, goal, steps):
    '''
    Checks if the goal is inside the memory from the search
    within the number of steps entered by the user.
    '''
    if goal in search_log.keys() and search_log[goal] <= steps:
        print(f'Station {goal.name} is reachable from station \
{start.name} within {steps} timesteps')

    elif goal not in search_log.keys():
        print(f'Actually, station {goal.name} is NOT reachable \
from station {start.name} within any timesteps')
            
    else:
        print(f'Station {goal.name} is NOT reachable \
from station {start.name} within {steps} timesteps')


def choose_menu():
    while True:
        print('Continue simulation[1], train info [2], route info [3] exit[q].')
        answer = input('Select an option\n')

        if answer not in ['1', '2', '3', 'q', 'Q']:
            print('Please select a valid option.')
            continue

        if answer == '1':
            for train in Train.list:
                delay_chance =  train.current_station.delay

                if rd.random() > delay_chance:
                    try:
                        train.simulate()
                    except KeyError:
                        print('Sorry, there seems to be something wrong with the \
connections file')
                        return True
                else:
                    train.delay_status = True

            continue

        if answer == '2':
            train_info()
            continue

        if answer == '3':
            start, goal, steps = route_info_menu()
            goal_reachable_check(start.reach_from(), start, goal, steps)
            continue

        if answer == 'q' or 'Q':
            print('Thank you and goodbye!')
            break

        
def main():
    while True:
        while True:

            filename1 = input('Enter name of stations file:\n')
            try:
                slist = parse_through_file(filename1, 1)
                break
            except:
                pass

        while True:

            filename2 = input('Enter name of connections file:\n')
            try:
                clist = parse_through_file(filename2, 3)
                break
            except:
                pass
        try:
            if check_stations_and_connections(slist, clist) == False:
                print(f'\
Sorry, {filename1} and {filename2} don\'t share the same stations.')
                continue
        except Exception:
            print(f'Sorry, the connections file seems to be of wrong format.')
            continue
        except: 
            pass

        try:
            create_stations(slist)
        except ValueError:
            continue

        try:
            setup_connections(clist)
        except Exception:
            continue

        while True:
            try:
                n = int(input('Enter amount of trains to simulate:\n'))

                if n > 0:
                    break

                else:
                    print("n has to be in \mathbb'{'Z}^+")
            except ValueError:
                print('Hhmm... try writing a number!')
                pass
        try:
            create_trains(n)
        except IndexError:
            print(f'The connections don\'t seem to be formatted correctly.')
            continue
        if choose_menu() == True:
            continue
        
        break

if __name__ == '__main__':
    main()




