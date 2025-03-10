import unittest

import trains as t

stations = [['A', '0.001'], ['B', '0.03'], ['C', '0.2'],\
        ['D', '0.001'], ['X', '0.1'], ['Y', '0.1'], ['Z', '0.1']]

connections = \
            [['Y', 'X', 'green', 'N'], ['B', 'C', 'blue', 'S'], \
            ['B', 'A', 'blue', 'N'], ['C', 'B', 'blue', 'N'], \
            ['X', 'Y', 'green', 'S'], ['Y', 'C', 'green', 'S'], \
            ['C', 'Z', 'green', 'S'], ['D', 'C', 'blue', 'N'], \
            ['C', 'Y', 'green', 'N'], ['Z', 'C', 'green', 'N'], \
            ['C', 'D', 'blue', 'S'], ['A', 'B', 'blue', 'S'], \
            ['A', 'X', 'brown', 'N'], ['X', 'A', 'brown', 'S']]

t.create_stations(stations)
t.setup_connections(connections)

A = t.Station.dict['A']
B = t.Station.dict['B']
C = t.Station.dict['C']
D = t.Station.dict['D']
X = t.Station.dict['X']


log_from_A = {t.Station.dict['A']: 0, t.Station.dict['B']: 1, \
            t.Station.dict['C']: 2, t.Station.dict['Z']: 3, \
            t.Station.dict['Y']: 2, t.Station.dict['X']: 1, \
            t.Station.dict['D']: 3}

class TestTrains(unittest.TestCase):
    
    def test_parsing(self):
        self.assertEqual(t.parse_through_file('s3.txt', 1), stations)
        
        self.assertEqual(t.parse_through_file('c3.txt', 3), connections)

    def test_create_stations(self):
        self.assertEqual(A.name, 'A')
        self.assertEqual(A.delay, 0.001)
        self.assertEqual(A.south, {'blue' : B})
        self.assertEqual(A.north, {'brown' : X})
        self.assertEqual(B.name, 'B')
        self.assertEqual(B.north, {'blue' : A})
        self.assertEqual(B.south, {'blue' : C})
    
    def test_delay(self):
        for i in t.Station.dict.values():
            self.assertEqual(i.delay <= 1, True)
            self.assertEqual(i.delay >= 0, True)

    def test_train_attributes(self):
        train = t.Train(1)
        train.add_info(D)
        self.assertEqual(train.name, 1)
        self.assertEqual(train.current_station, D)
        self.assertEqual(train.direction, 'North')
        self.assertEqual(train.line, 'blue')
        self.assertEqual(train.next_station, C)

    def test_run_simulation(self):
        train = t.Train(1)
        train.add_info(D)
        train.simulate() #Assuming it didn't get delayed
        self.assertEqual(train.current_station, C)
        self.assertEqual(train.direction, 'North')
        self.assertEqual(train.line, 'blue')
        self.assertEqual(train.next_station, B)

    def test_reach_from(self):
        self.assertEqual(A.reach_from(), log_from_A)

unittest.main()