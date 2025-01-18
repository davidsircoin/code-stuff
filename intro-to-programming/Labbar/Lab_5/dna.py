class DnaSeq:


    def __init__(self, accession, seq):
        if accession and seq != None or '':
            self.accession = accession
            self.seq = seq
        else:
            raise ValueError

    def __len__(self):
        return(len(self.seq))
        
    def __str__(self):
        return("<DnaSeq accession=" + '\'' + self.accession + '\'>')
    
def read_dna(filename):
    '''
    Returns a list of DNA sequences that have been given the name after what comes after >, and the sequence code by the respective string.
    '''
    accessions = []
    sequences = []
    with open(filename, 'r') as file:
        for line in file:
            if line[0]== '>':
                line = line.rstrip('\n')
                accessions.append(line[1:])
            elif line != '\n':
                sequences.append(line)
            pass
    dna_records = [DnaSeq(accessions[i], sequences[i]) for i in range(len(accessions))]
    
    return(dna_records)    

def check_exact_overlap(seq1, seq2, min_length = 10):
    '''
    Returns the length of the maximun overlap between the suffi, and respectively the prefix of to different sequences overlap with eachother.
    ''' 
    seq1 = seq1.seq
    seq2 = seq2.seq
    smallest_seq = min(seq1, seq2)
    overlapping_seq = []
    try:
        while min_length < len(smallest_seq):
            if seq1[-min_length:] ==  seq2[0:min_length]:
                overlapping_seq = list(seq2[0:min_length])
                min_length += 1

            else:
                min_length += 1

    except IndexError:
        return 0

    return len(overlapping_seq)


def overlaps(  ):
    pass


#
# Testing code. You should not change any code below here. To run the tests
# uncomment the last line in the file.
#
def test_class_DnaSeq():
    s1 = DnaSeq('s1', 'ACGT')
    s2 = DnaSeq('s2', 'ATGTTTGTTTTTCTTGTTTTATTGCCACTAGTCTCTAGTCAGTGTGTTAATCTTACAACCAGAACTCAAT')
    assert len(s1) == 4, 'Your length method (__len__) is not correct.'
    assert len(s2) == 70, 'Your length method (__len__) is not correct.'

    assert str(s1) == "<DnaSeq accession='s1'>", 'The __str__ method is not following the specification.'
    assert str(s2) == "<DnaSeq accession='s2'>", 'The __str__ method is not following the specification.'


    # The rest of this function is verifying that we are indeed raising an exception.
    status = 0
    try:
        s3 = DnaSeq('', 'ACGT')
    except ValueError:
        status += 1
    try:
        s3 = DnaSeq('s3', None)
    except ValueError:
        status += 1

    try:
        s3 = DnaSeq(None, '')
    except ValueError:
        status += 1
    
    if status != 3:
        raise Exception('class DnaSeq does not raise a ValueError '
                        'exception with initialised with empty '
                        'accession and sequence.')
        
    print('DnaSeq passed')


def test_reading():
    dna1 = read_dna('ex1.fa')
    assert len(dna1) == 6, 'The file "ex1.fa" has exactly 6 sequences, but your code does not return that.'
    assert list(map(lambda x: x.accession, dna1)) == [f's{i}' for i in range(6)], 'The accessions are not read correctly'

    dna2 = read_dna('ex2.fa')
    assert len(dna2) == 6, 'The file "ex2.fa" has exactly 6 sequences, but your code does not return that.'

    covid = read_dna('sars_cov_2.fa')
    assert len(covid[0].seq) == 29903, 'The length of the genome in "sars_cov_2.fa" is 29903, but your code does not return that.'

    print('read_dna passed')


def test_overlap():
   s0 = DnaSeq('s0', 'AAACCC')
   s1 = DnaSeq('s1', 'CCCGGG')
   s2 = DnaSeq('s2', 'TTATCC')
   s3 = DnaSeq('s3', 'CCAGGG')
   s4 = DnaSeq('s4', 'GGGGGGGGAAAGGGGG')
   s5 = DnaSeq('s5', 'AAATTTTTTTTTTTTTTTTTAT') #TATT = TTAT 


   data1 = [s0, s1, s2, s3]
   assert check_exact_overlap(s0, s1, 2) == 3
   assert check_exact_overlap(s0, s1) == 0
   assert check_exact_overlap(s0, s3, 2) == 2
   assert check_exact_overlap(s1, s2, 2) == 0
   assert check_exact_overlap(s2, s1, 2) == 2
   assert check_exact_overlap(s2, s3, 2) == 2
   assert check_exact_overlap(s4, s5, 1) == 0, 'Do not allow "internal" substrings to overlap. s4 and s5 does not have an overlap.'
   assert check_exact_overlap(s4, s5, 2) == 0
   assert check_exact_overlap(s4, s5, 3) == 0
   assert check_exact_overlap(s5, s2, 1) == 4

   res0 = overlaps(data1, lambda s1, s2: check_exact_overlap(s1, s2, 2))
   assert len(res0) == 2, 'You get the wrong number of overlaps'
   assert res0 == {'s0': {'s1': 3, 's3': 2}, 's2': {'s1': 2, 's3': 2}}

   dna_data = read_dna('ex1.fa')
   res1 = overlaps(dna_data, check_exact_overlap)
   assert len(res1) == 5
   for left, right in [('s0', 's1'), ('s1', 's2'), ('s2', 's3'), ('s3', 's4'),
                       ('s4', 's5')]:
       assert res1[left][right], f'Missing overlap of {left} and {right} (in that order)'
   print('overlap code passed')


def test_all():
    test_class_DnaSeq()
    test_reading()
    test_overlap()
    print('Yay, all good')


# Uncomment this to test everything:
test_all()
