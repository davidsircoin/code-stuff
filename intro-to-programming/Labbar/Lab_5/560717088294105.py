class DnaSeq:
    """
    A class to represent DNA - strings 
    
    accession : str 
        name of seq 
        
    seq : str 
        DNA-string 
    """
    def __init__(self, accession, seq):
        self.accession = accession
        self.seq = seq
        if not self.accession or not self.seq: #If any parameter is None or "" a ValueError is raised 
            raise ValueError

    def __len__(self):
        """
        Returns the length of a DNA-sequence 
        """
        return len(self.seq)
    

    def __str__(self):
        """
        Returns the a string with some text and self.accession 
        """
        return ("<DnaSeq accession='" + self.accession + "'>") 

def read_dna(filename):
    """
    Takes a filenmae and returns a list with DnaSeq objects
    """
    dna_list = [] #List of data in file without \n and <
    DnaSeq_object_list = []
    with open(filename, "r") as h:
        for line in h: 
            if line == "\n": #Ignores empty lines 
                continue
            else:
                a = line.replace(">", "").replace("\n", "").replace(" ", "") #"Removes >, \n and spaces in sequences of DNA
                dna_list.append(a)
        for n in range(0, len(dna_list), 2):
            DnaSeq_object = DnaSeq(dna_list[n], dna_list[n+1]) #dna_list[n] is the accession, dna_list[n+1] is the seq 
            DnaSeq_object_list.append(DnaSeq_object)
    return DnaSeq_object_list

def check_exact_overlap(a, b, min_len = 10):
    """
    Takes two DNA-objects and checks if the DNA seqs of objects a and b are equal. 
    aseq and bseq are used to the original is never altered 
    """
    if len (a) > len (b):
        aseq = a.seq[-len (b):] #Makes len (aseq) = len (bseq)
        bseq = b.seq
            
    elif len (a) < len (b): 
        aseq = a.seq
        bseq = b.seq[:len (a)] #Makes len (bseq) = len (aseq)
        
    elif len (a) == len (b):
        aseq = a.seq
        bseq = b.seq
        
    while aseq != bseq: #Keeps "removing" bases at the start of aseq and at the end of bseq 
    #until they are equal
        aseq = aseq[1:]
        bseq = bseq[:-1]
        
    if len (aseq) >= min_len:
        return len(aseq) #Returns the length of aseq when the sequence is identical to bseq. (return len(bseq) gives the same result) 
    else:
        return 0

def overlaps(lst, overlap_function):
    """
    Takes a list DnaSeq - object and a function that detects overlaps and returns 
    a dict of dicts where one can see how much the objects in the list overlaps eachother. 
    """
    overlap_dict = {}
    for objct in lst:
        for objct2 in lst:
            if overlap_function(objct, objct2) != 0 and objct.accession != objct2.accession: #Only adds to dictionary if overlap exceeds 0 
            #and if the accessions are not the same 
                if objct.accession not in overlap_dict: 
                    overlap_dict[objct.accession] = {}
                overlap_dict[objct.accession][objct2.accession] = overlap_function(objct, objct2)
    return overlap_dict 

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
   s5 = DnaSeq('s5', 'AAATTTTTTTTTTTTTTTTTAT')

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