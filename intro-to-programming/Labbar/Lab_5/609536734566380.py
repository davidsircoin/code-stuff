class DnaSeq:
    def __init__(self, accession, seq):
        # Initialise the object with the accession and sequence.
        # This is done by setting the attributes self.accession and self.seq
        # to the values of the parameters accession and seq.
        try:
            self.accession = accession
            self.seq = seq
        except ValueError:
            raise ValueError
        if self.accession == '' or self.accession is None:
            raise ValueError
        if self.seq == '' or self.seq is None:
            raise ValueError
        if self.seq.upper() != self.seq:
            raise ValueError

    def __len__(self):
        # Return the length of the sequence
        # If the sequence is empty, return 0
        return len(self.seq)

    def __str__(self):
        # Return a string representation of the object
        return f"<DnaSeq accession='{self.accession}'>"


def read_dna(filename):
    # Read a fasta file and return a list of DnaSeq objects.
    # The fasta file contains one or more sequences.
    # Each sequence starts with a line starting with '>'.
    dna_list = []
    with open(filename, 'r') as file:
        for line in file:
            if line.startswith('>'):
                accession = line[1:].strip()
            elif line.startswith('\n'):
                continue
            else:
                seq = line.strip()
                dna_list.append(DnaSeq(accession, seq))
    return dna_list


def check_exact_overlap(a, b, min_overlap=10):
    '''Return the length of the exact overlap between the sequences of a and b.
    If the overlap is less than min_overlap, return 0.

    Parameters: a and b are DnaSeq objects and min_overlap is an integer.

    Example: if a.seq is 'ACGTACGT' and b.seq is 'GTACGTAC' then the overlap is 6.

    Returns: an integer
    '''
    overlap = 0
    max_overlap = 0
    for index in range(min(len(a.seq), len(b.seq))):
        prefix = b.seq[:index + 1]
        suffix = a.seq[len(a.seq) - index - 1:]
        if prefix == suffix:
            overlap = len(prefix)
        else:
            if max_overlap < overlap:
                max_overlap = overlap
            overlap = 0
    if max_overlap >= min_overlap:
        return max_overlap
    else:
        return 0


def overlaps(list_of_dna, lambda_function=lambda s1, s2: check_exact_overlap(s1, s2, 2)):
    '''Return a dictionary of overlaps between the sequences in list_of_dna.
    The dictionary has the following structure:
    {accession1: {accession2: overlap, accession3: overlap, ...},


    Parameters: list_of_dna is a list of DnaSeq objects and lambda_function is a function that takes two DnaSeq objects and returns an integer.

    Returns: a dictionary
    '''
    dict_of_overlaps = {}
    for i in range(len(list_of_dna)):
        for j in range(len(list_of_dna)):
            if i == j:
                continue
            overlap = lambda_function(list_of_dna[i], list_of_dna[j])
            if overlap > 0:
                if list_of_dna[i].accession not in dict_of_overlaps:
                    dict_of_overlaps[list_of_dna[i].accession] = {}
                dict_of_overlaps[list_of_dna[i].accession][list_of_dna[j].accession] = overlap
    return dict_of_overlaps


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
    assert list(map(lambda x: x.accession, dna1)) == [f's{i}' for i in
                                                      range(6)], 'The accessions are not read correctly'

    dna2 = read_dna('ex2.fa')
    assert len(dna2) == 6, 'The file "ex2.fa" has exactly 6 sequences, but your code does not return that.'

    covid = read_dna('sars_cov_2.fa')
    assert len(covid[
                   0].seq) == 29903, 'The length of the genome in "sars_cov_2.fa" is 29903, but your code does not return that.'

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
    assert check_exact_overlap(s4, s5,
                               1) == 0, 'Do not allow "internal" substrings to overlap. s4 and s5 does not have an overlap.'
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


test_all()
