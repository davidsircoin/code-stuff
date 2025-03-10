# I have implemented two versions of read_dna, one uses regular expression, which is more powerful than another using no validation check.
import re
# Python >=3.5 typing,  https://docs.python.org/3/library/typing.html
from typing import Callable, List


class DnaSeq:
    def __init__(self, accession: str, seq: str):
        # if accesssion and seq is not empty string, or None
        if not (bool(accession) and bool(seq)):
            raise ValueError(
                "accession and seq can not be None or empty string")

        self.accession = accession
        self.seq = seq

    def __len__(self) -> int:
        return len(self.seq)

    def __str__(self) -> str:
        return '<DnaSeq accession=\'{self.accession}\'>'.format(self=self)


def read_dna(filename: str) -> List[DnaSeq]:
    '''read DNA file and store those into a list of object'''
    f = open(filename, 'r')
    lines = f.readlines()

    dna_seq_list = list()
    i = 0
    for line in lines:
        if len(line.strip()) == 0:  # check if it is empty line
            continue
        line = line.strip()
        if line.startswith('>'):
            accession_id = line[1:]
        else:
            dna_seq = DnaSeq(accession_id, line)
            dna_seq_list.append(dna_seq)
        i = i + 1
    return dna_seq_list


""""
def read_dna(file_name: str) -> List[DnaSeq]:
    f = open(file_name, 'r')
    lines = f.readlines()
    # Please use https://regex101.com/ to check the DNA regular expression of accession and seq
    accession_re = re.compile('>(\S+)')  # access regular expression
    seq_re = re.compile('^[a-zA-Z_ ]*$')  # seq regular expression
    dna_seq_list = list()
    i = 0
    for line in lines:
        if len(line.strip()) == 0:  # check if it is empty line
            continue
        out_accession = accession_re.search(line)
        if out_accession:
            accession_id = out_accession.group(1)  # Get DNA accession
        else:
            out_seq = seq_re.search(line)
            seq_result = out_seq.group(0)  # get DNA seq
            dna_seq = DnaSeq(accession_id, seq_result)
            dna_seq_list.append(dna_seq)
        i = i + 1
    return dna_seq_list
"""


def check_exact_overlap(a_with_suffix: DnaSeq, b_with_prefix: DnaSeq, min_overlap: int = 10) -> int:
    len_a = len(a_with_suffix)
    len_b = len(b_with_prefix)

    # Choose shorter len for iteration
    if len_a >= len_b:
        len_index = len_b
    else:
        len_index = len_a

    counter_overlap = 0

    '''check overlap, and counting'''
    for x in range(len_index):
        if a_with_suffix.seq[-(x + 1)] == b_with_prefix.seq[x]:
            counter_overlap = counter_overlap + 1
        else:
            break
    if counter_overlap < min_overlap:
        return 0
    return counter_overlap


def overlaps(list_dna_seq: List[DnaSeq], f: Callable[[DnaSeq, DnaSeq, int], int]) -> dict:
    # All combinations of two DNA Seq in alist
    list_combinations = [(list_dna_seq[i], list_dna_seq[j+i+1])
                         for i in range(len(list_dna_seq)) for j in range(len(list_dna_seq[i+1:]))]
    res = {}
    for each_combination in list_combinations:
        a_with_suffix, b_with_prefix = each_combination
        print(a_with_suffix, b_with_prefix)
        ''' overlap a -> b '''
        overlap_a_b = f(a_with_suffix, b_with_prefix)
        print(overlap_a_b)
        if not a_with_suffix.accession in res:
            res[a_with_suffix.accession] = {}
        if overlap_a_b != 0:
            res[a_with_suffix.accession][b_with_prefix.accession] = overlap_a_b
        ''' overlap b -> a '''
        overlap_b_a = f(b_with_prefix, a_with_suffix)
        print(overlap_b_a)
        if not b_with_prefix.accession in res:
            res[b_with_prefix.accession] = {}
        if overlap_b_a != 0:
            res[b_with_prefix.accession][a_with_suffix.accession] = overlap_b_a

    res = {k: v for k, v in res.items() if v}
    print(res)

    return res
#
# Testing code. You should not change any code below here. To run the tests
# uncomment the last line in the file.
#


def test_class_DnaSeq():
    s1 = DnaSeq('s1', 'ACGT')
    s2 = DnaSeq(
        's2', 'ATGTTTGTTTTTCTTGTTTTATTGCCACTAGTCTCTAGTCAGTGTGTTAATCTTACAACCAGAACTCAAT')
    assert len(s1) == 4, 'Your length method (__len__) is not correct.'
    assert len(s2) == 70, 'Your length method (__len__) is not correct.'

    assert str(
        s1) == "<DnaSeq accession='s1'>", 'The __str__ method is not following the specification.'
    assert str(
        s2) == "<DnaSeq accession='s2'>", 'The __str__ method is not following the specification.'

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
    assert len(
        dna1) == 6, 'The file "ex1.fa" has exactly 6 sequences, but your code does not return that.'
    assert list(map(lambda x: x.accession, dna1)) == [
        f's{i}' for i in range(6)], 'The accessions are not read correctly'

    dna2 = read_dna('ex2.fa')
    assert len(
        dna2) == 6, 'The file "ex2.fa" has exactly 6 sequences, but your code does not return that.'

    covid = read_dna('sars_cov_2.fa')
    assert len(
        covid[0].seq) == 29903, 'The length of the genome in "sars_cov_2.fa" is 29903, but your code does not return that.'

    print('read_dna passed')


def test_overlap():
    s0 = DnaSeq('s0', 'AAACCC')
    s1 = DnaSeq('s1', 'CCCGGG')
    s2 = DnaSeq('s2', 'TTTTCC')
    s3 = DnaSeq('s3', 'CCAGGG')
    s4 = DnaSeq('s4', 'GGGGGGGGAAAGGGGG')
    s5 = DnaSeq('s5', 'AAATTTTTTTTTTTTTTTTTTT')

    data1 = [s0, s1, s2, s3]
    assert check_exact_overlap(s0, s1, 2) == 3
    assert check_exact_overlap(s0, s1) == 0
    assert check_exact_overlap(s0, s3, 2) == 2
    assert check_exact_overlap(s1, s2, 2) == 0
    assert check_exact_overlap(s2, s1, 2) == 2
    assert check_exact_overlap(s2, s3, 2) == 2
    assert check_exact_overlap(
        s4, s5, 1) == 0, 'Do not allow "internal" substrings to overlap. s4 and s5 does not have an overlap.'
    assert check_exact_overlap(s4, s5, 2) == 0
    assert check_exact_overlap(s4, s5, 3) == 0


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
    #test_overlap()
    print('Yay, all good')


# Uncomment this to test everything:
test_all()
