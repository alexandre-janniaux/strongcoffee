#-*- encoding: utf-8 -*-
"""
    Generate prime circuit
"""

import sys
import random

def print_bits(number, nb_bits):
    """
        Print enought bit for number repr
    """
    value = bin(number)[2:]
    if len(value) < nb_bits:
        value = "0"*(nb_bits-len(value)) + value
    return value

def gen_circuit(nb_entries, circuit_size):
    """
        Generate a random
    """
    max_nb = 2**nb_entries
    inputs = []
    numbers = range(2,max_nb)# list(range(2, max_nb))
    sample = random.sample(numbers,circuit_size)


    return '\n'.join([print_bits(number, nb_entries) for number in sample])


if __name__ == "__main__":
    nb_var = int(sys.argv[1])
    circuit_size_start = int(sys.argv[2])
    circuit_size_end = circuit_size_start+1
    circuit_nb = 1
    try:
        circuit_nb = int(sys.argv[4])
    except:
        pass
    try:
        circuit_size_end = int(sys.argv[3])
    except:
        pass

    for size in range(circuit_size_start,circuit_size_end):
        for j in range(circuit_nb):
            id_circuit = circuit_size_start + (size-circuit_size_start)*circuit_nb + j
            f = open('random_circuit_%s.txt'%(id_circuit), 'w')
            f.write(gen_circuit(nb_var, size))
            f.close()
    print("files created from %s to %s" % (circuit_size_start, circuit_size_start + (circuit_size_end-circuit_size_start)*circuit_nb))

