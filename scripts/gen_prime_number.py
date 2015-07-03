#-*- encoding: utf-8 -*-
"""
    Generate prime circuit
"""

import sys

def print_bits(number, nb_bits):
    """
        Print enought bit for number repr
    """
    value = bin(number)[2:]
    if len(value) < nb_bits:
        value = "0"*(nb_bits-len(value)) + value
    return value

def gen_not_prime_circuit(nb_entries, find_prime=True):
    """
        Generate a circuit to find prime or non-prime numbers
    """
    max_nb = 2**nb_entries
    inputs = []
    for i in range(2, max_nb):
        is_prime = True
        for prime in inputs:
            if i % prime == 0:
                is_prime = False
                break
        if (is_prime and find_prime) or not(is_prime or find_prime):
            inputs.append(i)

    for number in inputs:
        print(print_bits(number, nb_entries))


if __name__ == "__main__":
    nb_var = int(sys.argv[1])
    find_prime = bool(sys.argv[2])
    gen_not_prime_circuit(nb_var, find_prime)
