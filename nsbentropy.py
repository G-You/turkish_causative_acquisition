import os, csv, string, argparse, xlrd, sys, math, random, statistics, re
import numpy as np
import pandas as pd
import ndd
from scipy.special import psi
from collections import Counter
from sklearn.utils import resample

random.seed(42)

def shannon_entropy(counts):

	ent = 0
	#l = Counter(item for item in inp)
	#l =  Counter(inp)
	#values = [int(i) for i in l.values()]

	prob = [p/sum(counts) for p in counts]
	for p in prob:
		if (p==0):
			continue

		ent += p*math.log(p)

	return (-ent)

def nsb_entropy(counts, k):

    # maximum number of classes; nsb fails otherwise (when k is too large); 10000 is arbitrary here
    if (k > 10000):
        k = 10000

    try:
        output = ndd.entropy(counts, k, return_std=True)
    except:
        return 0,0
    return output

def sgt(counts, distribution = "binomial", t = 0, sim_size = 10000):

    n = sum(counts)
    counter_of_counts = Counter(counts)
    
    try:
        t_max = math.log(n)
        if (t > t_max):
            t = t_max
    except:
        return 0
    

    if (distribution == "poisson"):
        samples = np.random.poisson(par, sim_size)
    elif (distribution == "zipf"):
        samples = np.random.zipf(par, sim_size)
    elif (distribution == "binomial"):
        try:
            k = (1/2)*math.log(n*t*t)/math.log(3)
            q = 2/(t+2)
            samples = np.random.binomial(k, q, sim_size)
        except:
            return 0

    # U: unseen classes
    U = 0

    for key in counter_of_counts.keys():
        
        count = counter_of_counts[key]
        prob = sum(samples >= key)/sim_size

        try:
            U += pow(-t, key) * prob * count
        except:
            continue

    U = -U    

    if (U <0):
        return int(len(counts))
    else:
        return int(U + len(counts))

def get_counts(samples):

    counter = Counter(samples)
    counts = [int(i) for i in counter.values()]

    return counts

if __name__ == "__main__":
    None






