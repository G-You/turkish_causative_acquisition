import os, csv, string, argparse, xlrd, sys, math, random, statistics, re, pickle
import numpy as np
import ndd
from scipy.special import psi
from collections import Counter
from sklearn.utils import resample
import nsbentropy

random.seed(42)

def entropy(inp):

	ent = 0
	#l = Counter(item for item in inp)
	l =  Counter(inp)
	values = [int(i) for i in l.values()]
	prob = [p/sum(values) for p in values]
	for p in prob:
		ent += p*math.log2(p)

	return (-ent)


def boot(stats, iteration=1000, measure="entropy", alpha=0.95, size=1000, metric="mean"):

	results = []
	is_empty = False

	if (stats == []):
		#results = [0 for i in range(iteration)]
		return 0

	if (iteration == 0):

		iteration = 1000
		is_empty = True

	for i in range(iteration):

		resamp = resample(stats, n_samples=size)
		if (measure == "entropy"):
			results.append(entropy(resamp))
		elif (measure == "ndd"):
			results.append(ndd_entropy(resamp))
		elif (measure == "prop"):
			results.append(sum(resamp)/size)

	if (is_empty):
		return 0 - statistics.median(results)
	elif (metric == "mean"):
		return statistics.mean(results)
	elif (metric == "median"):
		return statistics.median(results)

def div0(x,y):
    try:
        return x/y
    except ZeroDivisionError:
        return 0

def get_child_info(filename, merge):
	
	f = open(filename,"r")
	child_info = {}
	csvreader = csv.reader(f,delimiter=",")
	for line in csvreader:
		name = line[7]
		age_in_days = line[10]

		year = line[8][1]
		month = re.sub(r'.+Y(.+)M.+',r'\1',line[8])
		age_in_months = int(year)*12 + int(month)

		if (merge):
			child_info[line[2]] = "%s.%s"%(name, str(age_in_months))
		else:
			child_info[line[2]] = "%s.%s"%(name, str(age_in_days))

	return child_info

def arg_parser():
	parser = argparse.ArgumentParser()
	#parser.add_argument("--file", default="tmp", type=str, help="file for parsing")
	parser.add_argument("--merge", default=False, action="store_true", help="merge or not the sessions in the same month")
	parser.add_argument("--genre", default="cds", type=str, help="genre of speech: cds or child")

	args = parser.parse_args()
	return args


def init_collection(session_list):

	new_dictionary = {}
	for s in session_list:
		new_dictionary[s] = []

	return new_dictionary


def main():

	args = arg_parser()
	child_info = get_child_info("tur_children.csv", args.merge)

	session_list = sorted(list(set(child_info.values())))
	#session_list = list(child_info)
	with open("manual_checks.pickle","rb") as handle:
		manual_checks = pickle.load(handle)

	verb_collection = init_collection(session_list)
	lex_collection = init_collection(session_list)
	morph_collection = init_collection(session_list)
	dir_collection = init_collection(session_list)
	noncaus_collection = init_collection(session_list)

	non_passive_verb = init_collection(session_list)
	non_passive_lex = init_collection(session_list)
	non_passive_morph = init_collection(session_list)
	non_passive_dir = init_collection(session_list)
	non_passive_noncaus = init_collection(session_list)
	
	verb_w_obj = init_collection(session_list)
	lex_w_obj = init_collection(session_list)
	morph_w_obj = init_collection(session_list)
	dir_w_obj = init_collection(session_list)
	noncaus_w_obj = init_collection(session_list)

	with open("verb_list_%s.csv"%args.genre) as f:
		next(f)
		for line in f:
			items = line.strip().split("\t")

			raw_session = items[0]
			session = child_info[raw_session]
			verb = items[2]
			# all verbs
			verb_collection[session].append(verb)			# append verb
			
			# if non-passive; if has object
			if (items[7] == "False"):
				non_passive_verb[session].append(verb)

				if (items[5] == "True"):
					verb_w_obj[session].append(verb)

			# if lexical
			if (items[4] == "True"):
				lex_collection[session].append(verb)		# append lex

				if (items[7] == "False"):
					non_passive_lex[session].append(verb)

					# if has object
					if (items[5] == "True"):
						lex_w_obj[session].append(verb)
			# if morphological
			if (items[3] == "True"):
				morph_collection[session].append(verb)		# append morph

				if (items[7] == "False"):
					non_passive_morph[session].append(verb)

					# if has object					
					if (items[5] == "True"):
						morph_w_obj[session].append(verb)

			if (items[6] == "True"):
				dir_collection[session].append(verb)		# append dir

				if (items[7] == "False"):
					non_passive_dir[session].append(verb)
					# if has object
					if (items[5] == "True"):
						dir_w_obj[session].append(verb)

			if (items[4] != "True" and items[3] != "True"):
				noncaus_collection[session].append(verb)	# append noncaus

				if (items[7] == "False"):
					non_passive_noncaus[session].append(verb)
					# if has object
					if (items[5] == "True"):
						noncaus_w_obj[session].append(verb)



	with open("results_%s.csv"%args.genre,"w+") as f:

		csvwriter = csv.writer(f, delimiter="\t")
		csvwriter.writerow(["child",
			"age",
			"verb_types",
			"lexical_types",
			"morph_types",
			"dir_types",
			"noncaus_types",
			"verb_tokens",
			"lexical_tokens",
			"morph_tokens",
			"dir_tokens",
			"noncaus_tokens",
			"verb_entropy",
			"verb_var",
			"lex_entropy",
			"lex_var",
			"morph_entropy",
			"morph_var",
			"dir_entropy",
			"dir_var",
			"noncaus_entropy",
			"noncaus_var",
			"dir_inv_entropy",
			"dir_inv_var",
			"verb_obj",
			"lex_obj",
			"morph_obj",
			"dir_obj",
			"noncaus_obj"])

		for session in verb_collection:
			print(session)

			verb_type = len(set(verb_collection[session]))
			# change from 0 to 1; log1 would be 0
			if (verb_type == 0):
				verb_type = 1


			verb_counts = nsbentropy.get_counts(verb_collection[session])
			lex_counts = nsbentropy.get_counts(lex_collection[session])
			morph_counts = nsbentropy.get_counts(morph_collection[session])
			dir_counts = nsbentropy.get_counts(dir_collection[session])
			noncaus_counts = nsbentropy.get_counts(noncaus_collection[session])

			morph_inventory_counts = nsbentropy.get_counts(
				[verb for verb in verb_collection[session] 
				if verb in manual_checks["morph"]])
			dir_inventory_counts = nsbentropy.get_counts(
				[verb for verb in verb_collection[session] 
				if verb in manual_checks["dir"]])

			try:
				t = math.log(sum(verb_counts))
			except:
				t = 0

			entropy_measures = [nsbentropy.nsb_entropy(verb_counts, 
				nsbentropy.sgt(verb_counts, "binomial", t)),
				nsbentropy.nsb_entropy(lex_counts, 
				nsbentropy.sgt(lex_counts, "binomial", t)),
				nsbentropy.nsb_entropy(morph_counts, 
				nsbentropy.sgt(morph_inventory_counts, "binomial", t)),
				nsbentropy.nsb_entropy(dir_counts, 
				nsbentropy.sgt(dir_inventory_counts, "binomial", t)),
				nsbentropy.nsb_entropy(noncaus_counts,
				nsbentropy.sgt(noncaus_counts, "binomial", t)),
				nsbentropy.nsb_entropy(dir_inventory_counts, 
				nsbentropy.sgt(dir_inventory_counts, "binomial", t))]
			

			csvwriter.writerow([session.split(".")[0],
				session.split(".")[1],

				# number of unique verbs
				len(set(verb_collection[session])),
				len(set(lex_collection[session])),
				len(set(morph_collection[session])),
				len(set(dir_collection[session])),
				len(set(noncaus_collection[session])),

				len((verb_collection[session])),
				len((lex_collection[session])),
				len((morph_collection[session])),
				len((dir_collection[session])),
				len((noncaus_collection[session])),

				entropy_measures[0][0],
				entropy_measures[0][1],
				entropy_measures[1][0],
				entropy_measures[1][1],
				entropy_measures[2][0],
				entropy_measures[2][1],
				entropy_measures[3][0],
				entropy_measures[3][1],
				entropy_measures[4][0],
				entropy_measures[4][1],
				entropy_measures[5][0],
				entropy_measures[5][1],

				boot(stats=[1] * len(verb_w_obj[session]) + 
					[0] * (len(non_passive_verb[session])-len(verb_w_obj[session])), measure = "prop"),
				boot(stats=[1] * len(lex_w_obj[session]) + 
					[0] * (len(non_passive_lex[session])-len(lex_w_obj[session])), measure = "prop"),
				boot(stats=[1] * len(morph_w_obj[session]) + 
					[0] * (len(non_passive_morph[session])-len(morph_w_obj[session])), measure = "prop"),
				boot(stats=[1] * len(dir_w_obj[session]) + 
					[0] * (len(non_passive_dir[session])-len(dir_w_obj[session])), measure = "prop"),
				boot(stats=[1] * len(noncaus_w_obj[session]) + 
					[0] * (len(non_passive_noncaus[session])-len(noncaus_w_obj[session])), measure = "prop")
				])

if __name__ == "__main__":
	main()