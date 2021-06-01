import os, csv, string, argparse, xlrd, sys, time, pickle

class parsing_integration:

	def __init__(self):

		self.origin = []
		self.lemma = []
		self.POS = []
		self.POS_advanced = []
		self.gloss = []
		self.head = []
		self.role = []

		self.full = [self.origin, self.lemma, self.POS, self.POS_advanced, self.gloss, self.head, self.role]
		self.morph = {} # verb: [is_morph_caus, is_dir] 
		self.subtrees = {} 
		self.passive = {}

	def append_item(self, line):

		self.origin.append(line[1])
		self.lemma.append(line[2])
		self.POS.append(line[3])
		self.POS_advanced.append(line[4])
		self.gloss.append(line[5])
		self.head.append(line[6])
		self.role.append(line[7])

	def write_full(self, filename, session, utt):

		write_f = open(filename,"a+")
		csvwriter = csv.writer(write_f, delimiter="\t")
		csvwriter.writerow([session, utt]+
			[" ".join(unit) for unit in self.full])


	def find_verbal_subtree(self):

		#caus_markers = ["dir", ""]
		caus_markers = ["dir","dır","dur","dür","tir","tır","tur","tür"]

		for i in range(len(self.POS)):
			# "DERIV" no longer functions as a verb
			if (self.POS_advanced[i] == "Verb" and self.role != "DERIV"):
				# skip the repetitive verbs
				if ((i+1 < len(self.POS_advanced)) and 
					(self.POS_advanced[i+1] == "Verb") and 
					(self.lemma[i+1] == self.lemma[i])):
					continue

				# use the derived verbal form
				if (self.lemma[i] == "_"):
					verb = self.origin[i]
				else:
					verb = self.lemma[i]

				# argument structure
				new_tree = [self.role[j] for j in range(len(self.head)) if (self.head[j] == str(i+1) and self.role[j] in ["SUBJECT", "OBJECT"])]

				# add is_morphological_causative (Y/N)
				new_morph = ["Caus" in self.gloss[i]]
				new_morph.append (False) # default is_dir

				# add is_DIr (Y/N)
				if ("Caus" in self.gloss[i]):
					#print(self.origin[i])
					for marker in caus_markers:
						if marker in self.origin[i]:
							new_morph[-1] = True
							break

				if ("Pass" in self.gloss[i]):
					is_passive = True
				else:
					is_passive = False

				if verb in self.subtrees:
					self.subtrees[verb].append(new_tree)
					self.morph[verb].append(new_morph)
					self.passive[verb].append(is_passive)
				else:
					self.subtrees[verb] = [new_tree]
					self.morph[verb] = [new_morph]
					self.passive[verb] = [is_passive]


	def write_subtree(self, filename, session, utt, vocab, manual):

		f = open(filename, "a+")
		csvwriter = csv.writer(f, delimiter="\t")
		if (self.subtrees == {}):
			return
		for verb in self.subtrees:

			# plug in manual checks
			if (verb not in manual["verb"] and verb not in manual["convert"]):
				continue
			if (verb in manual["convert"]):
				stem = manual["convert"][verb]
			else:
				stem = verb

			# changes finish

			for subtree, morph, is_passive in zip(self.subtrees[verb], 
				self.morph[verb], self.passive[verb]):
				csvwriter.writerow([session,
					utt,				# utterance number 
					#verb,
					stem, 				# [CHANGE: "verb" to "stem"]
					#len(subtree), 		# number of arguments
					#len(subtree) == 2, 	# is complete arg structure
					#subtree[-2] or (verb in vocab[1]), 
					#morph[0],			# is caus
					verb in manual["already_morph"] or 
					(morph[0] and stem in manual["morph"]), # has to be able to take morph
					#verb in vocab[0], 	# is lexical [IMPORTANT: this is the old lex results]
					stem in vocab[0] or stem in vocab[1], 	# is lexical [highly lexicalized morphological caus]
					#" ".join(self.origin),	# full utterance
					#subtree,			# structure		
					"OBJECT" in subtree,	# has object
					#morph[1],			# is dir
					verb in manual["already_dir"] or
					(morph[1] and stem in manual["dir"]),
					is_passive or verb in manual["already_pass"]])		# is passive


def arg_parser():
	parser = argparse.ArgumentParser()
	parser.add_argument("--genre", default="cds", type=str, 
		help="genre of speech: cds or child")

	args = parser.parse_args()
	return args

def read_verb():
	# read the xlsx
	wb = xlrd.open_workbook("All turkish verbs_09.03.2020.xlsx")
	sheet = wb.sheet_by_index(0)
	# store the causatives
	lexical = []
	morph = []

	for i in range(sheet.nrows):
		
		word_cell = sheet.cell_value(i, 0)
		# skip the category labels and empty lines
		if (len(word_cell) == 0):
			continue
		if (word_cell[0].isdigit() or "i." in word_cell):
			continue

		word = word_cell.split("-")[0]
		if (sheet.cell_value(i, 3) == "lexical"):
			lexical.append(word)
		elif (sheet.cell_value(i, 3) == "morphological"):
			morph.append(word)

	return [lexical,morph]


def init_csv_files(filename, header):

	with open(filename, "w+") as f:
		csvwriter = csv.writer(f, delimiter="\t")
		csvwriter.writerow(header)
		f.close()


def main():

	args = arg_parser()


	init_csv_files("full_data_"+args.genre+".csv", ["session",
		"utterance_no",
		"utterance",
		"lemma",
		"POS",
		"POS_advanced",
		"gloss",
		"head",
		"dependency"])

	init_csv_files("verb_list_"+args.genre+".csv", ["session",
		"utterance_no",
		"verb",	
		#"arguments",
		#"complete_structure",
		"morphological",
		"lexical",
		
		#"utterance",
		#"structure",
		"if_object",
		"if_DIr",
		"if_passive"])


	sent = parsing_integration()

	# read the session and utterance number
	sessions = []
	utterances = []
	with open(args.genre+"_meta.txt", "r") as meta:
		for line in meta:
			sessions.append(line.split()[0])
			utterances.append(line.split()[1])

	# build the caus vocabulary
	voc = read_verb()

	# load the manual checks pickle
	with open("manual_checks.pickle","rb") as handle:
		manual_checks = pickle.load(handle)

	# read the parsed file
	with open ("parsed_"+args.genre+".txt") as csvfile:
		utterance_no = 0
		for line in csv.reader(csvfile, delimiter="\t"):
			# End of utterance
			if "." in line[1]:

				sent.write_full("full_data_"+args.genre+".csv",
					sessions[utterance_no],
					utterances[utterance_no])
				sent.find_verbal_subtree()

				sent.write_subtree("verb_list_"+args.genre+".csv",
					sessions[utterance_no],
					utterances[utterance_no],
					voc,
					manual_checks)

				# reset utterance
				utterance_no += 1
				sent = parsing_integration()
			else:
				sent.append_item(line)
				


if __name__ == "__main__":
	main()
