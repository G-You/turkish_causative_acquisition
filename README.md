# Children develop causatives despite pervasive ellipsis: Evidence from Turkish

## Introduction

Data and scripts for the study __Children develop causatives despite pervasive ellipsis: Evidence from Turkish__. Run _stat_analysis.R_ for quickly reproducing the results and plots. For processing the data before statistical analysis, please see below. Raw data (or an encrypted version) are available upon request.

## Data processing

### Step 1: collect data from parsed files
<pre>
	<code>
		python3 parsing_integration.py --genre child
		python3 parsing_integration.py --genre cds
	</code>
</pre>

### Step 2: calculate the entropy and set up the csv later analyzed in R
<pre>
	<code>
		python3 analyze_caus.py --genre child
		python3 analyze_caus.py --genre cds
	</code>
</pre>
