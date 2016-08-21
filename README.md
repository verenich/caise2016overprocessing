# caise2016overprocessing
Supplementary material for the article "Minimizing overprocessing waste in business processes via predictive activity ordering" by Ilya Verenich, Marlon Dumas, Marcello La Rosa, Fabrizio Maggi, and Chiara Di Francescomarino (http://eprints.qut.edu.au/91189)

Requirements: R 3.2.0 or above; internet connection to download the datasets and possibly missing packages

USAGE:
cd OptimizeKnockout
Rscript OptimizeKnockout.R inputData koActivities usefulFeatures disallowed_permutation learner sampling_method n retrain

Example usage:
Rscript OptimizeKnockout.R inputData="Bondora.csv" learner="svm" sampling_method="under" n=3

	Input: 	Event log in the csv format. Only "Bondora.csv" and "Envpermit.csv" have been tested so far  
	Output: 
	  - n csv files, containing optimal permutation, in the coded form, for each ongoing case according to our suggestion
	  (+ according to the constant probabilities baseline + random), number of checks actually performed for each case and the minimum possible number of checks
	  - file *_permutations.txt with the possible permutations and their codes
	  - file *_summary.txt containing total number of checks needed for all test cases, actual total number of performed checks and
	    distribution of the number of checks performed across test cases

	Parameters:
		- koActivities	list of checks in the knock-out sections. For example, koActivities='c("checkIncome","checkIdentity")'
		- usefulFeatures	list of features to be used for constructing predicitve models
		- disallowed_permutation	disallowed combination(s) of checks
		- learner	classification model for reject probabilities, "rf" for random forest, "tree" for decision tree and "svm" for SVM
		- sampling_method	sampling of training cases wrt class label to achieve class balance, "under" for undersampling, "none" for none, "over" for oversampling. Default "under"
		- n	number of random iterations of the whole algorithm (training and test files are regenerated at each run). Default 1
		- retrain:	logical flag indicating whether to retrain predicitve models. Default TRUE

Note: If using inputData="Bondora.csv" or inputData="Envpermit.csv", all other paramaters are optional 

Common problems:
  If you receive an error "'Rscript' is not recognized as an internal or external command, operable program or batch file",
  please add the path to the executables for R to the system variable "PATH" (on Windows typically 'C:\Program Files (x64)\R\R-3.x.y')

Note that this code is a supplementary material and should be modified for the use with other datasets
