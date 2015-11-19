#SET THE PATH
#the path should be the same as with the source file and Data
path=getwd()
path

#PACKAGES AND SOURCE FILES
source('functions_overprocessing.R')

#used for partitioning the data
seed_nr <- 40952

usefulFeatures = c("Age","Gender","Country","NewCreditCustomer","language_code","education_id",
                   "marital_status_id","nr_of_dependants","employment_status_id",
                   "Employment_Duration_Current_Employer","work_experience","occupation_area",
                   "home_ownership_type_id",
                   "income_from_principal_employer","income_total","TotalLiabilitiesBeforeLoan",
                   "TotalMonthlyLiabilities","DebtToIncome",
                   "AppliedAmountToIncome","LiabilitiesToIncome","NoOfPreviousApplications",
                   "AmountOfPreviousApplications",
                   "AppliedAmount","Interest","LoanDuration","UseOfLoan","ApplicationType",
                   "PostFundingCancellation","IdCancellation","CreditDecision", 
                   "PostFundingCancellation_time","IdCancellation_time","CreditDecision_time") 

numFeatures = c("AppliedAmount","Interest","LoanDuration","nr_of_dependants","income_from_principal_employer",
                "income_total",
                "TotalLiabilitiesBeforeLoan","TotalMonthlyLiabilities","DebtToIncome",
                "AppliedAmountToIncome","LiabilitiesToIncome","NoOfPreviousApplications",
                "AmountOfPreviousApplications")

computeBestPermutation(fileInputPath="Bondora.csv", fileOutputPath="prova5.csv", 
                       koActivities=c("IdCancellation", "PostFundingCancellation", "CreditDecision"), 
                       usefulFeatures, numFeatures,n=1)
