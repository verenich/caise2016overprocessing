rm(list=ls())

#PACKAGES AND SOURCE FILES
source('functions_overprocessing.R')

#used for partitioning the data
seed_nr <- 402
set.seed(seed_nr)

koActivities=c("IdCancellation", "PostFundingCancellation", "CreditDecision")

usefulFeatures = c("Age","Gender","Country","NewCreditCustomer","language_code","education_id",
                   "marital_status_id","nr_of_dependants","employment_status_id",
                   "Employment_Duration_Current_Employer","work_experience","occupation_area",
                   "home_ownership_type_id",
                   "income_from_principal_employer","income_total","TotalLiabilitiesBeforeLoan",
                   "TotalMonthlyLiabilities","DebtToIncome",
                   "AppliedAmountToIncome","LiabilitiesToIncome","NoOfPreviousApplications",
                   "AmountOfPreviousApplications",
                   "AppliedAmount","Interest","LoanDuration","UseOfLoan","ApplicationType",
                   koActivities,paste(koActivities,"_time",sep = ""))

numFeatures = c("AppliedAmount","Interest","LoanDuration","nr_of_dependants","income_from_principal_employer",
                "income_total",
                "TotalLiabilitiesBeforeLoan","TotalMonthlyLiabilities","DebtToIncome",
                "AppliedAmountToIncome","LiabilitiesToIncome","NoOfPreviousApplications",
                "AmountOfPreviousApplications")

computeBestPermutation(fileInputPath="Bondora.csv", fileOutputPath="bondora.csv", 
                       koActivities=koActivities, 
                       usefulFeatures, numFeatures,n=1)



### For the environmental permit log - uncomment if you want to use this ####
rm(list=ls())
source('functions_overprocessing.R')
seed_nr <- 4052
set.seed(seed_nr)

koActivities=c("T02","T06","T10")

usefulFeatures = c("Resource","X.case..channel","X.case..department","X.case..group","X.case..responsible",
                   "org.group", koActivities,paste(koActivities,"_time",sep = ""))

numFeatures = c()

disallowed_permutation = matrix(c("T10","T10","T06","T02","T02","T06"),ncol = length(koActivities))

computeBestPermutation(fileInputPath="Envpermit.csv", fileOutputPath="envpermit.csv", 
                       koActivities=koActivities, 
                       usefulFeatures, numFeatures,disallowed_permutation = disallowed_permutation, n=1)

