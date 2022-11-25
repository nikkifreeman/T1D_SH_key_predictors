# Key predictors of severe hypoglycemia in older adults with Type I Diabetes

For this project, we use data from a case-control study to identify a parsimonious set of key predictors of severe hypoglycemia in older adults with Type I Diabetes. After data clean and imputation, we fit random forest, L1-regularized logistic regression, and Gaussian process models in an impute-train-test framework. Analyses are performed in the R statistical programming language. 

# How to install and run this project

  1. Clone this repo to your local machine.
  2. ./1_code/0_analysisNotebook.Rmd is the analysis notebook and all analyses can be replicated using the notebook. Note that the relative file paths require running the chunks (file paths are relative to the notebook). Further note that file paths on a Windows machine may need modification. 
      * Functions related to extracting the analysis variables from the individual data tables are in ./1_code/1_createAnalysisRaw.R
      * Functions related to preparing the data for analysis are in ./1_code/2_dataPreparation2.R
      * Functions related to imputation are in ./1_code/3_imputation.R
      * Functions related to fitting the random forest are in ./1_code/4_randomForest.R
      * Functions related to fitting the L1-regularized logistic regression are in ./1_code/5_regLogisticReg.R
  
  3. The data tables are in SevereHypoDataset-c14d3739-6a20-449c-bbae-c02ff1764a91
