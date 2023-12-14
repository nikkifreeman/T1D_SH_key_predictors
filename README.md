# Key predictors of severe hypoglycemia in older adults with Type I Diabetes

For this project, we use data from a case-control study to identify a parsimonious set of key predictors of severe hypoglycemia in older adults with Type I Diabetes. After data cleaning and imputation, we analyzed 4 successively richer models (Figure 1) using the Random Forests algorithm in an impute-train-test framework. Analyses were performed in the R statistical programming language. 

![Figure 1](/4_images/figure1.png)
Figure 1. Models. We tested four models that were successively more complex, incorporating more individual-level factors that may be associated with SH. 

# How to install and run this project

  1. Clone this repo to your local machine.
  2. ./1_code/0_analysisNotebook.Rmd is the analysis notebook and all analyses can be replicated using the notebook. Note that the relative file paths require running the chunks (file paths are relative to the notebook). Further note that file paths on a Windows machine may need modification. 
      * Functions related to extracting the analysis variables from the individual data tables are in ./1_code/1_createAnalysisRaw.R
      * Functions related to preparing the data for analysis are in ./1_code/2_dataPreparation2.R
      * Functions related to imputation are in ./1_code/3_imputation.R
      * Functions related to fitting the random forest are in ./1_code/4_randomForest.R
  
  3. The data used in this analysis are publicly available from the JAEB Center for Health Research (https://www.jaeb.org/). The data are not included in this repo.
