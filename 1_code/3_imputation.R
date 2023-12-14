# Function to create the imputation data sets
getImputationDataSets <- function(analysisWithMissing){
  analysisWithMissing <- analysisWithMissing %>%
    mutate(hypoUnaware = as.factor(hypoUnaware),
           educationCat = as.factor(educationCat),
           bmiCat = as.factor(bmiCat),
           insurance = as.factor(insurance),
           insDoseCat = as.factor(insDoseCat),
           glucoseMonitoringCat = as.factor(glucoseMonitoringCat),
           annualIncomeCat = as.factor(annualIncomeCat)
           ) %>%
    mutate(BCaseControlStatus = if_else(BCaseControlStatus == "Case", 1, 0))
  
  # Do multiple imputation
  imputationObj <- mice(analysisWithMissing %>% filter(str_detect(exclude, "No")), seed = 5678)
  complete(imputationObj, action = "long")
}

trainingSetListHelper <- function(df, k){
  df %>% filter(.imp == k) 
}

# Function to create the training and test sets
createTrainTest <- function(analysisWithMissing, imputedData, predictors, testSetIDs){
  # Make factors as needed
  analysisWithMissing <- analysisWithMissing %>%
    mutate(hypoUnaware = as.factor(hypoUnaware), 
           educationCat = as.factor(educationCat),
           bmiCat = as.factor(bmiCat),
           insurance = as.factor(insurance),
           insDoseCat = as.factor(insDoseCat),
           glucoseMonitoringCat = as.factor(glucoseMonitoringCat),
           annualIncomeCat = as.factor(annualIncomeCat)) %>%
    mutate(BCaseControlStatus = if_else(BCaseControlStatus == "Case", 1, 0)) %>%
    select(all_of(predictors)) %>%
    select(-c(exclude)) 
  
  # Create the test set
  testingData <- analysisWithMissing %>% filter(PtID %in% testSetIDs)
  
  # remove the test set from the imputation data sets; this yields the training set
  trainingSets <- imputedData %>% filter(!(PtID %in% testSetIDs))
  
  # Put each of the training sets in a list

  trainingSets_list <- map(1:5, trainingSetListHelper, df = trainingSets)
  
  return(list(testSetIDs = testSetIDs,
              testingData = testingData,
              trainingSets = trainingSets, 
              trainingSets_list = trainingSets_list))
}
