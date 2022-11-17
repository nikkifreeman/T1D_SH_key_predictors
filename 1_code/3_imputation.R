# Function to create the imputation data sets
getImputationDataSets <- function(analysisWithMissing){
  analysisWithMissing <- analysisWithMissing %>%
    mutate(NumSHLastYr = as.factor(NumSHLastYr),
           hypoUnaware = as.factor(hypoUnaware),
           educationCat = as.factor(educationCat),
           bmiCat = as.factor(bmiCat),
           insDoseCat = as.factor(insDoseCat)) %>%
    mutate(BCaseControlStatus = if_else(BCaseControlStatus == "Case", 1, 0))
  
  # Do multiple imputation
  imputationObj <- mice(analysisWithMissing %>% filter(str_detect(exclude, "No")), seed = 5678)
  complete(imputationObj, action = "long")
}

trainingSetListHelper <- function(df, k){
  df %>% filter(.imp == k) 
}

# Function to create the training and test sets
createTrainTest <- function(analysisWithMissing, imputedData){
  # Make factors as needed
  analysisWithMissing <- analysisWithMissing %>%
    mutate(NumSHLastYr = as.factor(NumSHLastYr),
           hypoUnaware = as.factor(hypoUnaware),
           educationCat = as.factor(educationCat),
           bmiCat = as.factor(bmiCat),
           insDoseCat = as.factor(insDoseCat)) %>%
    mutate(BCaseControlStatus = if_else(BCaseControlStatus == "Case", 1, 0)) %>%
    select(-c(exclude))
  
  # Create the test set
  completeCases <- analysisWithMissing[complete.cases(analysisWithMissing),]
  testSet <- caret::createDataPartition(y  = completeCases$BCaseControlStatus, p = 0.40)
  testSetIDs <- completeCases[testSet[[1]],]$PtID
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
