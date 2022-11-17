regLogisticReg_helper <- function(trainingSet){
  temp <- convertTrainingCategoricalToBinary(trainingSet)
  train(y = factor(trainingSet$BCaseControlStatus), 
        x = temp,
        method = "glmnet", trControl = trainControl(method = "cv", number = 5))
}

convertTrainingCategoricalToBinary <- function(trainingSet){
  trainingSet %>% 
    dplyr::select(-c(.imp, .id, PtID, BCaseControlStatus, exclude)) %>%
    mutate(Gender = if_else(Gender == "F", 1, 0),
           race_white = if_else(Race == "White", 1, 0),
           race_asian = if_else(Race == "Asian", 1, 0),
           race_black = if_else(Race == "Black/African American", 1, 0),
           hispanic = if_else(Ethnicity == "Hispanic or Latino", 1, 0),
           LiveAlone = if_else(LiveAlone == "Yes", 1, 0),
           InsDeliveryMethod = if_else(InsDeliveryMethod == "Pump", 1, 0),
           NumSHLastYr_1 = if_else(NumSHLastYr == "1", 1, 0),
           NumSHLastYr_2 = if_else(NumSHLastYr == "2", 1, 0),
           NumSHLastYr_3 = if_else(NumSHLastYr == "3", 1, 0),
           NumSHLastYr_4 = if_else(NumSHLastYr == "4", 1, 0),
           NumSHLastYr_5to9 = if_else(NumSHLastYr == "5 - 9", 1, 0),
           NumSHLastYr_10to19 = if_else(NumSHLastYr == "10 - 19", 1, 0),
           NumSHLastYr_more19 = if_else(NumSHLastYr == ">19", 1, 0),
           hypo_reducedAwareness = if_else(hypoUnaware == "reduced awareness", 1, 0),
           hypo_unaware = if_else(hypoUnaware == "unaware", 1, 0),
           college = if_else(educationCat == "College", 1, 0),
           advDegree = if_else(educationCat == "Advanced degree", 1, 0),
           underweight = if_else(bmiCat == "underweight", 1, 0),
           overweight = if_else(bmiCat == "overweight", 1, 0),
           obese = if_else(bmiCat == "obese", 1, 0),
           insDose_40to60 = if_else(insDoseCat == "40to60", 1, 0),
           insDose_greater60 = if_else(insDoseCat == "greater60", 1, 0),
           HBA1C = as.numeric(HBA1C)) %>%
    select(-c(Race, Ethnicity, NumSHLastYr, hypoUnaware, educationCat, bmiCat, insDoseCat))
}

convertTestCategoricalToBinary <- function(testData){
  testData %>% 
    dplyr::select(-c(PtID, BCaseControlStatus)) %>%
    mutate(Gender = if_else(Gender == "F", 1, 0),
           race_white = if_else(Race == "White", 1, 0),
           race_asian = if_else(Race == "Asian", 1, 0),
           race_black = if_else(Race == "Black/African American", 1, 0),
           hispanic = if_else(Ethnicity == "Hispanic or Latino", 1, 0),
           LiveAlone = if_else(LiveAlone == "Yes", 1, 0),
           InsDeliveryMethod = if_else(InsDeliveryMethod == "Pump", 1, 0),
           NumSHLastYr_1 = if_else(NumSHLastYr == "1", 1, 0),
           NumSHLastYr_2 = if_else(NumSHLastYr == "2", 1, 0),
           NumSHLastYr_3 = if_else(NumSHLastYr == "3", 1, 0),
           NumSHLastYr_4 = if_else(NumSHLastYr == "4", 1, 0),
           NumSHLastYr_5to9 = if_else(NumSHLastYr == "5 - 9", 1, 0),
           NumSHLastYr_10to19 = if_else(NumSHLastYr == "10 - 19", 1, 0),
           NumSHLastYr_more19 = if_else(NumSHLastYr == ">19", 1, 0),
           hypo_reducedAwareness = if_else(hypoUnaware == "reduced awareness", 1, 0),
           hypo_unaware = if_else(hypoUnaware == "unaware", 1, 0),
           college = if_else(educationCat == "College", 1, 0),
           advDegree = if_else(educationCat == "Advanced degree", 1, 0),
           underweight = if_else(bmiCat == "underweight", 1, 0),
           overweight = if_else(bmiCat == "overweight", 1, 0),
           obese = if_else(bmiCat == "obese", 1, 0),
           insDose_40to60 = if_else(insDoseCat == "40to60", 1, 0),
           insDose_greater60 = if_else(insDoseCat == "greater60", 1, 0),
           HBA1C = as.numeric(HBA1C)) %>%
    select(-c(Race, Ethnicity, NumSHLastYr, hypoUnaware, educationCat, bmiCat, insDoseCat))
}
