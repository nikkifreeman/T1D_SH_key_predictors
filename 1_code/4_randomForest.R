# Functions for the analysis using random forests

# Functions for fitting the random forest --------------------------------------
rfHelper <- function(trainingSet){
  randomForest(x = trainingSet %>% 
                 dplyr::select(-c(BCaseControlStatus,
                                  .imp, .id, PtID, exclude)),
               y = as.factor(trainingSet$BCaseControlStatus))
}

rfImportanceHelper <- function(rfObj){
  data.frame(randomForest::importance(rfObj)) %>%
    rownames_to_column(var = "predictor") %>%
    arrange(-MeanDecreaseGini)
}


