# Functions for feature selection - removing redundancy by detecting highly correlated variables

## Correlation matrix for model 1 ----------------------------------------------
getCorrelationMatrix_model1 <- function(df){
  corDf <- df %>% 
    dplyr::select(-c(BCaseControlStatus,
                     .imp, .id, PtID, exclude)) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = "educationCat", values_from = "var") %>% 
    mutate(var = 1) %>%
    pivot_wider(names_from = insurance, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = bmiCat, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = insDoseCat, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = glucoseMonitoringCat, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = hypoUnaware, values_from = var) %>%
    mutate(var = 1) %>%
    # pivot_wider(names_from = LiveAlone, values_from = var) %>%
    # mutate(var = 1) %>%
    pivot_wider(names_from = Gender, values_from = var) 
  # %>%
  # dplyr::select(-c(SymbDigOTotCorr, M, No))
  corDf[is.na(corDf)] <- 0
  correlationMatrix <- cor(corDf)
  
  return(correlationMatrix)
}

## Correlation matrix for model 2 ----------------------------------------------
getCorrelationMatrix_model2 <- function(df){
  corDf <- df %>%
    dplyr::select(-c(BCaseControlStatus,
                     .imp, .id, PtID, exclude)) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = "educationCat", values_from = "var") %>% 
    mutate(var = 1) %>%
    pivot_wider(names_from = insurance, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = bmiCat, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = insDoseCat, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = glucoseMonitoringCat, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = hypoUnaware, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = LiveAlone, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = Gender, values_from = var) 
  corDf[is.na(corDf)] <- 0
  correlationMatrix <- cor(corDf)
  
  return(correlationMatrix)
  
}

## Correlation matrix for model 3 ----------------------------------------------
getCorrelationMatrix_model3 <- function(df){
  corDf <-  df %>% 
    dplyr::select(-c(BCaseControlStatus,
                     .imp, .id, PtID, exclude)) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = "educationCat", values_from = "var") %>% 
    mutate(var = 1) %>%
    pivot_wider(names_from = insurance, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = bmiCat, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = insDoseCat, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = glucoseMonitoringCat, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = hypoUnaware, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = LiveAlone, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = Gender, values_from = var) 
  
  corDf[is.na(corDf)] <- 0
  correlationMatrix <- cor(corDf)
  
  return(correlationMatrix)
}

## Correlation matrix for model 4 ----------------------------------------------
getCorrelationMatrix_model4 <- function(df){
  corDf <- df %>% 
    dplyr::select(-c(BCaseControlStatus,
                     .imp, .id, PtID, exclude)) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = "educationCat", values_from = "var") %>% 
    mutate(var = 1) %>%
    pivot_wider(names_from = insurance, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = bmiCat, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = insDoseCat, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = glucoseMonitoringCat, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = hypoUnaware, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = LiveAlone, values_from = var) %>%
    mutate(var = 1) %>%
    pivot_wider(names_from = Gender, values_from = var) 
  
  corDf[is.na(corDf)] <- 0
  correlationMatrix <- cor(corDf)
  
  return(correlationMatrix)
}

# RFE --------------------------------------------------------------------------
# RFE
doRFE <- function(df){
  control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
  results <- rfe(df  %>% 
                   dplyr::select(-c(BCaseControlStatus,
                                    .imp, .id, PtID, exclude)),
                 y = as.factor(df$BCaseControlStatus), rfeControl = control)
  # print(results)
  # plot(results, type = c("g", "o"))
  # results$optVariables
  # results$bestSubset
  
  return(results)
}