# Data Preparation
# Author: Nikki Freeman
# Created on: 5 October 2022
# Last modified: 5 October 2022

# Function to help summarize the categorical variables
getOverallStratifiedCounts <- function(dataframe, variableName){
  overallCounts <- dataframe %>%
    filter(exclude == "No") %>%
    group_by(!!sym(variableName)) %>%
    count(name = "Overall")
  stratifiedCounts <- dataframe %>%
    filter(exclude == "No") %>%
    group_by(!!sym(variableName), BCaseControlStatus) %>%
    count() %>%
    pivot_wider(names_from = "BCaseControlStatus", values_from = "n")
  left_join(overallCounts, stratifiedCounts, by = variableName)
}


# Old --------------------------------------------------------------------------


# 
# demographicsDiabDF %>%
#   filter(exclude == "No") %>%
#   summarise(meanAge = mean(T1DDiagAge, na.rm = TRUE),
#             medAge = median(T1DDiagAge, na.rm = TRUE),
#             minAge = min(T1DDiagAge, na.rm = TRUE),
#             maxAge = max(T1DDiagAge, na.rmu = TRUE))
# demographicsDiabDF %>%
#   filter(exclude == "No") %>%
#   ggplot(aes(x = T1DDiagAge)) +
#   geom_histogram(binwidth = 5)