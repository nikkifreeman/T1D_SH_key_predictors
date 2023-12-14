# Data preparation
# Author: Nikki Freeman
# Created on: 28 August 2022
# Last modified: 19 October 2022

# Packages
library(tidyverse)

# Functions for loading the raw data ------------------------------------------
# Function to get the file names of the raw data set
getFileNames <- function(projDirectory){
  dataDirectoryName <- str_subset(list.files(projDirectory), "SevereHypoDataset")
  dataDirectoryName <- paste0(dataDirectoryName, "/Data Tables")
  fileNames <- list.files(paste0(projDirectory, dataDirectoryName))
  return(list(dataDirectoryName = dataDirectoryName,
              fileNames = fileNames))
}


# Function for loading the data tables
loadDataTables <- function(fileName, projDirectory, dataDirectoryName){
  read_delim(paste0(projDirectory, dataDirectoryName, "/", fileName), delim = "|")
}

# Function for getting the raw data tables and putting them into a list
getRawDataTables <- function(projDirectory){
  # Get the names of the raw data set files
  fileNames <- getFileNames(projDirectory = projDirectory)
  
  # Load the data tables
  dataTables <- map(fileNames$fileNames, loadDataTables, 
                    projDirectory = projDirectory,
                    dataDirectory = fileNames$dataDirectoryName)
  
  # Name the data tables
  names(dataTables) <- fileNames$fileNames
  
  dataTables
  
}


# Collect variables from the various forms -------------------------------------
collectVariables <- function(dataTables){
  # Cases and controls
  caseControl <- dataTables$BPtRoster.txt %>% 
    dplyr::select(-RecID) %>% 
    mutate(exclude = "No") %>%
    mutate(exclude = if_else(PtID == 194, "Yes - missing demographics", exclude),
           exclude = if_else(PtID == 183, "Yes - missing demographics", exclude),
           exclude = if_else(PtID %in% c(79, 190, 70), "Yes - <7 days CGM", exclude),
           exclude = if_else(PtID %in% c(50, 181, 84, 184, 128, 203), "Yes - <24h night CGM", exclude),
           exclude = if_else(PtID == 126, "Yes - no CGM data", exclude))
  
  # Collect the demographic, life, and diabetes history and management variables
  demographicsDiabDF <- dataTables$BDemoLifeDiabHxMgmt.txt %>%
    dplyr::select(PtID, Gender, Race, Ethnicity,
           T1DDiagAge, T1DDiagAgeUnk,
           EduLevel, EduLevelNoAns, EduLevelUnk, 
           AnnualInc, AnnualIncNoAns, AnnualIncUnk,
           InsPriv, InsGov, InsSingleService, InsNoCoverage, InsUnkown, InsNoAns, 
           DaysWkEx, DaysWkExUnk, DaysWkExDNA,
           DaysWkDrinkAlc, DaysWkDrinkAlcNone, DaysWkDrinkAlcUnk, 
           DaysMonBingeAlc, DaysMonBingeAlcNone, DaysMonBingeAlcUnk,
           LiveAlone, 
           InsDeliveryMethod, 
           UnitsInsTotalUnk, UnitsInsTotal,
           UnitsInsBasalOrLongAct, UnitsInsBasalOrLongActUnk, 
           NumPumpBolusOrShortAct, NumPumpBolusOrShortActUnk,
           NumMeterCheckDay, NumMeterCheckDayUnk,
           NumHospDKA, NumHospDKAUnk, 
           NumSHLastYr) 
  demographicsDiabDF <- left_join(caseControl, demographicsDiabDF, by = c("PtID"))
  
  # Collect and construct the CGM variables
  cgmDF <- dataTables$BDataCGM.txt %>% 
    # Create a variable indicating if day (6 AM to midnight) or night (midnight to 6 AM)
    mutate(day_night = if_else(DeviceTm >= hms::as_hms("00:00:00") & 
                                 DeviceTm <= hms::as_hms("06:00:00"), 
                               "night", "day"))
  
  # CGM missingness as defined in Table 3 of Weinstock et al (2016)
  # Find out which participants did not have at least 7 days of CGM
  cgmDF %>%
    group_by(PtID) %>%
    mutate(daysOfCGM = max(DeviceDaysFromEnroll) - min(DeviceDaysFromEnroll)) %>%
    arrange(daysOfCGM) %>%
    select(PtID, daysOfCGM) %>%
    distinct()
  # Patients 79, 190, and 70 only have 6 days of CGM data  
  
  # Find out which patients did not have at least 24 hours of nighttime cgm
  cgmDF %>%
    filter(!(PtID %in% c(79, 190, 70))) %>%
    group_by(PtID) %>%
    summarise(numNight = sum(day_night == "night")) %>%
    mutate(numNight = as.numeric(numNight)) %>%
    arrange(numNight) 
  # Check out 50, 181, 84, 184, 128, 203 
  cgmDF %>% filter(PtID == 35) %>%
    group_by(DeviceDaysFromEnroll) %>%
    filter(day_night == "night") %>%
    # filter(!is.na(Glucose)) %>%
    arrange(DeviceDaysFromEnroll, DeviceTm) %>% 
    mutate(diff = c(NA, diff(DeviceTm)/60)) %>%
    summarise(nFiveMinIntervals = sum(diff <= 5.5 & diff >= .45, na.rm = TRUE)) %>%
    ungroup() %>%
    summarise((sum(nFiveMinIntervals)*5)/(24*60))
  # Patients 50, 181, 84, 184, 128, 203  have less than 24h of nighttime cgm (number of 5 minute intervals)
  
  # Legacy code when we cared about day/night
  # cgmDF <- cgmDF %>% 
  #   # Calculate less than 70 and over 180
  #   mutate(bgLess70 = if_else(Glucose <70, 1, 0),
  #          bgGreater180 = if_else(Glucose > 180, 1, 0)) %>%
  #   group_by(PtID, day_night) %>%
  #   summarise(bgLess70_pct = mean(bgLess70, na.rm = TRUE)*100,
  #             bgGreater180_pct = mean(bgGreater180, na.rm = TRUE)*100,
  #             meanGlucose = mean(Glucose, na.rm = TRUE),
  #             pctCV = sd(Glucose, na.rm = TRUE)/mean(Glucose, na.rm = TRUE)*100) %>%
  #   pivot_longer(cols = bgLess70_pct:pctCV, names_to = "measure", values_to = "values") %>%
  #   unite(measure, c(measure, day_night), sep = "_") %>%
  #   pivot_wider(id_cols = PtID, names_from = measure, values_from = values) %>%
  #   ungroup() 
  
  cgmDF <- cgmDF %>%
    # Calculate less than 70 and over 180
    mutate(bgLess70 = if_else(Glucose <70, 1, 0),
           bgGreater180 = if_else(Glucose > 180, 1, 0)) %>%
    group_by(PtID) %>%
    summarise(bgLess70_pct = mean(bgLess70, na.rm = TRUE)*100,
              bgGreater180_pct = mean(bgGreater180, na.rm = TRUE)*100,
              meanGlucose = mean(Glucose, na.rm = TRUE),
              pctCV = sd(Glucose, na.rm = TRUE)/mean(Glucose, na.rm = TRUE)*100) %>%
    ungroup()
  cgmDF <- left_join(caseControl, cgmDF, by = "PtID") %>% ungroup()
  dim(cgmDF)
  
  # Collect the sample result variables
  sampleResultsDF <- dataTables$BSampleResults.txt %>%
    dplyr::select(PtID, Analyte, ResultName, Value, Units) %>%
    filter(Analyte %in% c("CPEP", "CREA-S", "HBA1C", "GLU")) 
  dim(sampleResultsDF)
  sampleResultsDF <- left_join(caseControl, sampleResultsDF, by = c("PtID"))
  sampleResultsDF %>% filter(PtID == 173)
  dim(sampleResultsDF)
  sampleResultsDF <- sampleResultsDF %>%
    filter(!(PtID == 173 & is.na(Value))) %>% # Remove duplicate CREA-S and CPEP for 173 that are NA 
    distinct() %>% # remove the duplicate entry of CPEP for 2
    mutate(Value = if_else(PtID == 2 & Analyte == "HBA1C", "6.3", Value)) %>% # Take the average of the duplicates
    mutate(Value = if_else(PtID == 2 & Analyte == "CREA-S", "0.58", Value)) %>% # Take the average of the duplicates
    mutate(Value = if_else(PtID == 2 & Analyte == "GLU", "247.5", Value)) %>% # Take the average of the duplicates
    distinct() %>% # remove the duplicates
    pivot_wider(names_from = Analyte, values_from = Value, id_cols = c(PtID, BCaseControlStatus, exclude)) %>%
    dplyr::select(-`NA`) %>%
    mutate(GLU = as.numeric(GLU),
           HBA1C = as.numeric(HBA1C))
  
  # Collect the Medical chart variables
  medChartDF <- dataTables$BMedChart.txt %>%
    dplyr::select(PtID, VisitDaysFromEnroll, 
           Weight, WeightUnits, WeightUnk, WeightUnits,
           Height, HeightUnits, HeightUnk, HeightUnits)
  dim(medChartDF)
  medChartDF <- left_join(caseControl, medChartDF, by = "PtID")
  dim(medChartDF)
  
  # Collect the Total test scores variables
  totalTestScoresDF <- dataTables$BTotTestScores.txt %>% 
    dplyr::select(PtID, BGVisit1NotDone,
                  HopVerbTr1TotCorr, HopVerbTr2TotCorr,
                  HopVerbTr3TotCorr, HopVerbTr4TotCorr,
           SymbDigWNotDone, SymbDigWTotCorr,
           SymbDigONotDone, SymbDigOTotCorr,
           TrailMakANotDone, TrailMakATotTime,
           TrailMakBNotDone, TrailMakBTotTime,
           GrPegDomNotDone, GrPegDomTotTime,
           FuncActNotDone, FuncActTotTestScore,
           DukeSocNotDone, DukeSocTotDSSI, 
           FrailtyFirstWalkTotTimeMin, FrailtyFirstWalkTotTimeSec,
           FrailtySecWalkTotTimeMin, FrailtySecWalkTotTimeSec) %>% 
    mutate(hopkinsTotal = HopVerbTr1TotCorr + HopVerbTr2TotCorr + HopVerbTr3TotCorr) %>%
    mutate(hopkinsRecall = HopVerbTr4TotCorr) %>%
    select(-c(HopVerbTr1TotCorr, HopVerbTr2TotCorr, HopVerbTr3TotCorr, HopVerbTr4TotCorr))
  dim(totalTestScoresDF)
  totalTestScoresDF <- left_join(caseControl, totalTestScoresDF, by = "PtID")
  dim(totalTestScoresDF)
  
  # Collect the diabetes numeracy test variables
  diabNumDF <- dataTables$BDiabNumTest.txt %>%
    mutate(InTarget118_only = if_else(InTarget118 == 1 & is.na(InTarget55) & is.na(InTarget145), 1, 0)) %>%
    dplyr::select(-c(InTarget55, InTarget145, InTarget118)) %>%
    mutate(across(BagChipTotCarbs:InsUnitsNight2, as.character)) %>%
    pivot_longer(cols = BagChipTotCarbs:InsUnitsNight2, names_to = "item", values_to = "response") %>%
    mutate(score = if_else(item == "BagChipTotCarbs" & response == "63", 1, 0),
           score = if_else(item == "PotCarbChoices" & response == "4", 1, score),
           score = if_else(item == "FoodLabelTotCarbs" & response == "36", 1, score),
           score = if_else(item == "CrackBeforeWalk" & response %in% c("1", "2"), 1, score),
           score = if_else(item == "InTarget118_only" & response == "1", 1, score),
           score = if_else(item == "StripsNeedVaca" & response == "56", 1, score),
           score = if_else(item == "DateBuyNewStrips" & response == "March 21st", 1, score),
           score = if_else(item == "MetTabsSecWk" & response == "2", 1, score),
           score = if_else(item == "SyrDrawCorrect" & response == "Yes", 1, score),
           score = if_else(item == "EatSupInsUnits" & response == "14", 1, score),
           score = if_else(item == "InsUnitsSlidScale" & response == "3", 1, score),
           score = if_else(item == "InsUnitsBrkfst" & response == "10", 1, score),
           score = if_else(item == "InsUnitsSup" & response == "12", 1, score),
           score = if_else(item == "InsUnitsNight1" & response == "32", 1, score),
           score = if_else(item == "InsUnitsNight2" & response == "34", 1, score)) %>%
    group_by(PtID) %>%
    summarise(diabNumeracyScore = (sum(score)/15)*100) %>%
    ungroup()
  diabNumDF <- left_join(caseControl, diabNumDF, by = "PtID")
  
  # Collect the geriatric depression scale variables
  # Scoring by using: https://wwwoundcare.ca/Uploads/ContentDocuments/Geriatric%20Depression%20Scale.pdf
  geriDepressionScaleDF <-
    dataTables$BGeriDepressScale.txt %>%
    pivot_longer(cols = BasicSatLife:MostPplBetterOff, names_to = "item", values_to = "response") %>%
    mutate(response = str_to_lower(response)) %>%
    mutate(resp_score = if_else(item == "BasicSatLife" & response == "no", 1, 0),
           resp_score = if_else(item == "DroppedActInt" & response == "yes", 1, resp_score),
           resp_score = if_else(item == "LifeEmpty" & response == "yes", 1, resp_score),
           resp_score = if_else(item == "BoredOften" & response == "yes", 1, resp_score),
           resp_score = if_else(item == "GoodSpirits" & response == "no", 1, resp_score),
           resp_score = if_else(item == "AfraidBad" & response == "yes", 1, resp_score),
           resp_score = if_else(item == "HappyMostTime" & response == "no", 1, resp_score),
           resp_score = if_else(item == "HelplessOFten" & response == "yes", 1, resp_score),
           resp_score = if_else(item == "PrefStayHome" & response == "yes", 1, resp_score),
           resp_score = if_else(item == "MoreMemProb" & response == "yes", 1, resp_score), 
           resp_score = if_else(item == "WondAlive" & response == "no", 1, resp_score),
           resp_score = if_else(item == "Worthless" & response == "yes", 1, resp_score),
           resp_score = if_else(item == "FullEnergey" & response == "no", 1, resp_score),
           resp_score = if_else(item == "HopelessSit" & response == "yes", 1, resp_score),
           resp_score = if_else(item == "MostPplSatLife" & response == "yes", 1, resp_score)) %>%
    group_by(PtID) %>%
    summarise(geriDepressionScore = sum(resp_score))
  geriDepressionScaleDF <- left_join(caseControl, geriDepressionScaleDF, by = "PtID")
  
  # Collect the hyperglycemia fear variables
  # Per the protocol for Weinstock et al (2016), the first 5 items were used for scoring
  bloodGlucoseAttitudeScaleDF <- dataTables$BBGAttitudeScale.txt %>%
    select(PtID, DealHypoEp, UndertreatHypo, HighBGDamage, FreqHypoDamage, DangersHighBG) %>%
    pivot_longer(cols = DealHypoEp:DangersHighBG, names_to = "item", values_to = "resp") %>%
    mutate(resp_num = if_else(resp == "Strongly disagree", 1, -1),
           resp_num = if_else(resp == "Disagree", 2, resp_num),
           resp_num = if_else(resp == "Neutral", 3, resp_num),
           resp_num = if_else(resp == "Agree", 4, resp_num),
           resp_num = if_else(resp == "Strongly agree", 5, resp_num)) %>% 
    group_by(PtID) %>%
    summarise(hyperFear = sum(resp_num)) %>%
    ungroup()
  bloodGlucoseAttitudeScaleDF <- left_join(caseControl, bloodGlucoseAttitudeScaleDF, by = "PtID")
  
  # Collect the hypoglycemic fear survey variables
  hypoFearSurveyDF <- dataTables$BHypoFearSurvey.txt %>%
    pivot_longer(cols = LgSnackBed:WorryDizzy, names_to = "item", values_to = "response") %>%
    group_by(PtID) %>%
    summarise(hypoFear = sum(response))
  hypoFearSurveyDF <- left_join(caseControl, hypoFearSurveyDF, by = "PtID")
  
  # Collect the Medications variables
  # using the list of beta blockers from Anna
  medicationDF <- dataTables$BMedication.txt %>%
    mutate(DrugName = str_to_lower(DrugName)) %>%
    mutate(betaBlocker = if_else(DrugName %in% c("acebutolol",
                                                 "sectral",
                                                 "atenolol",
                                                 "tenormin",
                                                 "bisoprolol",
                                                 "zebeta", 
                                                 "betaxolol",
                                                 "kerlone",
                                                 "metoprolol",
                                                 "lopressor",
                                                 "toprol XL",
                                                 "zebeta", 
                                                 "ziac",
                                                 "nadolol",
                                                 "corgard",
                                                 "carteolol", 
                                                 "cartrol",
                                                 "nebivolol",
                                                 "bystolic",
                                                 "carvedilol", 
                                                 "coreg",
                                                 "propranolol", 
                                                 "inderal", 
                                                 "innopran xl",
                                                 "labetalol",
                                                 "normodyne", 
                                                 "trandate",
                                                 "penbutolol",
                                                 "levatol",
                                                 "pindolol",
                                                 "visken",
                                                 "sotalol",
                                                 "betapace",
                                                 "timolol", 
                                                 "blocadren",
                                                 "metoprolol succinate",
                                                 "metoprolol tartrate",
                                                 "metroprolol",
                                                 "toprol-xl"), 1, 0)) %>%
    group_by(PtID) %>%
    summarise(betaBlocker = sum(betaBlocker)) %>%
    mutate(betaBlocker = if_else(betaBlocker > 0, 1, 0))
  medicationDF <- left_join(caseControl, medicationDF, by = "PtID")
  
  # Collect the Montreal cognitive assessment variables
  mocatDF <- dataTables$BMoCA.txt %>%
    dplyr::select(PtID, MoCANotDone, MoCATotal)
  dim(mocatDF)
  mocatDF <- left_join(caseControl, mocatDF, by = "PtID")
  dim(mocatDF)
  
  # Hypoglycemia unawareness survey
  ## Note the modified scoring per the Weinstock paper
  hypoUnawareDF <- dataTables$BHypoUnawareSurvey.txt %>% 
    mutate(Bel70_1 = if_else(Bel70PastMonWSymp == "Never", 0, -1),
           Bel70_1 = if_else(Bel70PastMonWSymp == "1 to 3 times", 1, Bel70_1),
           Bel70_1 = if_else(Bel70PastMonWSymp == "1 time/week", 2, Bel70_1),
           Bel70_1 = if_else(Bel70PastMonWSymp == "2 to 3 times/week", 3, Bel70_1),
           Bel70_1 = if_else(Bel70PastMonWSymp == "4 to 5 times/week", 4, Bel70_1),
           Bel70_1 = if_else(Bel70PastMonWSymp == "Almost daily", 5, Bel70_1)) %>%
    mutate(Bel70_2 = if_else(Bel70PastMonNoSymp == "Never", 0, -1),
           Bel70_2 = if_else(Bel70PastMonNoSymp == "1 to 3 times", 1, Bel70_2),
           Bel70_2 = if_else(Bel70PastMonNoSymp == "1 time/week", 2, Bel70_2),
           Bel70_2 = if_else(Bel70PastMonNoSymp == "2 to 3 times/week", 3, Bel70_2),
           Bel70_2 = if_else(Bel70PastMonNoSymp == "4 to 5 times/week", 4, Bel70_2),
           Bel70_2 = if_else(Bel70PastMonNoSymp == "Almost daily", 5, Bel70_2)) %>%
    mutate(Bel70 = if_else(Bel70_1 < Bel70_2, "R", "A")) %>% 
    select(-c(Bel70PastMonWSymp, Bel70PastMonNoSymp, Bel70_1, Bel70_2)) %>%
    pivot_longer(cols = LowBGSympCat:Bel70, 
                 names_to = "item", values_to = "response") %>% #filter(item == "Bel70PastMonWSymp") %>% distinct(response)
    mutate(score = if_else(item == "LowBGSympCat" & str_detect(response, "^Always"), "A", "?"),
           score = if_else(item == "LowBGSympCat" & str_detect(response, "^Sometimes|^No"), "R", score),
           score = if_else(item == "LowBGLostSymp" & response == "Yes", "R", score),
           score = if_else(item == "LowBGLostSymp" & response == "No", "A", score),
           score = if_else(item == "ModHypoEpPast6Mon" & response == "Never", "A", score),
           score = if_else(item == "ModHypoEpPast6Mon" & response != "Never", "R", score),
           score = if_else(item == "SevHypoEpPastYear" & response == "Never", "A", score),
           score = if_else(item == "SevHypoEpPastYear" & response!= "Never" & response!= "12 or more times", "R", score),
           score = if_else(item == "SevHypoEpPastYear" & response == "12 or more times", "U", score),
           score = if_else(item == "Bel70", response, score),
           score = if_else(item == "FeelSympLowBG" & str_detect(response, "^60|^50"), "A", score),
           score = if_else(item == "FeelSympLowBG" & str_detect(response, "^40|^<40"), "R", score),
           score = if_else(item == "ExtentSympLowBG" & response %in% c("Never", "Rarely", "Sometimes"), "R", score),
           score = if_else(item == "ExtentSympLowBG" & response %in% c("Often", "Always"), "A", score)) %>%
    group_by(PtID) %>%
    summarise(totalR = sum(score == "R"), totalA = sum(score == "A"), totalU = sum(score == "U")) %>%
    mutate(hypoUnaware = if_else(totalU > 0, "unaware", "?"),
           hypoUnaware = if_else(hypoUnaware == "?" & totalR >= 4, "reduced awareness", hypoUnaware),
           hypoUnaware = if_else(hypoUnaware == "?", "hypo aware", hypoUnaware)) %>%
    dplyr::select(-c(totalR, totalA, totalU)) %>% ungroup()
  hypoUnawareDF <- left_join(caseControl, hypoUnawareDF, by = "PtID")
  
  
  analysisData_raw <- demographicsDiabDF %>% 
    left_join(cgmDF, by = c("PtID", "BCaseControlStatus", "exclude")) %>%
    left_join(sampleResultsDF, by = c("PtID", "BCaseControlStatus", "exclude")) %>%
    left_join(medicationDF, by = c("PtID", "BCaseControlStatus", "exclude")) %>%
    left_join(medChartDF, by = c("PtID", "BCaseControlStatus", "exclude")) %>% 
    left_join(totalTestScoresDF, by = c("PtID", "BCaseControlStatus", "exclude")) %>% 
    left_join(geriDepressionScaleDF, by = c("PtID", "BCaseControlStatus", "exclude")) %>%
    left_join(bloodGlucoseAttitudeScaleDF, by = c("PtID", "BCaseControlStatus", "exclude")) %>%
    left_join(hypoUnawareDF, by = c("PtID", "BCaseControlStatus", "exclude")) %>%
    left_join(hypoFearSurveyDF, by = c("PtID", "BCaseControlStatus", "exclude")) %>%
    left_join(diabNumDF, by = c("PtID", "BCaseControlStatus", "exclude")) %>%
    left_join(mocatDF, by = c("PtID", "BCaseControlStatus", "exclude")) 
  
  analysisData_raw
}


