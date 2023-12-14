makeAnalysisDataWithMissing <- function(analysisData_raw){
  analysisData_raw %>%
    # Remove variables that were only included to verify missingness
    dplyr::select(-c(T1DDiagAgeUnk, DaysWkExDNA, DaysWkExUnk, NumHospDKAUnk, 
                     MoCANotDone, SymbDigWNotDone, SymbDigONotDone, TrailMakANotDone,
                     TrailMakBNotDone, GrPegDomNotDone,
                     FuncActNotDone, DukeSocNotDone))%>%
    # Use the same race/ethnicity categories as Weinstock (2016)
    mutate(nonHispWhite = if_else(Race == "White" & Ethnicity == "Not Hispanic or Latino", 1, 0)) %>%
    select(-c(Race, Ethnicity)) %>%
    # Use the same education categories as Weinstock (2016): 
    # Less than HS or high school diploma/GED, 
    # some college/associate or bachelor degree, master/professional or doctorate degree
    mutate(educationCat = if_else(EduLevel %in% c("High school graduate/diploma/GED",
                                                  "11th Grade",
                                                  "12th Grade - no diploma",
                                                  "7th or 8th Grade",
                                                  "9th Grade"), "HS", "?")) %>%
    mutate(educationCat = if_else(EduLevel %in% c("Associate Degree",
                                                  "Bachelor's Degree",
                                                  "Some college but no degree"), 
                                  "College", educationCat)) %>%
    mutate(educationCat = if_else(EduLevel %in% c("Doctorate Degree",
                                                  "Master's Degree",
                                                  "Professional Degree"),
                                  "Advanced degree", educationCat)) %>%
    mutate(educationCat = if_else(EduLevelNoAns == 1 & !is.na(EduLevelNoAns), as.character(NA), educationCat)) %>%
    dplyr::select(-c(EduLevel, EduLevelNoAns, EduLevelUnk))%>% 
    mutate(annualIncomeCat = if_else(AnnualInc == "$25,000 - $35,000" | 
                                       AnnualInc == "Less than $25,000", 
                                     "<35k", "?"),
           annualIncomeCat = if_else(AnnualInc == "$35,000 - less than $50,000", 
                                     "35-50k", annualIncomeCat),
           annualIncomeCat = if_else(AnnualInc == "$50,000 - less than $75,000" |
                                       AnnualInc == "$75,000 - less than $100,000", 
                                     "50-100k", annualIncomeCat),
           annualIncomeCat = if_else(AnnualInc == "$100,000 - less than $200,000" |
                                       AnnualInc == "$200,000 or more", 
                                     ">100k", annualIncomeCat)) %>%
    select(-c(AnnualInc, AnnualIncNoAns, AnnualIncUnk)) %>%
    # Create a new variable for insurance coverage as in Weinstock et al (2016)
    mutate(insurance = if_else((InsPriv == 1 & InsGov == 1) | (InsSingleService == 1 & InsGov == 1), "Government and commercial", "?"),
           insurance = if_else((InsPriv == 1 | InsSingleService == 1) & is.na(InsGov), "Only commercial", insurance),
           insurance = if_else(is.na(InsPriv) & is.na(InsSingleService) & InsGov == 1, "Only government", insurance),
           insurance = if_else(!is.na(InsNoCoverage) & InsNoCoverage == 1, "None", insurance)) %>%
    dplyr::select(-c(InsPriv, InsGov, InsSingleService, InsNoAns, InsNoCoverage))  %>%
    ungroup() %>%
    # Construct the BMI variable from height and weight (kg/m^2). 
    # Round to 1 decimal place as is customary in reporting.
    # Convert pounds to kilograms
    mutate(bmiNumerator = if_else(WeightUnits == "kg", Weight, -1),
           bmiNumerator = if_else(WeightUnits == "lbs", Weight*0.453592, bmiNumerator),
           bmiDenominator = if_else(HeightUnits == "cm", (Height*0.01)^2, -1),
           bmiDenominator = if_else(HeightUnits == "in", (Height*0.0254)^2, bmiDenominator),
           bmi = round(bmiNumerator/bmiDenominator, 1)) %>%
    dplyr::select(-c(Weight, WeightUnits, WeightUnk, Height, HeightUnk, 
                     HeightUnits, bmiNumerator, bmiDenominator)) %>%
    # As in Weinstock (2016), we'll also create a categorical bmi variable:  
    # underweight (BMI <18.5), normal weight (18.5 <= BMI < 25), 
    # overweight (25 <= BMI < 30), obese (BMI >= 30)mutate(bmiCat = if_else(bmi <18.5, "underweight", "?"),
    mutate(bmiCat = if_else(bmi <18.5, "underweight", "?"),
           bmiCat = if_else(18.5 <= bmi & bmi < 25, "normal", bmiCat),
           bmiCat = if_else(25 <= bmi & bmi < 30, "overweight", bmiCat),
           bmiCat = if_else(bmi >= 30, "obese", bmiCat)) %>%
    mutate(bmiCat = if_else(bmiCat %in% c("underweight", "normal"), "underweight or normal weight", bmiCat)) %>%
  # Cleaning up for alcohol use
  mutate(DaysMoDrinkAlc = if_else(!is.na(DaysWkDrinkAlcNone) & 
                                    DaysWkDrinkAlcNone == 1, 0, DaysWkDrinkAlc)) %>%
  select(-c(DaysWkDrinkAlc, DaysWkDrinkAlcNone, DaysWkDrinkAlcUnk)) %>%
  # Follwoing Weinstock et al (2016), we'll consider 0 days vs 1+ days binge drinking per month
  mutate(bingeAlc = if_else(!is.na(DaysMonBingeAlc), 1, 0),
         bingeAlc = if_else(!is.na(DaysMonBingeAlcNone) & DaysMonBingeAlcNone == 1, 0, bingeAlc),
         bingeAlc = if_else(is.na(DaysMoDrinkAlc) & is.na(DaysMonBingeAlcNone), as.numeric(NA), bingeAlc)) %>%
  select(-c(DaysMonBingeAlc, DaysMonBingeAlcNone, DaysMonBingeAlcUnk)) %>%
  # Following Weinstock (2016) we'll categorize units of total insulin as <40, 40-60, and >= 60
  mutate(insDoseCat = if_else(UnitsInsTotal < 40, "less40", "?"),
         insDoseCat = if_else(UnitsInsTotal >= 40 & UnitsInsTotal < 60, "40to60", insDoseCat),
         insDoseCat = if_else(UnitsInsTotal >= 60, "greater60", insDoseCat)) %>%
  dplyr::select(-c(UnitsInsTotalUnk, UnitsInsTotal)) %>%
  # Following Weinstock et al. (2016) categorize the number of meter checks per day
  mutate(glucoseMonitoringCat = if_else(NumMeterCheckDay == "0", "0", "?"),
         glucoseMonitoringCat = if_else(NumMeterCheckDay == "1" | 
                                          NumMeterCheckDay == "2" | 
                                          NumMeterCheckDay == "3", 
                                        "1-3", glucoseMonitoringCat),
         glucoseMonitoringCat = if_else(NumMeterCheckDay == "4", "4", glucoseMonitoringCat),
         glucoseMonitoringCat = if_else(NumMeterCheckDay == "5" | NumMeterCheckDay == "6", "5-6", glucoseMonitoringCat),
         glucoseMonitoringCat = if_else(NumMeterCheckDay %in% c("7", "8", "9"), "7-9", glucoseMonitoringCat),
         glucoseMonitoringCat = if_else(NumMeterCheckDay %in% c("10", "11", "12", "13", "14", "15", "16", "17", "18", "> 19"), 
                                        ">=10", glucoseMonitoringCat)) %>%
    dplyr::select(-c(NumMeterCheckDay, NumMeterCheckDayUnk)) %>% 
  # Following Weinstock et al (2016), consider num days hosp dka >=1 vs 0
  mutate(hospWithDKA = if_else(NumHospDKA %in% c(1, 2), 1, -1),
         hospWithDKA = if_else(NumHospDKA == 0, 0, hospWithDKA)) %>%
  dplyr::select(-NumHospDKA) %>%
  # Table 2 says detectable C-peptide is >= 0.017
  mutate(detectableCPEP = if_else(CPEP != "<0.017", 1, 0)) %>% 
  dplyr::select(-CPEP) %>%
  # Table 2 says Abnormal creatinine defined as >1.1 mg/dL for females and >1.2 mg/dL for males
  mutate(abnormalCreatinine = if_else((`CREA-S` > 1.1 & Gender == "F") | (`CREA-S` > 1.2 & Gender == "M"), 1, 0)) %>%
  dplyr::select(-`CREA-S`) %>%
  # Average over the frailty walks
  mutate(FrailtyFirstWalkTotTimeMin = FrailtyFirstWalkTotTimeMin*60,
         FrailtySecWalkTotTimeMin = FrailtySecWalkTotTimeMin*60,
         frailty1 = FrailtyFirstWalkTotTimeMin + FrailtyFirstWalkTotTimeSec,
         frailty2 = FrailtySecWalkTotTimeMin + FrailtySecWalkTotTimeSec) %>%
  rowwise() %>%
  mutate(frailty = mean(c(frailty1, frailty2), na.rm = TRUE)) %>%
  dplyr::select(-c(FrailtyFirstWalkTotTimeMin, FrailtyFirstWalkTotTimeSec, 
                   FrailtySecWalkTotTimeMin, FrailtySecWalkTotTimeSec, frailty1, frailty2)) %>%
  # Remove more variables that we don't need for the analysis (pulled before final predictor list decided)
  dplyr::select(-c(VisitDaysFromEnroll, BGVisit1NotDone,
                   T1DDiagAge, InsUnkown, 
                   BGVisit1NotDone, UnitsInsBasalOrLongActUnk,
                   NumPumpBolusOrShortActUnk, bmi, NumSHLastYr)) %>%
  # Filter out the observatiosn we have decided to exclude (for any reason)
  filter(str_detect(exclude, "No")) 
}


predictorSelection <- function(analysisWithMissing){
  analysisWithMissing %>% select(PtID, BCaseControlStatus, exclude, 
                                 Gender, nonHispWhite, educationCat,
                                 insurance, 
                                 annualIncomeCat,
                                 bmiCat,
                                 DaysWkEx, 
                                 LiveAlone, InsDeliveryMethod,
                                 insDoseCat,
                                 NumPumpBolusOrShortAct, glucoseMonitoringCat, 
                                 hospWithDKA, HBA1C, detectableCPEP, 
                                 betaBlocker, abnormalCreatinine, 
                                 MoCATotal, SymbDigWTotCorr, SymbDigOTotCorr, 
                                 TrailMakATotTime, TrailMakBTotTime, 
                                 GrPegDomTotTime, hypoFear, 
                                 hyperFear,
                                 DukeSocTotDSSI, frailty, hypoUnaware,
                                 meanGlucose_day, meanGlucose_night,
                                 bgLess70_pct_day, bgLess70_pct_night,
                                 bgGreater180_pct_day, bgGreater180_pct_night,
                                 pctCV_day, pctCV_night)
}

      