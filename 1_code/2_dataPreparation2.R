makeAnalysisDataWithMissing <- function(analysisData_raw){
  analysisData_raw %>%
    # Remove variables that were only included to verify missingness
    dplyr::select(-c(T1DDiagAgeUnk, DaysWkExDNA, DaysWkExUnk, NumHospDKAUnk, 
                     MoCANotDone, SymbDigWNotDone, SymbDigONotDone, TrailMakANotDone,
                     TrailMakBTotTime, TrailMakBNotDone, GrPegDomNotDone,
                     FuncActNotDone, DukeSocNotDone))%>%
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
    # Create a new variable for has insurance/does not have insurance coverage
    rowwise() %>%
    mutate(nInsurance = sum(c(InsPriv, InsGov, InsSingleService), na.rm = TRUE)) %>% 
    mutate(anyInsurance = if_else(nInsurance > 0, 1, -1),
           anyInsurance = if_else(!is.na(InsNoCoverage) & InsNoCoverage == 1, 0, anyInsurance),
           anyInsurance = if_else(InsNoAns == 1 & !is.na(InsNoAns), as.numeric(NA), anyInsurance)) %>%
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
  # Cleaning up for alcohol use
  mutate(DaysMoDrinkAlc = if_else(!is.na(DaysWkDrinkAlcNone) & 
                                    DaysWkDrinkAlcNone == 1, 0, DaysWkDrinkAlc)) %>%
  select(-c(DaysWkDrinkAlc, DaysWkDrinkAlcNone, DaysWkDrinkAlcUnk)) %>%
  # Following Weinstock (2016) we'll categorize units of total insulin as <40, 40-60, and >= 60
  mutate(insDoseCat = if_else(UnitsInsTotal < 40, "less40", "?"),
         insDoseCat = if_else(UnitsInsTotal >= 40 & UnitsInsTotal < 60, "40to60", insDoseCat),
         insDoseCat = if_else(UnitsInsTotal >= 60, "greater60", insDoseCat)) %>%
  dplyr::select(-c(UnitsInsTotalUnk, UnitsInsTotal)) %>%
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
  dplyr::select(-c(FreqHypoDamage, DangersHighBG, VisitDaysFromEnroll, BGVisit1NotDone,
                   T1DDiagAge, InsUnkown, DealHypoEp, UndertreatHypo, HighBGDamage,
                   nInsurance, BBGAttitudeScaleNotDone, BGVisit1NotDone, UnitsInsBasalOrLongActUnk,
                   NumPumpBolusOrShortActUnk, bmi)) %>%
  # Filter out the observatiosn we have decided to exclude (for any reason)
  filter(str_detect(exclude, "No"))

}
