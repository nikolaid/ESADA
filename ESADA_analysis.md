#### Dataset view

Install the latest version of the `foreign` package: `install.packages("foreign")`

``` r
library(foreign)
```

\*Converitng file format (.sav to .csv)

``` r
write.table(read.spss("ESADA.sav"), file="ESADA.csv", col.names = TRUE, 
            row.names = FALSE, sep = ",")
library(readr)
```

\*Reading the file

``` r
ESADA <-read_csv("ESADA.csv")
```

*How many unique sites?*

``` r
unique(ESADA$Site)
```

    ##  [1] "Prague"                                            
    ##  [2] "Brno"                                              
    ##  [3] "Klecany"                                           
    ##  [4] "Thessaloniki"                                      
    ##  [5] "Alexandroupolis"                                   
    ##  [6] "Crete"                                             
    ##  [7] "Athens"                                            
    ##  [8] "Brussels"                                          
    ##  [9] "Antwerp"                                           
    ## [10] "Paris"                                             
    ## [11] "Grenoble"                                          
    ## [12] "Barcelona"                                         
    ## [13] "Lleida"                                            
    ## [14] "C<U+00E3>ceres"                                    
    ## [15] "Turku"                                             
    ## [16] "Riga"                                              
    ## [17] "Split"                                             
    ## [18] NA                                                  
    ## [19] "Palermo"                                           
    ## [20] "Milano"                                            
    ## [21] "Kosice"                                            
    ## [22] "Edinburgh"                                         
    ## [23] "Gothenburg"                                        
    ## [24] "F<U+00F8>rde"                                      
    ## [25] "Warsaw Medical University"                         
    ## [26] "Warsaw Institute of Tuberculosis and Lung Diseases"
    ## [27] "Berlin"                                            
    ## [28] "Giessen"                                           
    ## [29] "Hamburg"                                           
    ## [30] "Mainz"                                             
    ## [31] "Lisbon"                                            
    ## [32] "Porto"                                             
    ## [33] "Dublin"                                            
    ## [34] "Klaipeda"                                          
    ## [35] "Izmir"                                             
    ## [36] "Haifa"

\*Correcting language issue

``` r
Sys.setlocale(category = "LC_CTYPE", locale = "en_US.UTF-8")
```

    ## [1] "en_US.UTF-8"

#### Cleaning the data

\*Install the latest version of the `tidyverse` package: `install.packages("tidyverse")`

``` r
library(tidyverse)
```

``` r
which(is.na(ESADA$Site))
```

    ## [1] 8415

\*In row \#8415 there is a missing value

\*Search for "Site" name elsewhere in the dataset

``` r
names(select(ESADA, contains("Site")))
```

    ## [1] "Site"   "F1Site" "F2Site" "F3Site" "F4Site" "F5Site"

``` r
grep("*Site$", colnames(ESADA) )
```

    ## [1]    1  216  460  705  947 1187

*Check other Site names*

``` r
ESADA[8415, c(1,216,460,705,947,1187)]
```

    ## # A tibble: 1 x 6
    ##   Site  F1Site F2Site F3Site F4Site F5Site
    ##   <chr> <chr>  <chr>  <chr>  <chr>  <chr> 
    ## 1 <NA>  Split  <NA>   <NA>   <NA>   <NA>

*Recode missing \#8415*

``` r
ESADA$Site[is.na(ESADA$Site)] <- "Split"
```

*Create new variable "Country"*

``` r
ESADA$Country <- ifelse(ESADA$Site %in% c("Prague", "Brno", "Klecany"), "Czech Republic",
ifelse(ESADA$Site %in% c("Thessaloniki", "Alexandroupolis", "Crete", "Athens"), "Greece",
ifelse(ESADA$Site %in% c("Brussels", "Antwerp"), "Belgium", 
ifelse(ESADA$Site %in% c("Paris", "Grenoble"), "France",
ifelse(ESADA$Site %in% c("Barcelona", "Lleida", "Cãceres"), "Spain", 
ifelse(ESADA$Site=="Turku", "Finland", 
ifelse(ESADA$Site=="Riga", "Latvia", 
ifelse(ESADA$Site=="Split", "Croatia", 
ifelse(ESADA$Site %in% c("Palermo", "Milano"), "Italy", 
ifelse(ESADA$Site=="Kosice", "Slovakia", 
ifelse(ESADA$Site=="Edinburgh", "Scotland", 
ifelse(ESADA$Site=="Gothenburg", "Sweden", 
ifelse(ESADA$Site=="Førde", "Norway",
ifelse(ESADA$Site %in% c("Warsaw Medical University", 
                         "Warsaw Institute of Tuberculosis and Lung Diseases"), "Poland", 
ifelse(ESADA$Site %in% c("Berlin", "Giessen", "Hamburg", "Mainz"), "Germany", 
ifelse(ESADA$Site %in% c("Lisbon", "Porto"), "Portugal", 
ifelse(ESADA$Site=="Dublin", "Ireland", 
ifelse(ESADA$Site=="Klaipeda", "Lithuania", 
ifelse(ESADA$Site=="Izmir", "Turkey", 
ifelse(ESADA$Site=="Haifa", "Israel", NA))))))))))))))))))))
```

\*Move "Country" next to "Site"

``` r
ESADA <- ESADA %>% select(Site, Country, everything())
```

*Number of countries*

``` r
unique(ESADA$Country)
```

    ##  [1] "Czech Republic" "Greece"         "Belgium"        "France"        
    ##  [5] "Spain"          "Finland"        "Latvia"         "Croatia"       
    ##  [9] "Italy"          "Slovakia"       "Scotland"       "Sweden"        
    ## [13] "Norway"         "Poland"         "Germany"        "Portugal"      
    ## [17] "Ireland"        "Lithuania"      "Turkey"         "Israel"

*Data entries per country*

``` r
table(ESADA$Country)
```

    ## 
    ##        Belgium        Croatia Czech Republic        Finland         France 
    ##           1280           1612           1074           1067           1229 
    ##        Germany         Greece        Ireland         Israel          Italy 
    ##           1100           2084           1628            580           1109 
    ##         Latvia      Lithuania         Norway         Poland       Portugal 
    ##             18            700           2078           2938            272 
    ##       Scotland       Slovakia          Spain         Sweden         Turkey 
    ##            316            937            580           2487           2410

``` r
by_country <- ESADA %>%
    group_by(Country) %>%
    summarise(counts = n())
par(mar=c(6.1,4.1,4.1,2.1))
barplot(by_country$counts, name=by_country$Country, 
        cex.names = 0.8, ylim= c(0,3500), las=2, main="Patients per Country")
```

![](https://github.com/nikolaid/ESADA/blob/master/unnamed-chunk-15-1.png)

*Create a subset of the data*

``` r
Data1 <- ESADA[c(1:215)]
write.csv(Data1, file = "Data1.csv")
```

*Looking at the file*

``` r
str(Data1)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    25499 obs. of  215 variables:
    ##  $ Site                                       : chr  "Prague" "Prague" "Prague" "Prague" ...
    ##  $ Country                                    : chr  "Czech Republic" "Czech Republic" "Czech Republic" "Czech Republic" ...
    ##  $ PatientID                                  : num  2e+07 2e+07 2e+07 2e+07 2e+07 ...
    ##  $ VisitDate                                  : num  1.34e+10 1.34e+10 1.34e+10 1.34e+10 1.34e+10 ...
    ##  $ Year                                       : num  2008 2008 2008 2008 2008 ...
    ##  $ Gender                                     : chr  "Male" "Male" "Male" "Male" ...
    ##  $ Race                                       : chr  "Caucasian" "Caucasian" "Caucasian" "Caucasian" ...
    ##  $ Age                                        : num  63 36 39 46 33 54 55 59 53 42 ...
    ##  $ Height                                     : num  178 178 175 175 177 173 170 192 176 175 ...
    ##  $ Weight                                     : num  80 85 98 90 90 72 100 130 93 95 ...
    ##  $ BMI                                        : num  25.2 26.8 32 29.4 28.7 ...
    ##  $ SystolicBloodPressure                      : num  135 135 194 130 150 180 140 150 130 120 ...
    ##  $ MeanBloodPressure                          : num  98.3 95 127.3 96.7 106.7 ...
    ##  $ DiastolicBloodPressure                     : num  80 75 94 80 85 100 110 100 70 80 ...
    ##  $ Pulse_Pressure                             : num  55 60 100 50 65 80 30 50 60 40 ...
    ##  $ HeartRate                                  : num  66 48 66 106 66 100 93 71 66 80 ...
    ##  $ RatePressureProduct                        : num  8910 6480 12804 13780 9900 ...
    ##  $ Waist                                      : num  98 99 107 94 101 NA 115 133 108 102 ...
    ##  $ Hip                                        : num  104 106 113 102 100 NA 114 121 108 106 ...
    ##  $ WHR                                        : num  0.942 0.934 0.947 0.922 1.01 ...
    ##  $ Neck                                       : num  41 40 45 41 43 NA 46 48 40 45 ...
    ##  $ AverageSubjectiveSleepLength               : num  6 6 8 8 7.5 7.5 8.5 7 9 10 ...
    ##  $ AverageSubjectiveSleepLatency              : num  15 105 5 10 10 15 5 60 10 5 ...
    ##  $ ESS                                        : num  8 11 7 14 3 NA NA NA NA NA ...
    ##  $ LicenseNotSet                              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ LicenseNo                                  : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ LicenseYes                                 : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ LicenseAB                                  : num  1 1 0 1 1 1 1 1 1 1 ...
    ##  $ LicenseCDE                                 : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Kmyear                                     : num  8000 12000 NA 8000 8000 12000 8000 12000 8000 8000 ...
    ##  $ CGISNotSet                                 : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CGISNormalnotatallill                      : num  0 0 1 0 0 0 0 0 0 0 ...
    ##  $ CGISBorderlineill                          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CGISMildlyill                              : num  0 0 0 0 1 1 0 0 1 0 ...
    ##  $ CGISModeratelyill                          : num  0 0 0 0 0 0 1 0 0 1 ...
    ##  $ CGISMarkedlyill                            : num  1 1 0 0 0 0 0 1 0 0 ...
    ##  $ CGISSeverlyill                             : num  0 0 0 1 0 0 0 0 0 0 ...
    ##  $ CGISAmongthemostextremelyill               : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CGIINotset                                 : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ CGIIVeryMuchImproved                       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CGIIMuchImproved                           : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CGIIMinimallyImproved                      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CGIINoChange                               : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CGIIMinimallyWorse                         : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CGIIMuchWorse                              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CGIIVeryMuchWorse                          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SmokingNotSet                              : num  0 0 0 0 0 0 1 1 1 0 ...
    ##  $ SmokingNo                                  : num  1 1 1 1 1 0 0 0 0 0 ...
    ##  $ SmokingYes                                 : num  0 0 0 0 0 1 0 0 0 1 ...
    ##  $ Alcoholunits                               : num  4 3 5 2 2 3 14 21 0 3 ...
    ##  $ NotApplicable_A                            : num  0 0 0 0 0 1 0 0 0 0 ...
    ##  $ CholesterolmmolL                           : num  5.2 5.4 4.19 4.76 4.54 NA 5.06 6.44 4.67 5.9 ...
    ##  $ Cholesterolmgdl                            : logi  NA NA NA NA NA NA ...
    ##  $ HDLCholesterolmmolL                        : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ HDLCholesterolmgdl                         : logi  NA NA NA NA NA NA ...
    ##  $ LDLCholesterolmmolL                        : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ LDLCholesterolmgdl                         : logi  NA NA NA NA NA NA ...
    ##  $ TriglyceridsmmolL                          : num  1.8 1.2 2.32 2.56 1.94 NA 5.14 2.9 1.34 2.42 ...
    ##  $ Triglyceridsmgdl                           : logi  NA NA NA NA NA NA ...
    ##  $ CReactiveProteinmgL                        : num  1 23 3.9 0 0 NA 3.6 2.2 10.1 5.4 ...
    ##  $ CReactiveProteinmgdl                       : logi  NA NA NA NA NA NA ...
    ##  $ Creatinine..molL                           : num  97 85 92 76 85 NA 120 77 66 93 ...
    ##  $ Creatininemgdl                             : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ HbA1c                                      : num  3.7 2.8 2.8 3.5 3.9 NA 3.9 3.8 3.7 4 ...
    ##  $ HbA1cmmolmol                               : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ UrinealbuminemgL                           : num  2.78 6.74 15.1 7.43 NA NA 16.5 13.1 6.39 7.75 ...
    ##  $ UrinealbumingL                             : logi  NA NA NA NA NA NA ...
    ##  $ FastingBloodSampleYes                      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ FastingBloodSampleNo                       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ GeneticAnalysisTakenYes                    : num  0 0 0 0 0 NA 0 0 0 0 ...
    ##  $ GeneticAnalysisTakenNo                     : num  1 1 1 1 1 NA 1 1 1 1 ...
    ##  $ Nochange                                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Unknown                                    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Changeofdiagnosis                          : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Nodiagnosis                                : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CVSystemicHypertension                     : num  0 0 0 0 0 1 1 1 0 0 ...
    ##  $ CVIschemicHeartDisease                     : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CVLeftVentricularHypertrophy               : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CVValvularHeartDisease                     : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CVTIAorstroke                              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ AtrialFibrillation                         : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ CVPulmonaryHypertension                    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CVStatusPostMyocardialInfarction           : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CVCardiacfailure                           : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CVOtherCerebrovascularDisease              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CVOtherYesNo                               : num  0 0 0 0 0 0 0 1 0 0 ...
    ##  $ CVOther                                    : chr  NA NA NA NA ...
    ##  $ MetabolicDiabetesNoninsulindependent       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ MetabolicHyperlipidemia                    : num  0 1 0 0 0 0 0 1 0 0 ...
    ##  $ MetabolicDiabetesInsulinDependent          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ MetabolicHyperuricemia                     : num  0 0 0 0 0 0 1 0 0 0 ...
    ##  $ MetabolicOtherYesNo                        : num  0 0 0 0 0 0 0 0 1 0 ...
    ##  $ MetabolicOther                             : chr  NA NA NA NA ...
    ##  $ SleepDisorderOSA                           : num  1 1 1 1 0 0 1 1 1 1 ...
    ##  $ SleepDisorderCSA                           : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SleepDisorderAlveolusHypoventilation       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SleepDisorderRLS                           : num  0 0 0 0 0 0 0 0 1 0 ...
    ##  $ SleepDisorderInsomnia                      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SleepDisorderNarcolepsyIdiopaticHypersomnia: num  0 0 0 0 0 0 0 0 0 0 ...
    ##   [list output truncated]
    ##  - attr(*, "problems")=Classes 'tbl_df', 'tbl' and 'data.frame': 191781 obs. of  5 variables:
    ##   ..$ row     : int  1002 1002 1002 1014 1025 1026 1076 1076 1076 1080 ...
    ##   ..$ col     : chr  "HDLCholesterolmgdl" "LDLCholesterolmgdl" "Triglyceridsmgdl" "UrinealbumingL" ...
    ##   ..$ expected: chr  "1/0/T/F/TRUE/FALSE" "1/0/T/F/TRUE/FALSE" "1/0/T/F/TRUE/FALSE" "1/0/T/F/TRUE/FALSE" ...
    ##   ..$ actual  : chr  "40" "80" "80" "25" ...
    ##   ..$ file    : chr  "'ESADA.csv'" "'ESADA.csv'" "'ESADA.csv'" "'ESADA.csv'" ...

*Exploring variable names*

``` r
names(Data1)
```

\*Generates 215 variables (working DATASET)

*Checking for duplicates*

``` r
sum(duplicated(Data1$PatientID))
```

    ## [1] 0

*Some summary statistics*

``` r
table(Data1$Gender)
```

    ## 
    ## Female   Male 
    ##   7594  17904

``` r
summary(Data1$Age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   18.00   44.00   54.00   52.71   62.00   94.00      11

``` r
summary(Data1$BMI)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00   26.96   30.47   31.52   34.95   86.86     233

*Correcting "BMI" error by removing "0" values*par()

``` r
Data1$BMI[is.na(Data1$BMI) | Data1$BMI==0] <- NA
```

``` r
summary(Data1$MeanBloodPressure)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   55.67   91.00   98.33   99.02  106.67  160.33    1279

``` r
summary(Data1$HeartRate)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   32.00   64.00   72.00   73.47   80.00  134.00    4287

``` r
summary(Data1$WHR)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  0.5773  0.9216  0.9792  0.9753  1.0300  1.5495    2533

### Grouping co-morbidities

*CVD*

``` r
Data1$CVD <- ifelse(Data1$"CVSystemicHypertension"==1 |
                      Data1$"CVIschemicHeartDisease"==1 |
                      Data1$"CVLeftVentricularHypertrophy"==1 |
                      Data1$"CVValvularHeartDisease"==1 |
                      Data1$"CVTIAorstroke"==1|"AtrialFibrillation"==1 |
                      Data1$"CVPulmonaryHypertension"==1 |
                      Data1$"CVStatusPostMyocardialInfarction"==1 | 
                      Data1$"CVCardiacfailure"==1 |
                      Data1$"CVOtherCerebrovascularDisease"==1 | 
                      Data1$"CVOtherYesNo"==1, 1, 0) 
```

*Metabolic*

``` r
Data1$Metabolic <- ifelse(Data1$"MetabolicDiabetesNoninsulindependent"==1 |
                            Data1$"MetabolicDiabetesInsulinDependent"==1 |
                            Data1$"MetabolicHyperlipidemia"==1 |  
                            Data1$"MetabolicHyperuricemia"==1 | 
                            Data1$"MetabolicOtherYesNo"==1, 1, 0) 
```

*Sleep*

``` r
Data1$Sleep <- ifelse(Data1$"SleepDisorderOSA"==1 | 
                        Data1$"SleepDisorderCSA"==1 | 
                        Data1$"SleepDisorderAlveolusHypoventilation"==1 | 
                        Data1$"SleepDisorderRLS"==1 | 
                        Data1$"SleepDisorderInsomnia"==1 |
                        Data1$"SleepDisorderNarcolepsyIdiopaticHypersomnia"==1 | 
                        Data1$"SleepDisorderOtherYesNo"==1, 1, 0) 
```

*Pulmonary*

``` r
Data1$Pulmonary <- ifelse(Data1$"RestrictivePulmonaryDisease"==1 |
                            Data1$"PulmonaryRespiratoryfailure"==1 |
                            Data1$"PulmonaryCOPD"==1 |
                            Data1$"PulmonaryAsthma"==1 |
                            Data1$"PulmonaryOtherYesNo"==1, 1, 0)  
```

*Multi-morbidities*

``` r
Data1$CVD[is.na(Data1$CVD)] <- 0
Data1$Metabolic[is.na(Data1$Metabolic)] <- 0
Data1$Sleep[is.na(Data1$Sleep)] <- 0
Data1$Pulmonary[is.na(Data1$Pulmonary)] <- 0
Data1$OtherNeurologicalDisease[is.na(Data1$OtherNeurologicalDisease)] <- 0
Data1$OtherPsychiatricDisease[is.na(Data1$OtherPsychiatricDisease)] <- 0
Data1$OtherInflammatoryDisease[is.na(Data1$OtherInflammatoryDisease)] <- 0
Data1$OtherGastrointestinalDisease[is.na(Data1$OtherGastrointestinalDisease)] <- 0
Data1$OtherMalignantDisease[is.na(Data1$OtherMalignantDisease)] <- 0
Data1$OtherYesNo[is.na(Data1$OtherYesNo)] <- 0
```

*10 groups: 'CVD, Metabolic, Sleep, Pulmonary, Neurological, Psychiatric, Inflammatory, GI, Malignant, Other'*

``` r
Data1$Multi_morb <- Data1$CVD+
  Data1$Metabolic+ 
  Data1$Sleep+ 
  Data1$Pulmonary+ 
  Data1$OtherNeurologicalDisease+
  Data1$OtherPsychiatricDisease+
  Data1$OtherInflammatoryDisease+
  Data1$OtherGastrointestinalDisease+
  Data1$OtherMalignantDisease+
  Data1$OtherYesNo
```

*Number of patients with 0-10 co-morbidities*

``` r
table(Data1$Multi_morb)
```

    ## 
    ##    0    1    2    3    4    5    6    7    8    9   10 
    ## 2087 6406 7414 5797 2615  870  237   62    9    1    1

*Total number of patietns with 2 or more co-morbities*

``` r
nrow(Data1[Data1$Multi_morb>=2, ])
```

    ## [1] 17006

*Sample percentage*

``` r
17006/25499*100
```

    ## [1] 66.69281

*Rearranging variables in dataset: bringing comorbidity columns up front*

``` r
Data2 <- Data1 %>% select(1:21, 76:141, 215:220, everything())
```

``` r
write.csv(Data2, file = "Data2.csv")
```

``` r
names(Data2 [1:93])
```

*CV comorbidities*

``` r
names (Data2 [22:33])
```

    ##  [1] "CVSystemicHypertension"           "CVIschemicHeartDisease"          
    ##  [3] "CVLeftVentricularHypertrophy"     "CVValvularHeartDisease"          
    ##  [5] "CVTIAorstroke"                    "AtrialFibrillation"              
    ##  [7] "CVPulmonaryHypertension"          "CVStatusPostMyocardialInfarction"
    ##  [9] "CVCardiacfailure"                 "CVOtherCerebrovascularDisease"   
    ## [11] "CVOtherYesNo"                     "CVOther"

*Systemic Hypertension*

``` r
sum(Data2$CVSystemicHypertension, na.rm=T)
```

    ## [1] 10602

*Ischemic Heart Disease*

``` r
sum(Data2$CVIschemicHeartDisease, na.rm=T)
```

    ## [1] 2064

*Left Ventricular Hypertrophy*

``` r
sum(Data2$CVLeftVentricularHypertrophy, na.rm=T)
```

    ## [1] 328

*Valvular Heart Disease*

``` r
sum(Data2$CVValvularHeartDisease, na.rm=T)
```

    ## [1] 320

*Transient Ischemic Attack (TIA) or stroke*

``` r
sum(Data2$CVTIAorstroke, na.rm=T)
```

    ## [1] 514

*Atrial Fibrillation*

``` r
sum(Data2$AtrialFibrillation, na.rm=T)
```

    ## [1] 915

*Pulmonary Hypertension*

``` r
sum(Data2$CVPulmonaryHypertension, na.rm=T)
```

    ## [1] 123

*Status Post Myocardial Infarction*

``` r
sum(Data2$CVStatusPostMyocardialInfarction, na.rm=T)
```

    ## [1] 534

*Cardiac Failure*

``` r
sum(Data2$CVCardiacfailure, na.rm=T)
```

    ## [1] 605

*Other Cerebrovascular Disease*

``` r
sum(Data2$CVOtherCerebrovascularDisease, na.rm=T)
```

    ## [1] 212

*CV Other YesNo*

``` r
sum(Data2$CVOtherYesNo, na.rm=T)
```

    ## [1] 1670

``` r
CVD <- c(10602, 2064, 328, 320, 514, 915, 123, 534, 605, 212, 1670)
par(mar=c(11.1,6.1,4.1,2.1))
barplot(CVD, names.arg=c("Systemic Hypertension", "Ischemic Heart Disease", "Left Ventricular Hypertrophy", "Valvular Heart Disease", "TIA or Stroke", "Atrial Fibrillation", "Pulmonary Hypertension", "Status Post Myocardial Infarction", "Cardiac Failure", "Other CVD", "Other"), cex.names = 0.8, ylim= c(0,11000), las=2, main="CVD")
```

![](https://github.com/nikolaid/ESADA/blob/master/unnamed-chunk-47-1.png)

#### Variable names to ICD10 codes

Systemic Hypertension = Essential (primary) hypertension = **I10**

Ischemic Heart Disease = Chronic ischemic heart disease = **I25**

Left Ventricular Hypertrophy=Cardiomegaly = **I51.7** (**I51**, Complications and ill-defined descriptions of heart diseae)

Valvular Heart Disease = Nonrheumatic aortic valve disorder, unspecified = **I35.9** (**I35** = Nonrheumatic aortic valve disorders)

TIA or Stroke = Transient cerebral ischemic attack, unspecified = **G45.9** AND/OR Cerebral infarction, unspecified = **I63.9** (**I63** = Cerebral infarction)

Atrial Fibrillation = Unspecified atrial fibrillation = **I48.91** (**I48** = Atrial fibrillation and flutter)

Pulmonary Hypertension = Pulmonary hypertension, unspecified = **I27.20** (**I27** = Other pulmonary heart diseases)

Status Post Myocardial Infarction = Other current complications following acute myocardial infarction = **I23.8** (**I23** = Certain current complications following acute myocardial infarction)

Cardiac Failure = Heart Failure = **I50**

##### Looking at the CVOther (free text)

``` r
unique(names(which(sapply(Data2$CVOther, function(x) any(!is.na(x))))))
```

##### This returns N=809 entries BUT there are many spelling errors

``` r
unique(grep("^ar", Data2$CVOther, value=T, ignore.case=T))
```

    ##  [1] "arrhytmua"                                    
    ##  [2] "arrhytmia"                                    
    ##  [3] "arrhytmogenic kardiomyopathy"                 
    ##  [4] "Arrhytmia"                                    
    ##  [5] "Arrhytmia unspecified"                        
    ##  [6] "Arrhytmia unspecified, pacemaker 9/2017"      
    ##  [7] "Arrhytmia unspec."                            
    ##  [8] "Arytmie"                                      
    ##  [9] "ARYTMIE"                                      
    ## [10] "arrythmia"                                    
    ## [11] "Arrhythmia"                                   
    ## [12] "arrhythmia"                                   
    ## [13] "arythmia"                                     
    ## [14] "Arrythmia"                                    
    ## [15] "ARYTHMIES"                                    
    ## [16] "ARYTHMIES,CARTIOUS STRENGTH"                  
    ## [17] "arythmie"                                     
    ## [18] "arrhythmia, pacemaker"                        
    ## [19] "arrhythmia, PTCI, stenting"                   
    ## [20] "arrhythmia, EKV, pacemaker, stenting, CABG"   
    ## [21] "artial fibrillation"                          
    ## [22] "arial fibrillation"                           
    ## [23] "arrhythmia, congenital lymphedema"            
    ## [24] "arrhythmia, stenting"                         
    ## [25] "arrhythmia, ablation"                         
    ## [26] "arrhythmia, ablation,"                        
    ## [27] "arrhythmia, pulmonary embolisation"           
    ## [28] "arrhythmia, electrical kardioversion"         
    ## [29] "arrhythmia, pericardial tamponade"            
    ## [30] "arrhythmia, CABG, phlebothrombosis"           
    ## [31] "arrhythmia, CABG"                             
    ## [32] "arrhythmia, PCI, abdominal aortic aneurysm"   
    ## [33] "arrhythmia, open foramen ovale"               
    ## [34] "Arterial stent, Stenosis carotidienne"        
    ## [35] "arteripathy"                                  
    ## [36] "arteriopathy"                                 
    ## [37] "ARYTHMIA"                                     
    ## [38] "ARTHERIOPATHIE"                               
    ## [39] "ARYTHMIA, ANEVRISME ARTERE ILLIAQUE"          
    ## [40] "ARYTHMIA, RENAUD SYNDROME, VERTIGES"          
    ## [41] "ARYTHMIA ARTHERIOPAHIE"                       
    ## [42] "ARTERIOPATHIE"                                
    ## [43] "arritmias"                                    
    ## [44] "arytmia"                                      
    ## [45] "Arrhythmias (treated by cardiac PM)"          
    ## [46] "arrhythmias"                                  
    ## [47] "arrhythmia, aortic aneurysm"                  
    ## [48] "arrhythmias (pace-maker implanted)"           
    ## [49] "arrhythmia (pace-maker)"                      
    ## [50] "arrhythmias (nocturnal AV 2nd degree blocks)" 
    ## [51] "arrhytmias"                                   
    ## [52] "arrhytmias, aortic aneurysm"                  
    ## [53] "Arrhythmias"                                  
    ## [54] "Arytmia"                                      
    ## [55] "Arythmia AV blockade of 1st and 2nd degree"   
    ## [56] "Aryithmia"                                    
    ## [57] "artrial fibrialtion"                          
    ## [58] "arterial fibrilation"                         
    ## [59] "artrial fibrillation"                         
    ## [60] "Arrhythmia, Pacemaker"                        
    ## [61] "Arythmia"                                     
    ## [62] "Arrytmia"                                     
    ## [63] "Arytmi"                                       
    ## [64] "arrythmy"                                     
    ## [65] "arythmia, pulmonary embolism"                 
    ## [66] "arythmia  -AF"                                
    ## [67] "arythmia AF"                                  
    ## [68] "arrythmic disease"                            
    ## [69] "artiosclerosis"                               
    ## [70] "arterial hypertension"                        
    ## [71] "arterial hypertension; coronary heart disease"
    ## [72] "arrithmia"                                    
    ## [73] "arrithmya"                                    
    ## [74] "arryhmia"                                     
    ## [75] "Arrithmya"

##### The goal is to unify all free text containing the word "arrhythmia" in any possible spelling.

Then the ICD-10 code **I49.9** "Cardiac arrhythmia, unspecified" can be assigned (**I49** Other cardiac arrhythmias)

``` r
Data2$CVOther <- gsub("arrhytmua|arrhytmia|arrhytmogenic kardiomyopathy|Arrhytmia|Arrhytmia unspecified|Arrhytmia unspec.|Arytmie|ARYTMIE|arrythmia|Arrhythmia|ARYTHMIES|arythmia|Arrythmia|arythmie|
ARYTHMIA|ARYTHMIA|arritmias|arytmia|arrhythmias|Arytmia|Aryithmia|Arythmia|Arrytmia|Arytmi|arrythmy|arrythmic disease|arrithmya|arryhmia|Arrithmya|arrithmia|heart arrhythmia|heart rhythm disorder|cardiac arrhythmia|cardiac arythmitic|rythme disorder|rythm disorder|rythme disorder|heart ryhtm disorder|^heart arrhythmia$|hEART ARYTMIA|tachy-brady arrhythmia|Cardiac dysrhythmia|atrie arrytmia|Heathrythm disturbance|atythmia|absolute[[:blank:]]arrhythmia|Heart Arritmia|
Cardiac arrhythmias|s/p[[:blank:]]arrhythmia|irregular heartbeat|sometimes arrhythmia|Cardiac arrhythmia|\\<arrhythias\\>|Supraventricular arrhythmia|cardiac dysrhythmia|
disorder of cardiac rythm|heart rhythm disturbance|HEART INACCURACY","arrhythmia",Data2$CVOther)
```

##### The same for "atrial fibrillation" in any possible spelling.

ICD-10 code **I48.91** "Unspecified atrial fibrillation" (**I48** = Atrial fibrillation and flutter)

``` r
Data2$CVOther <- gsub("^chronic  atrial fibrilation$|^paroxysmal fibrilation$|^Atrial fibrilation$|^atrial fibrilation$| ^atrial fibrilation paroxysmal$|^Persistent atrial fibrilation$|^Persisting atrial fibrilation$|^Atrial fibrilation permanent$|^Paroxysmal atrial fibrilation$|^paroxsysmal atrial fibrillation$|^Atrial Fibrillation$|^atria Fibrillation$|^arial fibrillation$|^atrial fibrilloflutter$|^atrial fibrillation and flutter$|^paroxysmal atrial fibrillation$|^atrial fibrilation paroxysmal$|^chronic atrial fibrillation$|^FA, not chronic$|^chronic atrial fibriclation$|^atrial fifralation$|^Atrial fibrillation$|^Fibrillatio atriorum$|^non-chronic atrial fibrillation$|^Auricular Fibrilhation$|^PAROSSISTIC ATRIAL FIBRILLATION$|^Atrial fibrillation -paroxysmal$|^artrial fibrialtion$|^arterial fibrilation$|^atrila fibrillation$|^atrial fibrillation paroxysmal$|^artrial fibrillation$|^Intermittent Atrial fibrillation$|^Paroxysmal Atrial fibrillation$|^Paroxystisk AF$|^fibrillatio atriorum$|^fibryllatio atriorum$|^Fibryllatio atriorum$|^Intermittent atrial fibrillation night timei$|^atrial fibrylation$|^atrial fibration$|^Fibrillatio atriorum$|^atrial fibrilliation$|^intermittent atrial fibrillation$|^atrial fibrilhation$|^artial fibrillation$|^Atrial fibrilation$|^Atrial fibrillation$|^Atrial Fibrilation$|^ATRIAL FIBRILLATION$|^a-fib$|^paroxysmal atrial fibrillation$|^paroxysmal a-fib$|^A-FIB$|^Paroxysmal Atrial Fibrillation$|^parox. a fib$|^paroxysmal atrial fib$|^atrial fib$|^Atrial fibrilation$|^atrial fibrilation$|^Intermittent fibrillation$|^a fib$|^A fib$|^Atrial fibrilation$|^Atrial fibrillation$|^Patrial fibrillation$|^atrieflimme$|^Atrieflimmer$|^atrieflimmer$|^chr. flimmer$|^paroxymal flimmer$|^paroks. atrial fibrallation$|^parogsymal flimmer$|^paroksymal flimmer$|^Fibrillatio atriorum$|^Atrial fibrillation,$|^atrial fibrallation$|^paroxysmal atrial fibrillation$|^parossitic FA$|^paroxysmal atrium fibriliation$|^Paroxysmal atrial fibrillation$|^flimmeri$|^Persister.atrieflimmer$|^ flimmeri$^Persisting atrial fibrilation, st.p.ECV 5/2017$|^atrial fibrallation: pacemaker$|^ACFA$|^atrieflimmer,opr aortainsuff$|^Persistent atrial fibrilation, Pacemaker 2017$|^AF,flutter, trombe i u.ex$", "atrial fibrillation", Data2$CVOther)
```

ICD-10 code **I48.92** "Unspecified atrial flutter" (**I48** = Atrial fibrillation and flutter)

``` r
Data2$CVOther <- gsub("^atrial flutter-paroxysmal$|^atrial fibrilloflutter$|^Atrial flutter$|^Paroxysmal Atrial Flutter,$|^atrial flutter-paroxysmal,$|^Atrial flutter Pacemaker$|^Atrial flutter 2:1$","atrial flutter", Data2$CVOther)
```

ICD-10 code **Z95.0** "Presence of cardiac pacemaker"

``` r
Data2$CVOther <- gsub("^pacemaker$|^paacemaker$|^PACEMAKER$|^Pcemaker$|^Pacemaker$|^PM due to sick sinus$|^Pacemaker due to bradycardia$|^pace maker$|^pacemaker for heartblock$|^pace maker, a-fib$|^Pacemaker since 1986$|^PACEMAKER,ANEURYSM$|^Pacemaker 2017$|^cardiac pacemaker$|^treated by cardiac PM$|^pace-maker implanted$|^pacemaker 9/2017$|pace-maker|Pacemaker 05/2016$","pacemaker", Data2$CVOther)
```

ICD-10 code **R00.0** "Tachycardia, unspecified"

``` r
Data2$CVOther <- gsub("^Tachycardie$|^Tachycardia$|^Tachykardi$|^Taquicardia$|^Takykardia$|^takykardia$|^sinus tachycardia$|^Sinus tachycardia$","tachycardia unspecified", Data2$CVOther)
```

ICD-10 code **R00.1** "Bradycardia, unspecified"

``` r
Data2$CVOther <- gsub("^sinus bradykardia$|^Bradycardia$|^Nocturnal Bradycardia$|^sinus bradycardia$","bradykardia", Data2$CVOther)
```

ICD-10 code **R00.2** "Palpitations"

``` r
Data2$CVOther <- gsub("^Palpitations$|^Palpatations$|^Intermittent palpitation$","palpitations", Data2$CVOther)
```

ICD-10 code **R22.4** "Localized swelling, mass and lump, unspecified lower limb"

``` r
Data2$CVOther <- gsub("^Leg swelling$|^Leg oedema$","leg swelling", Data2$CVOther)
```

ICD-10 code **I73.9** "Peripheral vascular disease, unspecified"

``` r
Data2$CVOther <- gsub("^PVD$|^Peripheral Vascular disease$|^Peripheral vascular disease$|^Peripheral Vascular Disease$|^peripheral arterial occlusion disease$|^peripheral arterial disease$|^Peripheral arterial disease$|^Peripheral arterial dissease$|^peripheral artery disease$|^Limb ischemia$|^Limb ischemia$|^aorto bifemoral bypass$|ARTHERIOPAHIE|ARTERIOPATHIE|^PTCA A. femoralis$|^A. femoralis thrombosis$|^atherosclerosis of the legs$","peripheral vascular disease", Data2$CVOther)
```

ICD-10 code **Z86.71** "Personal history of venous thrombosis and embolism"

``` r
Data2$CVOther <- gsub("^P.E$|^pulmonary thromboembolism$|^pulmonal emboli$|^Pulm emboli$|^Pulmonary embolus$|^Pulmonary embolism$|^pulmonary embolus$|^Pulmonary embolus$|^pulmonary embolism 2008$|^deep venous trombosis, trombophilia$|^Deep vein trombosis recidiving$|deep[[:blank:]]venous[[:blank:]]trombosis|^Deep vein thrombosis$|^Thromboembolism$|^St.p.flebotrombosis 2016$|^DEEP VEIN THROMBOSIS$|^DVT$|^DVT 2013$|^status post flebothrombosis$|^deep venous trombosis$|S.p.lung embolia[[:blank:]]2014|^St.p.DVT 2011$|^trombosis dk$|^status post phlebothrombosis of the lower limbs$|^St.p. trombosis of left lower limb$|^st.p. pulmonary embolism$|^St.p.DVT 2013$|^St.p. lung embolism 2013 from FP trombosis$|postrombotic[[:blank:]]syndrom|posttrombotic[[:blank:]]syndrom|^St.p. lung embolism 1983$|^St.p.PE 2014, St.p.flebotrombosis$|^vein thrombosis$|^Thrombosis$|^embolia arteriae pulmonalis$|^pulmonary embolism in 2007. I degree AV block$|^St.p. lung embolism 1999, 2004$|^Post VT$|^Venous thrombosis$|^thrombosis$|^pulmonary embolism$","pe-dvt", Data2$CVOther)
```

ICD-10 code **I45.10** "Unspecified right bundle-branch block"

``` r
Data2$CVOther <- gsub("^RBBB, AVB I.$|^RBBB$|^iRBBB$|^AV block I.degree, iRBBB$|^RBBB, sinus bradycardia$|^iRBBB, AVB I.$","RBBB", Data2$CVOther)
```

ICD-10 code **I44.7** "Left bundle-branch block, unspecified"

``` r
Data2$CVOther <- gsub("^LBBB, iRBBB$|^LBBB,St.p.myokarditis 2008$","LBBB", Data2$CVOther)
```

ICD-10 code **I47.0** "Re-entry ventricular arrhythmia" (**I47** = Paroxysmal tachycardia)

``` r
Data2$CVOther <- gsub("^re-entry tachycardia$","re-entry ventricular tachycardia", Data2$CVOther)
```

ICD-10 code **I47.1** "Supraventricular tachycardia" (**I47** = Paroxysmal tachycardia)

``` r
Data2$CVOther <- gsub("^supra ventricular tachycardia$|^Supraventricular arrhythmia$|^Paroxysmal atriventricular tachycardia$|SVES+VES|SVES|VES|SVES/VES","supraventricular tachycardia",Data2$CVOther)
```

ICD-10 code **I47.2** "Ventricular tachycardia" (**I47** = Paroxysmal tachycardia)

``` r
Data2$CVOther <- gsub("^Ventricular tachycardia$|^paroxsystisk ventrikulær takykardi$|^Tidl.rytmeforstyrrelse$","ventricular tachycardia", Data2$CVOther)
```

ICD-10 code **I47.9** "Paroxysmal tachycardia, unspecified" (**I47** = Paroxysmal tachycardia)

``` r
Data2$CVOther <- gsub("^tachycardia of bouveret$|^Bouverte syndrome$|^periodic tachy$|^Tachycardia intermittent$","paroxysmal tachycardia", Data2$CVOther)
```

ICD-10 code **I49.3** "Ventricular premature depolarization (**I49** = Other cardiac arrhythmias)

``` r
Data2$CVOther <- gsub("Ventricular[[:blank:]]extrasystolia|^exstrasystolia$|^extra systolia$|ventricular[[:blank:]]extrasystolia|^Ekstra systoler$|^ekstrasystoler$|^ekstrasystoles$|isol.ventricular[[:blank:]]extrasystoles|^ventricular extrasystolia$","ventricular premature depolarization", Data2$CVOther)
```

ICD-10 code **I49.5** "Sick sinus syndrome" (**I49** = Other cardiac arrhythmias)

``` r
Data2$CVOther <- gsub("^tachy-brady arrhytmia$|^tachycardia-bradycardia syndrome$|Sick[[:blank:]]sinus[[:blank:]]syndrom|tachycardia-bradycardia syndrome[[:blank:]]-[[:blank:]]VVI$|^Sick Sinus syndrome$","sick sinus syndrome", Data2$CVOther)
```

ICD-10 code **I44.0** "Atrioventricular block, first degree"

``` r
Data2$CVOther <- gsub("AVB[[:blank:]]I.degree$|^Type[[:blank:]]1[[:blank:]]AV[[:blank:]]block|AVB[[:blank:]]I.|AVBlock[[:blank:]]I.degree|AV[[:blank:]]block[[:blank:]]I.degree|I[[:blank:]]degr.[[:blank:]]A-V[[:blank:]]block|I[[:blank:]]degreevAV[[:blank:]]block","AV block I", Data2$CVOther)
```

ICD-10 code **I44.1** "Atrioventricular block, second degree"

``` r
Data2$CVOther <- gsub("^AVB II.degree$|^AVB II.degree$|^AVB II Wenkebach$|nocturnal AV[[:blank:]]2nd[[:blank:]]degree[[:blank:]]blocks|^II grade AV. block:pacemaker$|^AV block 2 , pacemaker$|AV[[:blank:]]blockade[[:blank:]]of[[:blank:]]1st[[:blank:]]and[[:blank:]]2nd[[:blank:]]degree$","AV block II", Data2$CVOther)
```

ICD-10 code **I44.2** "Atrioventricular block, complete"

``` r
Data2$CVOther <- gsub("^AVB III.degree$|^AVB III$|^AV-block III degener$|^III degree AV block$|AV[[:blank:]block[[:blank:]]3|AV blocks III$|AV-block[[:blank:]]3[[:blank:]]+PM|^AVB III, impl.PM 2018$","AV block III", Data2$CVOther)
```

ICD-10 code **I44.3** "Unspecified atrioventricular block"

``` r
Data2$CVOther <- gsub("^AVB unspec.$|^AV block$|AV-block","AV block unspecified", Data2$CVOther)
```

ICD-10 code **I83.2** "Varicose veins of unspecified lower extremity with both ulcer and inflammation"

``` r
Data2$CVOther <- gsub("^Leg varices$|^Venous leg ulcers$|Leg[[:blank:]]varices[[:blank:]]and [[:blank:]]ulcers|Venous[[:blank:]]ulceration|Venous[[:blank:]]ulcers|Varices[[:blank:]]cruris [[:blank:]]bilat.|Varices[[:blank:]]cruris|^varices cruris$|^aricose veins bilat$|varices|^varicose vein$|^varices surgery$|^varices cruris surgery$|^varicose vein surgery$|^varices operation$|^varices,varicose ulcer$|^Leg varicose veins$|^Leg varicose veins and ulcers$|^leg varicose veins$|^varicous veins$","varicose veins", Data2$CVOther)
```

ICD-10 code **I87.2** "Venous insufficiency (chronic) (peripheral)"

``` r
Data2$CVOther <- gsub("Chronic[[:blank:]]vein[[:blank:]]insuficiency|^chronic venous insuffitienci$|^chronic venous isufficienci$|^chronic venous insuficienci$|^chronic venous insuficiency of the lower limbs$|^chronic venous insuficiency of lower limbs$|^chronic venous insufficiency of the lower limbs$|^chronic venous insufficiency,$|^chronic venous insufficiency$|^venous disease,$","venous insufficiency", Data2$CVOther)
```

ICD-10 code **I80.3** "Phlebitis and thrombophlebitis of lower extremities, unspecified"

``` r
Data2$CVOther <- gsub("^Vaskulitis$|^venous disease of the lower limbs$|^Recid.flebotrombosis$|^phlebitis$|phlebothrombosis|^Phlebitis$|^St.p.flebotrombosis$|^Plhlebite$|St.p.tromboflbitis[[:blank:]]LL|^phlebotrombosis of left limb$|^phlebite$|^flebotromboza$","thrombophlebitis", Data2$CVOther)
```

ICD-10 code **Q21.1** "Atrial septal defect" (Persisting foramen ovale)

``` r
Data2$CVOther <- gsub("^Persisting foramen ovale$|^open foramen ovale$|^Open foramen ovale$|^persistent foramen ovale$|^ASD II$|^Persistent foramen ovale$","atrial septal defect", Data2$CVOther)
```

ICD-10 code **I45.6** "Pre-excitation syndrome"

``` r
Data2$CVOther <- gsub("^WPW-SDR$|^Wolff-Parkinson-White syndrome$|^Wolff-Parkinson-White$|^WPW syndrome$|^Wolf parkingson white$","pre-excitation syndrome", Data2$CVOther)
```

ICD-10 code **I71.00** "Dissection of unspecified site of aorta"

``` r
Data2$CVOther <- gsub("^aortal dissection type B$|^Aortic Dissection type B$|^Ruptured aneurysm$|^Aortic dissection$|^aortic dissection$|^AORTIC DISSECTION$|^surgically treated aorta aneurysm$|^aorta dissection$|^Aorta dissection/rupture$|^dissection$|^Aortadissection 2012$","dissection of aorta unspecified", Data2$CVOther)
```

ICD-10 code **I71.2** Thoracic aortic aneurysm, without rupture

``` r
Data2$CVOther <- gsub("^Ascendent aortic aneurysm$|^ANEURYSM OF ASCENDING AORTA$|^ANEURYSM OF THRACIC AORTA$","thoracic aortic aneurysm", Data2$CVOther)
```

ICD-10 code **I71.4** "Abdominal aortic aneurysm, without rupture"

``` r
Data2$CVOther <- gsub("^abdominal aortic aneurysm$|^aneurysm of the abdominal aorta$|^Descending aorta$","abdominal aortic aneurysm", Data2$CVOther)
```

ICD-10 code **I71.9** "Aortic aneurysm of unspecified site, without rupture"

``` r
Data2$CVOther <- gsub("^aortic aneurism$|^Anévrysme of the aorta$|^aneurism$|^Aorta aneurysm$|^Aortic aneurysm$|^aorta-aneurysm$","aortic aneurysm unspecified", Data2$CVOther)
```

ICD-10 code **I51.9** "Heart disease, unspecified"

``` r
Data2$CVOther <- gsub("^Heart disease$|^heart disease$|^Heart problems$|^heart oroblems$|heart Poroblems$|^heart problems$|^cardiac disease$|^HEART PROBLEMS$|^Heartb disease$","heart disease", Data2$CVOther)                                       
```

ICD-10 code **I42.9** "Cardiomyopathy, unspecified"

``` r
Data2$CVOther <- gsub("^Kardiomyopati$|^cardiomyopati$|^cardiomiopatia$","cardiomyopathy unspecified", Data2$CVOther)                                       
```

ICD-10 code **I42.0** "Dilated cardiomyopathy"

``` r
Data2$CVOther <- gsub("^dilated cardiomyopathy$|^left atrial dillatation$|^Dilatert kardiomyopati$|^Dilated cardiomyopathy$","cardiomyopathy unspecified", Data2$CVOther)                                 
```

ICD-10 code **I42.2** "Other hypertrophic cardiomyopathy"

``` r
Data2$CVOther <- gsub("^Hypertrophic myocardiopati$|^hypertrophic cardiomyopathy$|^heart muscle hypertrophy$","hypertrophic cardiomyopathy", Data2$CVOther)                           
```

ICD-10 code **I51.81** "Takotsubo syndrome"

``` r
Data2$CVOther <- gsub("^Takotsubo cardiomyopathy$|^Takotsubocardiomyopati$","takotsubo syndrome", Data2$CVOther)                           
```

ICD-10 code **I11.9** "Hypertensive heart disease without heart failure"

``` r
Data2$CVOther <- gsub("hypertensive cardiomyopath", "hypertensive heart disease", Data2$CVOther)  
```

ICD-10 code **I25.1** "Atherosclerotic heart disease of native coronary artery"

``` r
Data2$CVOther <- gsub("^CABG$|^Coronary Artery Bypass Graft$|^bypass$|triple bypass|^coronary artery bypass$|^Coronary bypass$|^coronary bypass$|^coronary stents$|^bypass surgery$|^coronary artery bypass graft$|^By pass surgery-74$|^By pass surgery$|^Aorta by pass surgery$|^Bypass surgery 2007$|^Op bypass$|^coronary stents$|^ACB surgery$|^ACB surgery 2010$|^stent opr$|^Coronar stent$|^stenting, coronary artery bypass graft$|^CABG, stenting$|^stenting$|^coronair stenose$|^INSUFISANCE CARDIAQUE AND CORONAIRE$|^INSUFFISANCE CORONAIRE$|^Coronopathie$|^coronary heart disease$|^atherosclerotic coronary disease$|^coronary failure$|^stunt-opr.$|^coronary insufficiency$|^cardio artery stenosis$|^coronary heartdisease$|^PCI$|^PTCA$|^ACB + stent opr$|^St.p.2xCABG$|^St.p.2xCABG 2014$|^Coronary artey desease surgery$", "coronary artery disease", Data2$CVOther)  
```

-   This code also includes coronary artery bypass graft (CABG), coronary stents

ICD-10 code **I27.81** "Cor pulmonale (chronic)"

``` r
Data2$CVOther <- gsub("^Cor pulmonale$","cor pulmonale", Data2$CVOther)                       
```

ICD-10 code **Z86.79** "Personal history of other diseases of the circulatory system"

``` r
Data2$CVOther <- gsub("^ablation$|^st.p.catether ablation, unspecified$|^AVNRT, st.p. RF ablation$|^st.p.RF ablation$","other circulatory", Data2$CVOther)                       
```

ICD-10 code **R01.1** "Cardiac murmur, unspecified"

``` r
Data2$CVOther <- gsub("^murmur$|^murmer$|^Systolic murmur$|^Holosystolic murmur$","cardiac murmur", Data2$CVOther)                   
```

ICD-10 code **I73.0** "Raynaud's syndrome"

``` r
Data2$CVOther <- gsub("^Raynaud disease$","Raynaud", Data2$CVOther)                   
```

ICD-10 code **I70.219** "Atherosclerosis of native arteries of extremities with intermittent claudication, unspecified extremity"

``` r
Data2$CVOther <- gsub("^Claudicatio$|^Intermittent claudication$","intermittent claudication", Data2$CVOther)
```

ICD-10 code **K64.9** "Unspecified hemorrhoids"

``` r
Data2$CVOther <- gsub("^Hemorrhoides$|^Haemmorhoids$|^Haemorrhoides$|^hemorrhoids$","hemorrhoids", Data2$CVOther)
```

ICD-10 code **I20.9** "Angina pectoris, unspecified"

``` r
Data2$CVOther <- gsub("^Angina pectoris$|^Angina Pectoris$|^angina pectoris stabilis$|^Angina ,utredast$|^Angina?$","angina unspecified", Data2$CVOther)
```

ICD-10 code **I20.0** "Instable Angina pectoris"

``` r
Data2$CVOther <- gsub("^Unstable angina$","instable angina", Data2$CVOther)
```

ICD-10 code **R55** "Syncope and collapse"

``` r
Data2$CVOther <- gsub("^vasovagal syndrome$","syncope", Data2$CVOther)
```

ICD-10 code **Z95.810** "Presence of automatic (implantable) cardiac defibrillator"

``` r
Data2$CVOther <- gsub("^ICD$|^ICD from 2011$|^DEFIBRILLATOR$|^kardioverter defibrillator$|^ICD-CRT-D 7/2016$","defibrillator", Data2$CVOther)
```

<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 10px;}
</style>
##### Further cleaning done manually, ICD-10 codes assigned to CVOther

``` r
CV_ICD10 <- read.csv("CVOther_ICD10.csv", header=TRUE, sep=",")
str(CV_ICD10)
```

    ## 'data.frame':    25499 obs. of  1 variable:
    ##  $ CVOther_ICD10: Factor w/ 178 levels " Z95.5","025S0ZZ ",..: NA NA NA NA NA NA NA 127 NA NA ...

##### MetabolicOther

unique(names(which(sapply(Data2$MetabolicOther, function(x) any(!is.na(x)))))) N=604

##### PulmonaryOther

unique(names(which(sapply(Data2$PulmonaryOther, function(x) any(!is.na(x)))))) N=457

##### SleepDisorderOther

unique(names(which(sapply(Data2$SleepDisorderOther, function(x) any(!is.na(x)))))) N=373

##### Other

unique(names(which(sapply(Data2$Other, function(x) any(!is.na(x)))))) N=2561
