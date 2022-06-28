Objective and subjective experiences of childhood adversity: a
meta-analysis of their agreement and relationship with psychopathology
================
Emma Francis

### Overview

##### 1. Meta analysis of agreement between objective and subjective experiences of childhood adversity

**Note: Corresponding data: “Data-extraction_agreement_june2022”**

-   1.1 Derive correlations

-   1.2 Convert correlations to Fishers-z

-   1.3 Prepare to run meta-analysis of agreement

-   1.4 Descriptive statistics

-   1.5 Run meta-analysis of agreement

    -   1.5.1 Maltreatment - Fishers-Z
    -   1.5.2 Maltreatment - Kappa
    -   1.5.3 Bullying
    -   1.5.4 Neighbourhood

-   1.6 Forest plots for meta-analysis

    -   1.6.1 Maltreatment - Fishers-Z
    -   1.6.2 Bullying
    -   1.6.3 Neighbourhood

-   1.7 Publication bias

    -   1.7.1 Maltreatment - Fishers-Z
    -   1.7.2 Maltreatment - Kappa
    -   1.7.3 Bullying

-   1.8 Leave one out analysis

    -   1.8.1 Maltreatment - Kappa
    -   1.8.2 Maltreatment - Correlations
    -   1.8.3 Bullying

##### 2. Meta analysis of objective and subjective experiences of childhood adversity and their association with psychopathology

**Note: Corresponding data: “Data-extraction_june22”**

-   2.1 Examine data

-   2.2 Derive new variables

-   2.3 Convert effect sizes

    -   2.3.1 Prepare data
    -   2.3.2 Convert Cohen’s d to r
    -   2.3.3 Convert odds ratio to r
    -   2.3.4 Convert unstandardised betas to r
    -   2.3.5 Convert risk ratios to r
    -   2.3.6 Convert r to Fisher’s Z

-   2.4 Descriptive statistics

-   2.5 Do objective and subjective measures of childhood adversity
    independently predict psychopathology?

    -   2.5.1 Maltreatment
    -   2.5.2 Bullying
    -   2.5.3 Neighbourhood
    -   2.5.4 Comparing Estimates of Independent Meta-Analyses or
        Subgroups
    -   2.5.5 Wald-type test for confirmation

-   2.6 Forest plots for meta-analysis

    -   2.6.1 Subjective Maltreatment
    -   2.6.2 Objective Maltreatment
    -   2.6.3 Subjective Bullying
    -   2.6.4 Objective Bullying
    -   2.6.5 Subjective Neighbourhood
    -   2.6.6 Objective Neighbourhood
    -   2.6.7 Combined Subjective and Objective Maltreatment
    -   2.6.8 Combined Subjective and Objective Bullying
    -   2.6.9 Combined Subjective and Objective Neighbourhood

-   2.7 What moderates the independent associations between objective
    and subjective measure of childhood adversity and psychopathology?

    -   2.7.1 Combining moderation analyses for maltreatment & bullying
    -   2.7.2 Test for moderation by mental health outcome
    -   2.7.3 Test for moderation by informant of mental health outcome
    -   2.7.4 Test for moderation by study type
    -   2.7.5 Test for moderation by study quality
    -   2.7.6 Test for moderation by sex

-   2.8 Publication bias

    -   2.8.1 Maltreatment
    -   2.8.2 Bullying

-   2.9 Leave one out analysis

    -   2.9.1 Maltreatment
    -   2.9.2 Bullying

#### 1. Meta analysis of agreement between objective and subjective experiences of childhood adversity

##### 1.1 1.1 Read in data, load packages and derive correlations

``` r
library(readxl)
library(psych) 
library(polycor) 
```

    ## 
    ## Attaching package: 'polycor'

    ## The following object is masked from 'package:psych':
    ## 
    ##     polyserial

``` r
library(metafor) 
```

    ## Loading required package: Matrix

    ## Loading required package: metadat

    ## 
    ## Loading the 'metafor' package (version 3.4-0). For an
    ## introduction to the package please type: help(metafor)

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data <- data.frame(read_excel("/Users/emma/Desktop/Data-extraction_agreement_june2022.xlsx")) # Read dataset
str(data)
```

    ## 'data.frame':    63 obs. of  26 variables:
    ##  $ author                    : chr  "Danese & Widom," "Danese & Widom," "Danese & Widom," "Danese & Widom," ...
    ##  $ year                      : num  2020 2020 2020 2020 2013 ...
    ##  $ cohort                    : chr  "Widom Midwest cohort" "Widom Midwest cohort" "Widom Midwest cohort" "Widom Midwest cohort" ...
    ##  $ cohort_distinct           : chr  "Widom Midwest cohort" "Widom Midwest cohort" "Widom Midwest cohort" "Widom Midwest cohort" ...
    ##  $ percent_female            : num  48.7 48.7 48.7 48.7 48.3 ...
    ##  $ Age_Assessment_MH         : num  28.7 28.7 28.7 28.7 11.9 12.5 11.2 12.4 11.5 18 ...
    ##  $ age_self                  : num  28.7 28.7 28.7 28.7 11.9 12.5 11.2 12.4 11.5 14 ...
    ##  $ Obj_Exposure              : chr  "Maltreatment" "Physical_Abuse" "Sexual_Abuse" "Neglect" ...
    ##  $ Subj_Exposure             : chr  "Maltreatment" "Physical_Abuse" "Sexual_Abuse" "Neglect" ...
    ##  $ es_type                   : chr  "kappa" "kappa" "kappa" "kappa" ...
    ##  $ es_agree                  : num  0.254 0.0897 0.1678 0.3172 0.3153 ...
    ##  $ se_agree                  : num  0.0289 0.0284 0.0431 0.0285 0.0595 ...
    ##  $ n_total                   : num  687 1107 1122 910 687 ...
    ##  $ n_obj_subj                : num  492 82 52 262 37 NA NA NA 81 163 ...
    ##  $ n_obj_only                : num  173 25 42 264 33 NA NA NA 81 110 ...
    ##  $ n_subj_only               : num  252 527 241 122 79 NA NA NA 192 253 ...
    ##  $ n_none                    : num  262 555 839 524 575 NA NA NA 431 244 ...
    ##  $ Q_Exposed_Rep             : num  0 0 0 0 1 1 1 1 1 0 ...
    ##  $ Q_Control_Selec           : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Q_Exposure_Subj           : num  1 1 1 0 1 0 1 1 0 1 ...
    ##  $ Q_Exposure_comparison     : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Q_exposure_time_comparison: num  1 1 1 1 0 1 1 1 1 1 ...
    ##  $ Q_MH_Control              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Q_Confound                : num  1 1 1 1 0 0 0 0 0 1 ...
    ##  $ Q_Longitud                : num  0 0 0 0 0 0 0 0 0 1 ...
    ##  $ Notes                     : chr  NA NA NA NA ...

``` r
View(data)

# Derive variable for the correlation between objective and subjective measures
data$r_obj_subj <- NA

# Input the correlation for studies already reporting the correlation between obj & subj measures
data$r_obj_subj[data$es_type=="correlation"] <- data$es_agree[data$es_type=="correlation"] 
describe(data$r_obj_subj)  
```

    ##    vars  n mean   sd median trimmed  mad  min  max range skew kurtosis   se
    ## X1    1 20  0.3 0.11   0.25     0.3 0.09 0.17 0.48  0.31 0.48    -1.44 0.02

``` r
# Derive variable for the variance of the correlation between objective and subjective measures
data$r_obj_subj_var <- NA

# Input the variance for studies already reporting the SE of the correlation between obj & subj measures
# Note: Vr = (1 - r^2)^2 / n - 1 (Borenstein et al., 2009)
data$r_obj_subj_var[data$es_type=="correlation"] <- (1 - data$r_obj_subj[data$es_type=="correlation"]^2)^2 / (data$n_total[data$es_type=="correlation"] - 1)
describe(data$r_obj_subj_var)
```

    ##    vars  n mean sd median trimmed mad min max range skew kurtosis se
    ## X1    1 20    0  0      0       0   0   0   0     0 0.33    -1.25  0

``` r
#### Derive correlation for studies reporting kappas 
# To do so, need to define function to derive tetrachoric correlations from n_obj_subj, n_obj_only, n_subj_only, n_none
# This function makes a 2x2 table with Ns of groups with:
# - subjective and objective measures (n_obj_subj)
# - objective measures only (n_obj_only)
# - subjective measures only (n_subj_only)
# - neither subjective nor objective measures (n_none)
# Then the polychoric correlation and variance is calculated from the 2x2 table
# the function returns the correlation between objective & subjective measures and variance

# Check Ns in each group are reported for studies reporting kappas
describe(data$n_obj_subj[data$es_type=="kappa"]) # n with concordant objective and subjective measures
```

    ##    vars  n  mean    sd median trimmed   mad min max range skew kurtosis    se
    ## X1    1 43 76.77 93.86     51      58 54.86   3 492   489 2.43     7.05 14.31

``` r
describe(data$n_obj_only[data$es_type=="kappa"]) # n with objective measures only
```

    ##    vars  n  mean    sd median trimmed  mad min max range skew kurtosis   se
    ## X1    1 43 48.09 50.86     30   38.54 25.2   3 264   261 2.25     5.86 7.76

``` r
describe(data$n_subj_only[data$es_type=="kappa"]) # n with subjective measures only
```

    ##    vars  n   mean    sd median trimmed   mad min max range skew kurtosis    se
    ## X1    1 43 100.16 97.96     68    83.8 56.34   7 527   520 2.23     6.32 14.94

``` r
describe(data$n_none[data$es_type=="kappa"]) # n with neither objective nor subjective measures
```

    ##    vars  n   mean    sd median trimmed    mad min max range skew kurtosis    se
    ## X1    1 43 250.98 204.4    232  228.63 186.81   2 839   837  0.9     0.28 31.17

``` r
# Define function to derive tetrachoric correlations
tetrachoric_cor <- function(n_obj_subj, n_obj_only, n_subj_only, n_none) {
  table <- matrix(nrow=2, ncol=2, 
                  c(n_obj_subj, n_obj_only, n_subj_only, n_none)) # input Ns in each group into a 2x2 table
  results <- polychor(table, std.err=TRUE) # calculate the tetrachoric correlation
  r <- results$rho
  var <- results$var
  return(c(r, var)) # return the correlation and variance
}

# Derive tetrachoric correlations for studies reporting kappas
corr_tetra <- as.data.frame(mapply(tetrachoric_cor, 
                                   data$n_obj_subj[data$es_type=="kappa"], # n with concordant objective and subjective measures
                                   data$n_obj_only[data$es_type=="kappa"], # n with objective measures only
                                   data$n_subj_only[data$es_type=="kappa"], # n with subjective measures only
                                   data$n_none[data$es_type=="kappa"])) # n with neither objective nor subjective measures
corr_tetra
```

    ##            V1          V2          V3          V4          V5          V6
    ## 1 0.398565044 0.363195266 0.430576746 0.507544780 0.597408670 0.283511335
    ## 2 0.001714599 0.003564129 0.003489909 0.001489651 0.003944182 0.003676455
    ##           V7          V8          V9        V10        V11       V12        V13
    ## 1 0.13635375 0.161937002 0.048053204 0.15348015 0.23916139 0.3101324 0.62551413
    ## 2 0.00330348 0.003599021 0.004853267 0.01162385 0.02356247 0.0200989 0.02158052
    ##          V14         V15        V16        V17        V18        V19
    ## 1 0.31042725 0.577473670 0.16777774 0.14659757 0.08072779 0.40867589
    ## 2 0.01515659 0.009136286 0.01121859 0.01131031 0.01366712 0.01817221
    ##           V20         V21        V22        V23         V24         V25
    ## 1 0.661880643 0.925065421 0.09814850 0.08160140 0.410023576 0.558128889
    ## 2 0.007129169 0.001034428 0.05265624 0.02072414 0.003039903 0.009176613
    ##          V26         V27        V28        V29        V30        V31
    ## 1 0.61281298 -0.01362941 0.12293748 0.36373608 0.11773244 0.24849629
    ## 2 0.01424795  0.02140112 0.01277083 0.01303384 0.01468163 0.01337372
    ##           V32        V33        V34         V35         V36         V37
    ## 1 0.139223379 0.08074730 0.19774824 0.327336495 0.095009476 0.577200474
    ## 2 0.008615533 0.01652397 0.02112247 0.007542745 0.008316547 0.004366321
    ##           V38         V39         V40        V41        V42        V43
    ## 1 0.259214479 0.064693566 0.300186063 0.32927109 0.36158594 0.13422583
    ## 2 0.005742709 0.007025649 0.007656705 0.02317748 0.01177222 0.01101496

``` r
# The columns show the results for each study, row 1 shows the correlation and row 2 shows the variance

table(data$es_type)
```

    ## 
    ## correlation       kappa 
    ##          20          43

``` r
# Input the correlation between objective and subjective measures into the variable
data$r_obj_subj[data$es_type=="kappa"] <- as.vector(unlist(corr_tetra[1,])) # 1 = first row
describe(data$r_obj_subj) 
```

    ##    vars  n mean   sd median trimmed  mad   min  max range skew kurtosis   se
    ## X1    1 63  0.3 0.18   0.26    0.29 0.18 -0.01 0.93  0.94 0.83     0.68 0.02

``` r
# Input the variance of the correlation between objective and subjective measures into the variable
data$r_obj_subj_var[data$es_type=="kappa"] <- as.vector(unlist(corr_tetra[2,])) # 2 = second row
describe(data$r_obj_subj_var) 
```

    ##    vars  n mean   sd median trimmed  mad min  max range skew kurtosis se
    ## X1    1 63 0.01 0.01      0    0.01 0.01   0 0.05  0.05 2.22     7.74  0

##### 1.2 Convert correlations to Fishers-z

``` r
# Create new variables for Fishers-Z conversion
data$z_obj_subj <- NA

# Convert correlations to Fishers-Z
data$z_obj_subj <- fisherz(data$r_obj_subj)
# Compare variables for subjective effect sizes
list(data$z_obj_subj)
```

    ## [[1]]
    ##  [1]  0.42194181  0.38056181  0.46060447  0.55941705  0.68910802  0.25541281
    ##  [7]  0.47223080  0.32054541  0.29149620  0.13720835  0.16337522  0.04809024
    ## [13]  0.15470261  0.24388444  0.18198269  0.32069189  0.73401267  0.32101815
    ## [19]  0.65866407  0.16937916  0.14766149  0.08090385  0.43402059  0.79615310
    ## [25]  1.62304989  0.09846550  0.08178325  0.49731129  0.25541281  0.23418947
    ## [31]  0.18198269  0.45989668  0.47223080  0.47223080  0.52298428  0.33543960
    ## [37]  0.28118341  0.23630221  0.26396460  0.24265295  0.43563956  0.63011136
    ## [43]  0.71341365 -0.01363026  0.12356250  0.38118494  0.11828097  0.25380949
    ## [49]  0.14013352  0.24477411  0.17166666  0.21317135  0.08092348  0.20038806
    ## [55]  0.33984218  0.09529691  0.65825428  0.26526612  0.06478405  0.30972408
    ## [61]  0.34201048  0.37870918  0.13504076

``` r
list(data$r_obj_subj)
```

    ## [[1]]
    ##  [1]  0.39856504  0.36319527  0.43057675  0.50754478  0.59740867  0.25000000
    ##  [7]  0.44000000  0.31000000  0.28351133  0.13635375  0.16193700  0.04805320
    ## [13]  0.15348015  0.23916139  0.18000000  0.31013240  0.62551413  0.31042725
    ## [19]  0.57747367  0.16777774  0.14659757  0.08072779  0.40867589  0.66188064
    ## [25]  0.92506542  0.09814850  0.08160140  0.46000000  0.25000000  0.23000000
    ## [31]  0.18000000  0.43000000  0.44000000  0.44000000  0.48000000  0.32340000
    ## [37]  0.27400000  0.23200000  0.25800000  0.23800000  0.41002358  0.55812889
    ## [43]  0.61281298 -0.01362941  0.12293748  0.36373608  0.11773244  0.24849629
    ## [49]  0.13922338  0.24000000  0.17000000  0.21000000  0.08074730  0.19774824
    ## [55]  0.32733650  0.09500948  0.57720047  0.25921448  0.06469357  0.30018606
    ## [61]  0.32927109  0.36158594  0.13422583

``` r
# Check number of effect sizes are the same between r and Fishers-Z 
describe(data$z_obj_subj) 
```

    ##    vars  n mean   sd median trimmed  mad   min  max range skew kurtosis   se
    ## X1    1 63 0.33 0.25   0.27     0.3 0.19 -0.01 1.62  1.64 2.29     8.73 0.03

``` r
describe(data$r_obj_subj) 
```

    ##    vars  n mean   sd median trimmed  mad   min  max range skew kurtosis   se
    ## X1    1 63  0.3 0.18   0.26    0.29 0.18 -0.01 0.93  0.94 0.83     0.68 0.02

``` r
mean(data$z_obj_subj[!is.na(data$z_obj_subj)]) 
```

    ## [1] 0.3330874

``` r
describe(data$z_obj_subj) 
```

    ##    vars  n mean   sd median trimmed  mad   min  max range skew kurtosis   se
    ## X1    1 63 0.33 0.25   0.27     0.3 0.19 -0.01 1.62  1.64 2.29     8.73 0.03

``` r
# Derive variables for variance of Fisher's Z
# Variance = 1/n-3 (see https://www.meta-analysis.com/downloads/Meta-analysis%20Effect%20sizes%20based%20on%20correlations.pdf)
data$z_obj_subj_var <- NA
data$z_obj_subj_var <- 1 / (data$n_total-3)
as.data.frame(describe(data$z_obj_subj_var))
```

    ##    vars  n        mean          sd      median     trimmed         mad
    ## X1    1 63 0.002866749 0.001611116 0.002638522 0.002743721 0.001661811
    ##             min         max       range      skew   kurtosis           se
    ## X1 0.0002242655 0.006369427 0.006145161 0.6010314 -0.3988482 0.0002029815

``` r
# Show the variance converted to Fisher's z
list(data$z_obj_subj_var)
```

    ## [[1]]
    ##  [1] 0.0014619883 0.0009057971 0.0008936550 0.0011025358 0.0014619883
    ##  [6] 0.0030120482 0.0008410429 0.0024096386 0.0013037810 0.0013037810
    ## [11] 0.0013037810 0.0013037810 0.0013037810 0.0013037810 0.0004847310
    ## [16] 0.0028818444 0.0028985507 0.0028818444 0.0046728972 0.0046728972
    ## [21] 0.0046728972 0.0046728972 0.0063694268 0.0063694268 0.0063694268
    ## [26] 0.0063694268 0.0063694268 0.0020833333 0.0020833333 0.0022675737
    ## [31] 0.0022675737 0.0006877579 0.0006968641 0.0022624434 0.0020449898
    ## [36] 0.0002242655 0.0047846890 0.0047846890 0.0037453184 0.0037453184
    ## [41] 0.0011862396 0.0037593985 0.0037878788 0.0036764706 0.0037037037
    ## [46] 0.0026385224 0.0026666667 0.0024570025 0.0024752475 0.0026315789
    ## [51] 0.0050000000 0.0050000000 0.0035460993 0.0035460993 0.0035460993
    ## [56] 0.0022075055 0.0021459227 0.0021052632 0.0022522523 0.0027700831
    ## [61] 0.0027855153 0.0027027027 0.0027397260

##### 1.3 Prepare to run meta-analysis of agreement

``` r
# Derive variable indexing whether the study assesses maltreatment, bullying or neighbourhood adversity
table(data$Subj_Exposure)
```

    ## 
    ##               Acceptance_And_Self_Esteem 
    ##                                        1 
    ##                 Age_Appropriate_Autonomy 
    ##                                        1 
    ##               Any_Emotional_Maltreatment 
    ##                                        1 
    ##                          Emotional_Abuse 
    ##                                        3 
    ##           Emotional_Maltreatment_3-8_Yrs 
    ##                                        1 
    ##          Emotional_Maltreatment_9-16_Yrs 
    ##                                        1 
    ##               Failure_To_Provide_3-8_Yrs 
    ##                                        1 
    ##              Failure_To_Provide_9-16_Yrs 
    ##                                        1 
    ##                          Family_Violence 
    ##                                        1 
    ##              Lack_Of_Supervision_3-8_Yrs 
    ##                                        1 
    ##             Lack_Of_Supervision_9-16_Yrs 
    ##                                        1 
    ##                             Maltreatment 
    ##                                        2 
    ##            Maltreatment_out_of_home_care 
    ##                                        1 
    ##   Maltreatment_prior_to_out_of_home_care 
    ##                                        1 
    ##                                  Neglect 
    ##                                        3 
    ##                 Neglect_out_of_home_care 
    ##                                        1 
    ##        Neglect_prior_to_out_of_home_care 
    ##                                        1 
    ##                   Neighborhood_Disorder  
    ##                                        1 
    ##                   Neighbourhood_Violence 
    ##                                        1 
    ##                      Overt_Victimisation 
    ##                                        1 
    ##                       Peer_Victimisation 
    ##                                       11 
    ##              Peer_Victimisation_Grade_10 
    ##                                        1 
    ##               Peer_Victimisation_Grade_5 
    ##                                        1 
    ##               Peer_Victimisation_Grade_6 
    ##                                        1 
    ##               Peer_Victimisation_Grade_9 
    ##                                        1 
    ##                Peer_Victimisation_Time_1 
    ##                                        1 
    ##                Peer_Victimisation_Time_2 
    ##                                        1 
    ##                           Physical_Abuse 
    ##                                        5 
    ##                   Physical_Abuse_3-8_Yrs 
    ##                                        1 
    ##                  Physical_Abuse_9-16_Yrs 
    ##                                        1 
    ##          Physical_Abuse_out_of_home_care 
    ##                                        1 
    ## Physical_Abuse_prior_to_out_of_home_care 
    ##                                        1 
    ##                      Psychological_Abuse 
    ##                                        1 
    ##        Psychological_Safety_And_Security 
    ##                                        1 
    ##                 Relational_Victimisation 
    ##                                        2 
    ##                              Restriction 
    ##                                        1 
    ##                             Sexual_Abuse 
    ##                                        5 
    ##            Sexual_Abuse_out_of_home_care 
    ##                                        1 
    ##   Sexual_Abuse_prior_to_out_of_home_care 
    ##                                        1

``` r
data$exposure_type_broad <- "maltreatment" # code to maltreatment unless otherwise specified

# Code all measures including the terms "peer_vic", "relational_vic", or "overt_vic" to bullying
data$exposure_type_broad[grepl("peer_vic", data$Subj_Exposure, ignore.case = TRUE)] <- "bullying"
data$exposure_type_broad[grepl("relational_vic", data$Subj_Exposure, ignore.case = TRUE)] <- "bullying"
data$exposure_type_broad[grepl("overt_vic", data$Subj_Exposure, ignore.case = TRUE)] <- "bullying"

# Code all measures including the terms "neighbourhood", "neighborhood" to neighbourhood
data$exposure_type_broad[grepl("neighborhood_disorder", data$Subj_Exposure, ignore.case = TRUE)] <- "neighbourhood"
data$exposure_type_broad[grepl("neighbourhood_violence", data$Subj_Exposure, ignore.case = TRUE)] <- "neighbourhood"
table(data$exposure_type_broad)
```

    ## 
    ##      bullying  maltreatment neighbourhood 
    ##            20            41             2

``` r
# Compare original coding to new broad categories of adversity type
data %>%  dplyr::select(author, Subj_Exposure, exposure_type_broad)
```

    ##                        author                            Subj_Exposure
    ## 1             Danese & Widom,                             Maltreatment
    ## 2             Danese & Widom,                           Physical_Abuse
    ## 3             Danese & Widom,                             Sexual_Abuse
    ## 4             Danese & Widom,                                  Neglect
    ## 5             Gromann et al.,                       Peer_Victimisation
    ## 6      Zimmer-Gembeck et al.,                 Relational_Victimisation
    ## 7              Bouman et al.,                       Peer_Victimisation
    ## 8          Graham & Juvonen.,                       Peer_Victimisation
    ## 9              Graham et al.,                       Peer_Victimisation
    ## 10              White et al.,               Any_Emotional_Maltreatment
    ## 11              White et al.,        Psychological_Safety_And_Security
    ## 12              White et al.,               Acceptance_And_Self_Esteem
    ## 13              White et al.,                 Age_Appropriate_Autonomy
    ## 14              White et al.,                              Restriction
    ## 15            Newbury et al.,                   Neighborhood_Disorder 
    ## 16            Everson et al.,                           Physical_Abuse
    ## 17            Everson et al.,                             Sexual_Abuse
    ## 18            Everson et al.,                      Psychological_Abuse
    ## 19            Negriff et al.,                             Sexual_Abuse
    ## 20            Negriff et al.,                           Physical_Abuse
    ## 21            Negriff et al.,                          Emotional_Abuse
    ## 22            Negriff et al.,                                  Neglect
    ## 23              McGee et al.,                           Physical_Abuse
    ## 24              McGee et al.,                          Family_Violence
    ## 25              McGee et al.,                             Sexual_Abuse
    ## 26              McGee et al.,                          Emotional_Abuse
    ## 27              McGee et al.,                                  Neglect
    ## 28             Kochel et al.,               Peer_Victimisation_Grade_5
    ## 29             Kochel et al.,               Peer_Victimisation_Grade_6
    ## 30             Kochel et al.,               Peer_Victimisation_Grade_9
    ## 31             Kochel et al.,              Peer_Victimisation_Grade_10
    ## 32             Mulder et al.,                Peer_Victimisation_Time_1
    ## 33             Mulder et al.,                Peer_Victimisation_Time_2
    ## 34              Rigby & Slee,                       Peer_Victimisation
    ## 35              Rigby & Slee,                       Peer_Victimisation
    ## 36     Goldman-Mellor et al.,                   Neighbourhood_Violence
    ## 37            McClain et al.,                       Peer_Victimisation
    ## 38            McClain et al.,                       Peer_Victimisation
    ## 39            McClain et al.,                       Peer_Victimisation
    ## 40            McClain et al.,                       Peer_Victimisation
    ## 41              Smith et al.,                             Maltreatment
    ## 42             Sierau et al.,               Failure_To_Provide_3-8_Yrs
    ## 43             Sierau et al.,              Lack_Of_Supervision_3-8_Yrs
    ## 44             Sierau et al.,                   Physical_Abuse_3-8_Yrs
    ## 45             Sierau et al.,           Emotional_Maltreatment_3-8_Yrs
    ## 46             Sierau et al.,              Failure_To_Provide_9-16_Yrs
    ## 47             Sierau et al.,             Lack_Of_Supervision_9-16_Yrs
    ## 48             Sierau et al.,                  Physical_Abuse_9-16_Yrs
    ## 49             Sierau et al.,          Emotional_Maltreatment_9-16_Yrs
    ## 50           Flanagan et al.,                       Peer_Victimisation
    ## 51 De Los Reyes & Prinstein.,                      Overt_Victimisation
    ## 52 De Los Reyes & Prinstein.,                 Relational_Victimisation
    ## 53                Cho et al.,                           Physical_Abuse
    ## 54                Cho et al.,                          Emotional_Abuse
    ## 55                Cho et al.,                             Sexual_Abuse
    ## 56           Havlicek et al.,   Maltreatment_prior_to_out_of_home_care
    ## 57           Havlicek et al.,   Sexual_Abuse_prior_to_out_of_home_care
    ## 58           Havlicek et al., Physical_Abuse_prior_to_out_of_home_care
    ## 59           Havlicek et al.,        Neglect_prior_to_out_of_home_care
    ## 60           Havlicek et al.,            Maltreatment_out_of_home_care
    ## 61           Havlicek et al.,            Sexual_Abuse_out_of_home_care
    ## 62           Havlicek et al.,          Physical_Abuse_out_of_home_care
    ## 63           Havlicek et al.,                 Neglect_out_of_home_care
    ##    exposure_type_broad
    ## 1         maltreatment
    ## 2         maltreatment
    ## 3         maltreatment
    ## 4         maltreatment
    ## 5             bullying
    ## 6             bullying
    ## 7             bullying
    ## 8             bullying
    ## 9             bullying
    ## 10        maltreatment
    ## 11        maltreatment
    ## 12        maltreatment
    ## 13        maltreatment
    ## 14        maltreatment
    ## 15       neighbourhood
    ## 16        maltreatment
    ## 17        maltreatment
    ## 18        maltreatment
    ## 19        maltreatment
    ## 20        maltreatment
    ## 21        maltreatment
    ## 22        maltreatment
    ## 23        maltreatment
    ## 24        maltreatment
    ## 25        maltreatment
    ## 26        maltreatment
    ## 27        maltreatment
    ## 28            bullying
    ## 29            bullying
    ## 30            bullying
    ## 31            bullying
    ## 32            bullying
    ## 33            bullying
    ## 34            bullying
    ## 35            bullying
    ## 36       neighbourhood
    ## 37            bullying
    ## 38            bullying
    ## 39            bullying
    ## 40            bullying
    ## 41        maltreatment
    ## 42        maltreatment
    ## 43        maltreatment
    ## 44        maltreatment
    ## 45        maltreatment
    ## 46        maltreatment
    ## 47        maltreatment
    ## 48        maltreatment
    ## 49        maltreatment
    ## 50            bullying
    ## 51            bullying
    ## 52            bullying
    ## 53        maltreatment
    ## 54        maltreatment
    ## 55        maltreatment
    ## 56        maltreatment
    ## 57        maltreatment
    ## 58        maltreatment
    ## 59        maltreatment
    ## 60        maltreatment
    ## 61        maltreatment
    ## 62        maltreatment
    ## 63        maltreatment

``` r
# Derive effect size ID variable
data$es_id <- 1:nrow(data)
```

##### 1.4 Descriptive statistics

``` r
# Descriptive data for included agreement studies
# Number of independent studies 
k_studies <- data %>%
  group_by(author) %>%
  dplyr::summarise(m = max(author)) %>% nrow()
k_studies
```

    ## [1] 22

``` r
# Number of cohorts
k_cohort <- data %>%
  group_by(cohort_distinct) %>%
  dplyr::summarise(m = max(cohort)) %>% nrow()
k_cohort
```

    ## [1] 21

``` r
# Total sample size
data %>%
  group_by(cohort) %>%
  dplyr::summarise(m = max(n_total))%>%
  dplyr::summarise(sum = sum(m))
```

    ## # A tibble: 1 × 1
    ##     sum
    ##   <dbl>
    ## 1 18163

``` r
# Age of self-report
age_self <- data %>%
  group_by(cohort_distinct) %>%
  dplyr::summarise(mean_age_self = mean(age_self)) %>% #to edit 
  dplyr::summarise(overall=mean(mean_age_self, na.rm=TRUE))
age_self
```

    ## # A tibble: 1 × 1
    ##   overall
    ##     <dbl>
    ## 1    14.8

``` r
# Number and type of effect sizes 
#**Note: es_type is the original effect sizes reported (before deriving correlations from studies reporting kappas)**
describe(data$r_obj_subj)$n
```

    ## [1] 63

``` r
table(data$es_type) 
```

    ## 
    ## correlation       kappa 
    ##          20          43

``` r
# es reported for each exposure
#es for maltreatment
n_es_mal <- length(data$es_type[!is.na(data$es_type) & data$exposure_type_broad=="maltreatment"])
n_es_mal
```

    ## [1] 41

``` r
# es for bullying
n_es_mal <- length(data$es_type[!is.na(data$es_type) & data$exposure_type_broad=="bullying"])
n_es_mal
```

    ## [1] 20

``` r
# es for neighbourhood
n_es_neigh <- length(data$es_type[!is.na(data$es_type) & data$exposure_type_broad=="neighbourhood"])
n_es_neigh
```

    ## [1] 2

``` r
# Average proportion female
perc_female <- data %>%
  group_by(cohort_distinct) %>%
  dplyr::summarise(mean_percent_female = mean(percent_female)) %>%
  dplyr::summarise(overall=mean(mean_percent_female, na.rm=TRUE))
perc_female
```

    ## # A tibble: 1 × 1
    ##   overall
    ##     <dbl>
    ## 1    51.3

``` r
# Number of studies reporting maltreatment
data %>%
  group_by(author) %>%
  filter(exposure_type_broad=="maltreatment")%>%
  dplyr::summarise(m = max(author)) %>% nrow()
```

    ## [1] 9

``` r
# Number of studies reporting bullying
data %>%
  group_by(author) %>%
  filter(exposure_type_broad=="bullying")%>%
  dplyr::summarise(m = max(author)) %>% nrow()
```

    ## [1] 11

``` r
# Number of studies reporting neighbourhood
data %>%
  group_by(author) %>%
  filter(exposure_type_broad=="neighbourhood")%>%
  dplyr::summarise(m = max(author)) %>% nrow()
```

    ## [1] 2

##### 1.5 Run meta-analysis of agreement

###### 1.5.1 Maltreatment - Fishers-Z

``` r
# Maltreatment - Fishers Z
maltreat_z <- rma.mv(z_obj_subj, z_obj_subj_var, data=data, random=list(~ 1 | es_id, ~ 1 | author, ~1 | cohort), subset = exposure_type_broad == "maltreatment")
maltreat_z
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 41; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed  factor 
    ## sigma^2.1  0.0778  0.2790     41     no   es_id 
    ## sigma^2.2  0.0041  0.0641      9     no  author 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 40) = 930.3168, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub     ​ 
    ##   0.3355  0.0501  6.6895  <.0001  0.2372  0.4337  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert estimate and CIs back to r 
round(fisherz2r(c(maltreat_z$beta, maltreat_z$ci.lb, maltreat_z$ci.ub)),2)
```

    ## [1] 0.32 0.23 0.41

###### 1.5.2 Maltreatment - Kappa

``` r
# Meta-analysis of Cohens kappa
data$kappa_var <- data$se_agree^2 # derive variance of the kappa
maltreat_kappa <- rma.mv(es_agree, kappa_var, data=data, random=list(~ 1 | es_id, ~ 1 | author, ~1 | cohort),
       subset = exposure_type_broad == "maltreatment" & es_type=="kappa")
maltreat_kappa 
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 41; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed  factor 
    ## sigma^2.1  0.0139  0.1180     41     no   es_id 
    ## sigma^2.2  0.0000  0.0000      9     no  author 
    ## sigma^2.3  0.0039  0.0628      8     no  cohort 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 40) = 258.9919, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub     ​ 
    ##   0.1597  0.0319  5.0090  <.0001  0.0972  0.2222  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert estimate and CIs back to r 
round(c(maltreat_kappa$b, maltreat_kappa$ci.lb, maltreat_kappa$ci.ub),2)
```

    ## [1] 0.16 0.10 0.22

###### 1.5.3 Bullying

``` r
# Bullying - Fishers Z
bully_z <- rma.mv(z_obj_subj, z_obj_subj_var, data=data, random=list(~ 1 | es_id, ~ 1 | author, ~1 | cohort),
                     subset = exposure_type_broad == "bullying")
bully_z 
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 20; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed  factor 
    ## sigma^2.1  0.0055  0.0744     20     no   es_id 
    ## sigma^2.2  0.0161  0.1267     11     no  author 
    ## sigma^2.3  0.0000  0.0000     13     no  cohort 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 19) = 198.2599, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub     ​ 
    ##   0.3617  0.0441  8.2017  <.0001  0.2753  0.4482  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert estimate and CIs back to r
round(fisherz2r(c(bully_z$beta, bully_z$ci.lb, bully_z$ci.ub)),2) 
```

    ## [1] 0.35 0.27 0.42

###### 1.5.4 Neighbourhood

``` r
# Neighbourhood - Fishers Z 
neigh_z <- rma.mv(z_obj_subj, z_obj_subj_var, data=data, random=list(~ 1 | es_id, ~ 1 | author, ~1 | cohort),
                  subset = exposure_type_broad == "neighbourhood" & es_type=="correlation")
neigh_z 
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 2; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed  factor 
    ## sigma^2.1  0.0038  0.0617      2     no   es_id 
    ## sigma^2.2  0.0038  0.0617      2     no  author 
    ## sigma^2.3  0.0038  0.0617      2     no  cohort 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 1) = 33.2146, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub     ​ 
    ##   0.2596  0.0767  3.3830  0.0007  0.1092  0.4099  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert estimate and CIs back to r
round(fisherz2r(c(neigh_z$beta, neigh_z$ci.lb, neigh_z$ci.ub)),2)
```

    ## [1] 0.25 0.11 0.39

##### 1.6 Forest plots for meta-analysis

**Create a forest plot for each of the subtypes of adversity exposure**

###### 1.6.1 Maltreatment - Fishers-Z

``` r
maltreatment_data <- subset(data, exposure_type_broad=="maltreatment")
maltreatment_data2 <- maltreatment_data[order(maltreatment_data$author, maltreatment_data$year),] #re-order
maltreatment_data2$refPlot <- paste0(maltreatment_data2$author, " ", maltreatment_data2$year)


# Maltreatment - Re-run meta-analyses - Fishers-Z
maltreat_z <- rma.mv(z_obj_subj, z_obj_subj_var, random=list(~ 1 | es_id, ~ 1 | author, ~1 | cohort),
                     data = maltreatment_data2, subset = exposure_type_broad == "maltreatment" & !is.na(exposure_type_broad),
                     slab=paste(author, es_id, sep=", "))
maltreat_z 
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 41; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed  factor 
    ## sigma^2.1  0.0778  0.2790     41     no   es_id 
    ## sigma^2.2  0.0041  0.0641      9     no  author 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 40) = 930.3168, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub     ​ 
    ##   0.3355  0.0501  6.6895  <.0001  0.2372  0.4337  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Forest plot
library(mgsub)
forest(maltreat_z,
       transf=transf.ztor, # inverse of Fisher's r-to-z transformation
       order = order(maltreatment_data2$author, maltreatment_data2$year), 
       xlim=c(-1.5, 1.5), at=c(-0.2,0,0.2,0.4,0.6,0.8,1), 
       xlab = "Correlation", 
       mlab="Multi-level Meta-Analysis Model", 
       slab=NA,
       ilab=cbind(paste0(maltreatment_data2$refPlot), 
                  mgsub(maltreatment_data2$Obj_Exposure, c("_", "(3 to 8 yrs)", "(9 to 16 yrs)"), c(" ", "(3-8 years)", "(9-16 years)"))),
       ilab.xpos=c(-1.55, -1.1), cex=0.45, 
       ilab.pos = c(4, 4),
       top=1.5) 
par(font=2)
text(c(-1.4, -0.88, 1.3), 43, c("Reference", "Maltreatment type", "Correlation [95% CI]"), cex=0.45)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
#save 575x600 
```

###### 1.6.2 Bullying

``` r
bullying_data <- subset(data, exposure_type_broad=="bullying")
bullying_data2 <- bullying_data[order(bullying_data$author, bullying_data$year),]
#re-order
bullying_data2$refPlot <- paste0(bullying_data2$author, " ", bullying_data2$year)

# Removes underscores from Obj_Exposure variable
bullying_data2$Obj_Exposure <- gsub("_"," ", bullying_data$Obj_Exposure)
bullying_data2$Obj_Exposure
```

    ##  [1] "Peer Victimisation"          "Relational Victimisation"   
    ##  [3] "Peer Victimisation"          "Peer Victimisation"         
    ##  [5] "Peer Victimisation"          "Peer Victimisation Grade 5" 
    ##  [7] "Peer Victimisation Grade 6"  "Peer Victimisation Grade 9" 
    ##  [9] "Peer Victimisation Grade 10" "Peer Victimisation Time 1"  
    ## [11] "Peer Victimisation Time 2"   "Peer Victimisation"         
    ## [13] "Peer Victimisation"          "Overt Victimisation"        
    ## [15] "Relational Victimisation"    "Overt Victimisation"        
    ## [17] "Relational Victimisation"    "Peer Victimisation"         
    ## [19] "Overt Victimisation"         "Relational Victimisation"

``` r
# Bullying - Re-run meta-analyses - Fishers-Z
bully_z <- rma.mv(z_obj_subj, z_obj_subj_var, random=list(~ 1 | es_id, ~ 1 | author, ~1 | cohort),
                  data=bullying_data2, subset = exposure_type_broad == "bullying" & !is.na(exposure_type_broad),
                  slab=paste(author, es_id, sep=", "))
bully_z
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 20; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed  factor 
    ## sigma^2.1  0.0055  0.0744     20     no   es_id 
    ## sigma^2.2  0.0161  0.1267     11     no  author 
    ## sigma^2.3  0.0000  0.0000     13     no  cohort 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 19) = 198.2599, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub     ​ 
    ##   0.3617  0.0441  8.2017  <.0001  0.2753  0.4482  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Forest plot
forest(bully_z,
       transf=transf.ztor, 
       order = order(bullying_data2$author, bullying_data2$year), 
       xlim=c(-1.7, 1.5), at=c(-0.2,0,0.2,0.4,0.6, 0.8), 
       xlab = "Correlation", 
       mlab="Multivariate Meta-Analysis Model",
       slab=NA, 
       ilab=cbind(bullying_data2$refPlot, bullying_data2$Obj_Exposure),  
      ilab.xpos=c(-1.7,-0.9), cex=0.4, 
       ilab.pos = c(4, 4, 4),
       top=3) 
par(font=2)
text(c(-1.5, -0.7, 1.25), 21.5, c("Reference", "Bullying type", "Correlation [95% CI]"), cex=0.4)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

###### 1.6.3 Neighbourhood

``` r
neighbour_data <- subset(data, exposure_type_broad=="neighbourhood")

# Removes underscores from Subj_Exposure variable
neighbour_data$Subj_Exposure <- gsub("_"," ", neighbour_data$Subj_Exposure)

neighbour_data$Subj_Exposure
```

    ## [1] "Neighborhood Disorder " "Neighbourhood Violence"

``` r
# Neighbourhood - Re-run meta-analyses - Fishers-Z
neigh_z <- rma.mv(z_obj_subj, z_obj_subj_var, data=neighbour_data, random=list(~ 1 | es_id, ~ 1 | author, ~1 | cohort),
                  subset=es_type=="correlation")
neigh_z 
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 2; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed  factor 
    ## sigma^2.1  0.0038  0.0617      2     no   es_id 
    ## sigma^2.2  0.0038  0.0617      2     no  author 
    ## sigma^2.3  0.0038  0.0617      2     no  cohort 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 1) = 33.2146, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub     ​ 
    ##   0.2596  0.0767  3.3830  0.0007  0.1092  0.4099  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert estimate and CIs back to r
fisherz2r(c(neigh_z$beta, neigh_z$ci.lb, neigh_z$ci.ub))    
```

    ## [1] 0.2538838 0.1087522 0.3884180

``` r
# Forest plot
forest(neigh_z,
       transf=transf.ztor, 
       order = order(neighbour_data$author, neighbour_data$year), 
       xlim=c(-1.7, 1.5), at=c(-0.2,0,0.2,0.4,0.6, 0.8), 
       xlab = "Correlation", 
       mlab="Multilevel Meta-Analysis Model",
       slab=NA, 
       ilab=cbind(paste0(neighbour_data$author[neighbour_data$es_type=="correlation"], " ",
                  neighbour_data$year[neighbour_data$es_type=="correlation"]), 
                  neighbour_data$Subj_Exposure[neighbour_data$es_type=="correlation"]), 
       ilab.xpos= c(-1.65, -0.9), cex=0.6, 
       ilab.pos = c(4, 4),
       top=2) 
par(font=2)
text(c(-1.47,-0.63, 1.3), 3.2, c("Reference", "Neighbourhood adversity type", "Correlation [95% CI]"), cex=0.6)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
# save 740x360
```

##### 1.7 Test publication bias

    # Information on adapting Eggers test in multi-level meta-analysis models: https://stats.stackexchange.com/questions/134768/metafor-package-in-r-ranktest-for-multivariate-meta-analysis
    # Note: SE and variance should not be used because they are directly related to the correlation coefficient, see: https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2020-May/002086.html
    # "There is an inherent correlation between correlations and their sampling variances, which can lead to false positives."
    # Wolfgang Viechtbauer recommends using the inverse sample sizes as the predictor

###### 1.7.1 Maltreatment - Fishers-Z

``` r
egger_maltreat_z <- rma.mv(z_obj_subj, z_obj_subj_var, data=data, random=list(~ 1 | es_id, ~ 1 | author, ~1 | cohort),
                           mod = I(1/n_total),  # moderator = the inverse sample size
                           subset = exposure_type_broad == "maltreatment") 
egger_maltreat_z
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 41; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed  factor 
    ## sigma^2.1  0.0784  0.2799     41     no   es_id 
    ## sigma^2.2  0.0000  0.0000      9     no  author 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 39) = 916.1111, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 2.5873, p-val = 0.1077
    ## 
    ## Model Results:
    ## 
    ##          estimate       se    zval    pval    ci.lb     ci.ub   ​ 
    ## intrcpt    0.1943   0.0968  2.0082  0.0446   0.0047    0.3839  * 
    ## mods      45.6617  28.3879  1.6085  0.1077  -9.9775  101.3009    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

###### 1.7.2 Maltreatment - Kappa

``` r
egger_maltreat_kappa <- rma.mv(es_agree, kappa_var, data=data, random=list(~ 1 | es_id, ~ 1 | author, ~1 | cohort),
                               subset = exposure_type_broad == "maltreatment" & es_type=="kappa",
                               mod = kappa_var) 
egger_maltreat_kappa
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 41; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed  factor 
    ## sigma^2.1  0.0138  0.1175     41     no   es_id 
    ## sigma^2.2  0.0000  0.0000      9     no  author 
    ## sigma^2.3  0.0045  0.0668      8     no  cohort 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 39) = 255.9653, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.4624, p-val = 0.4965
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval     ci.lb   ci.ub     ​ 
    ## intrcpt    0.1786  0.0426   4.1964  <.0001    0.0952  0.2620  *** 
    ## mods      -3.6823  5.4153  -0.6800  0.4965  -14.2961  6.9315      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

###### 1.7.3 Bullying

``` r
egger_bully_z <- rma.mv(z_obj_subj, z_obj_subj_var, data=data, random=list(~ 1 | es_id, ~ 1 | author, ~1 | cohort),
                        mods =  I(1/n_total),
                        subset = exposure_type_broad == "bullying")
egger_bully_z
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 20; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed  factor 
    ## sigma^2.1  0.0059  0.0769     20     no   es_id 
    ## sigma^2.2  0.0068  0.0826     11     no  author 
    ## sigma^2.3  0.0000  0.0000     13     no  cohort 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 18) = 123.5732, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 7.8016, p-val = 0.0052
    ## 
    ## Model Results:
    ## 
    ##          estimate       se     zval    pval      ci.lb     ci.ub     ​ 
    ## intrcpt    0.5247   0.0675   7.7740  <.0001     0.3924    0.6570  *** 
    ## mods     -69.3919  24.8438  -2.7931  0.0052  -118.0848  -20.6990   ** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Make funnel plot to observe the relationship between sample size and agreement between subjective and objective measures
funnel(x=data$r_obj_subj[data$exposure_type_broad == "bullying"], yaxis="ni", ni=data$n_total[data$exposure_type_broad == "bullying"], xlab="Correlation") #label your x-axis according to the effect size)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

##### 1.8 Leave one out analysis

###### 1.8.1 Maltreatment - Kappa

``` r
groupids <- unique(data$author[data$exposure_type_broad=="maltreatment"])
groupids <- na.omit(groupids)

leave1out_study <- matrix(rep(NA, length(groupids)), ncol=6, nrow=length(groupids))  

for(i in 1:length(groupids)) {  
  dataexcl <- subset(data, exposure_type_broad=="maltreatment") # subset to maltreatment data
  dataexcl <- subset(dataexcl, author!=groupids[i])  # remove each reference
  print(nrow(dataexcl))
  
  N_rows <- psych::describe(dataexcl$n_total)$n
  mean_N <- psych::describe(dataexcl$n_total)$mean

  # Run meta-analysis for maltreatment obj and subj measures 
  maltreat_kappa <- rma.mv(es_agree, kappa_var, data=dataexcl, random=list(~ 1 | es_id, ~ 1 | author, ~1 | cohort),
                           subset = exposure_type_broad == "maltreatment" & es_type=="kappa")
  
  # Extract results
  leave1out_study[i,1] <- i # number study omitted
  leave1out_study[i,2] <- N_rows 
  leave1out_study[i,3] <- mean_N 
  leave1out_study[i,4] <- maltreat_kappa$beta[1] #  ES
  leave1out_study[i,5] <- maltreat_kappa$ci.lb[1] # low CI
  leave1out_study[i,6] <- maltreat_kappa$ci.ub[1] # upper CI

  
}
```

    ## [1] 37
    ## [1] 36
    ## [1] 38
    ## [1] 37
    ## [1] 36
    ## [1] 40
    ## [1] 33
    ## [1] 38
    ## [1] 33

``` r
leave1out_study
```

    ##       [,1] [,2]     [,3]      [,4]       [,5]      [,6]
    ##  [1,]    1   37 385.0000 0.1525528 0.08254152 0.2225640
    ##  [2,]    2   36 395.0278 0.1670185 0.10814966 0.2258873
    ##  [3,]    3   38 447.9737 0.1590972 0.09593528 0.2222592
    ##  [4,]    4   37 464.9459 0.1634658 0.09135114 0.2355805
    ##  [5,]    5   36 479.7500 0.1321958 0.09244323 0.1719484
    ##  [6,]    6   40 430.6250 0.1555149 0.09010073 0.2209291
    ##  [7,]    7   33 466.9697 0.1664345 0.09316573 0.2397033
    ##  [8,]    8   38 453.0526 0.1675109 0.09844593 0.2365758
    ##  [9,]    9   33 447.0909 0.1662398 0.09301429 0.2394654

``` r
leave1out_study[,1] <- as.vector(groupids) # Name studies
leave1out_study_df <- as.data.frame(leave1out_study) # Convert to dataframe
names(leave1out_study_df) <-c('author', 'N_rows', 'Mean_N', 
                              'es', 'ci.lb', 'ci.ub') # Rename columns
# Convert columns from character to numeric
leave1out_study_df <- leave1out_study_df %>% mutate_at(c('N_rows', 'Mean_N', 
                                                         'es', 'ci.lb', 'ci.ub'), as.numeric)

# Check studies that when removed, led to the minimum and maximum effect size
# maltreatment obj and subj agreement meta-analysis
leave1out_study_df[which.min(leave1out_study_df$es),c('author','es', 'ci.lb', 'ci.ub')]
```

    ##          author        es      ci.lb     ci.ub
    ## 5 McGee et al., 0.1321958 0.09244323 0.1719484

``` r
leave1out_study_df[which.max(leave1out_study_df$es),c('author','es', 'ci.lb', 'ci.ub')]
```

    ##        author        es      ci.lb     ci.ub
    ## 8 Cho et al., 0.1675109 0.09844593 0.2365758

###### 1.8.2 Maltreatment - Correlations

``` r
groupids <- unique(data$author[data$exposure_type_broad=="maltreatment"])
groupids <- na.omit(groupids)

leave1out_study <- matrix(rep(NA, length(groupids)), ncol=6, nrow=length(groupids))  

for(i in 1:length(groupids)) {  
  dataexcl <- subset(data, exposure_type_broad=="maltreatment") # subset to maltreatment data
  dataexcl <- subset(dataexcl, author!=groupids[i])  
  print(nrow(dataexcl))
  
  N_rows <- psych::describe(dataexcl$n_total)$n
  mean_N <- psych::describe(dataexcl$n_total)$mean
  # Run meta-analysis for maltreatment obj and subj measures
  maltreat_z <- rma.mv(z_obj_subj, z_obj_subj_var, data=dataexcl, random=list(~ 1 | es_id, ~ 1 | author, ~1 | cohort),
                       subset = exposure_type_broad == "maltreatment")
  
  # Extract results
  leave1out_study[i,1] <- i # number study omitted
  leave1out_study[i,2] <- N_rows 
  leave1out_study[i,3] <- mean_N 
  leave1out_study[i,4] <- fisherz2r(maltreat_z$beta[1]) #  ES
  leave1out_study[i,5] <- fisherz2r(maltreat_z$ci.lb[1]) # low CI
  leave1out_study[i,6] <- fisherz2r(maltreat_z$ci.ub[1]) # upper CI
  
}
```

    ## [1] 37
    ## [1] 36
    ## [1] 38
    ## [1] 37
    ## [1] 36
    ## [1] 40
    ## [1] 33
    ## [1] 38
    ## [1] 33

``` r
leave1out_study
```

    ##       [,1] [,2]     [,3]      [,4]      [,5]      [,6]
    ##  [1,]    1   37 385.0000 0.3100606 0.2125710 0.4014384
    ##  [2,]    2   36 395.0278 0.3439766 0.2542596 0.4278239
    ##  [3,]    3   38 447.9737 0.3137880 0.2164881 0.4049157
    ##  [4,]    4   37 464.9459 0.3319462 0.2303910 0.4263484
    ##  [5,]    5   36 479.7500 0.2895845 0.2221778 0.3542363
    ##  [6,]    6   40 430.6250 0.3207229 0.2269490 0.4086046
    ##  [7,]    7   33 466.9697 0.3328618 0.2235354 0.4339169
    ##  [8,]    8   38 453.0526 0.3337337 0.2363698 0.4244625
    ##  [9,]    9   33 447.0909 0.3343408 0.2255735 0.4348759

``` r
leave1out_study[,1] <- as.vector(groupids) # Name studies
leave1out_study_df <- as.data.frame(leave1out_study) # Convert to dataframe
names(leave1out_study_df) <-c('author', 'N_rows', 'Mean_N', 
                              'es', 'ci.lb', 'ci.ub') # Rename columns
# Convert columns from character to numeric
leave1out_study_df <- leave1out_study_df %>% mutate_at(c('N_rows', 'Mean_N', 
                                                         'es', 'ci.lb', 'ci.ub'), as.numeric)

# Check studies that when removed, led to the minimum and maximum effect size
# maltreatment obj and subj agreement meta-analysis
leave1out_study_df[which.min(leave1out_study_df$es),c('author','es', 'ci.lb', 'ci.ub')]
```

    ##          author        es     ci.lb     ci.ub
    ## 5 McGee et al., 0.2895845 0.2221778 0.3542363

``` r
leave1out_study_df[which.max(leave1out_study_df$es),c('author','es', 'ci.lb', 'ci.ub')]
```

    ##          author        es     ci.lb     ci.ub
    ## 2 White et al., 0.3439766 0.2542596 0.4278239

###### 1.8.3 Bullying

``` r
groupids <- unique(data$author[data$exposure_type_broad=="bullying"])
groupids <- na.omit(groupids)

leave1out_study <- matrix(rep(NA, length(groupids)), ncol=6, nrow=length(groupids))  

for(i in 1:length(groupids)) {  
  dataexcl <- subset(data, exposure_type_broad=="bullying") # subset to bullying data
  dataexcl <- subset(dataexcl, author!=groupids[i])  
  print(nrow(dataexcl))
  
  N_rows <- psych::describe(dataexcl$n_total)$n
  mean_N <- psych::describe(dataexcl$n_total)$mean
  # Run meta-analysis for bullying obj and subj measures
  maltreat_z <- rma.mv(z_obj_subj, z_obj_subj_var, data=dataexcl, random=list(~ 1 | es_id, ~ 1 | author, ~1 | cohort),
                       subset = exposure_type_broad == "bullying")
  
  # Extract results
  leave1out_study[i,1] <- i # number study omitted
  leave1out_study[i,2] <- N_rows 
  leave1out_study[i,3] <- mean_N 
  leave1out_study[i,4] <- fisherz2r(maltreat_z$beta[1]) #  ES
  leave1out_study[i,5] <- fisherz2r(maltreat_z$ci.lb[1]) # low CI
  leave1out_study[i,6] <- fisherz2r(maltreat_z$ci.ub[1]) # upper CI
  
  
}
```

    ## [1] 19
    ## [1] 19
    ## [1] 19
    ## [1] 19
    ## [1] 19
    ## [1] 16
    ## [1] 18
    ## [1] 18
    ## [1] 16
    ## [1] 19
    ## [1] 18

``` r
leave1out_study
```

    ##       [,1] [,2]     [,3]      [,4]      [,5]      [,6]
    ##  [1,]    1   19 534.4211 0.3212353 0.2572112 0.3824517
    ##  [2,]    2   19 552.9474 0.3548524 0.2715294 0.4328955
    ##  [3,]    3   19 507.8421 0.3374734 0.2531301 0.4167253
    ##  [4,]    4   19 548.5789 0.3500089 0.2644971 0.4300598
    ##  [5,]    5   19 530.0526 0.3525451 0.2675277 0.4321157
    ##  [6,]    6   16 561.6875 0.3553372 0.2679287 0.4369438
    ##  [7,]    7   18 441.4444 0.3356233 0.2510023 0.4151534
    ##  [8,]    8   18 550.2222 0.3330346 0.2506427 0.4106366
    ##  [9,]    9   16 617.3125 0.3569104 0.2737286 0.4347928
    ## [10,]   10   19 550.4211 0.3557912 0.2729176 0.4334232
    ## [11,]   11   18 579.7222 0.3612806 0.2825932 0.4351383

``` r
leave1out_study[,1] <- as.vector(groupids) # Name studies
leave1out_study_df <- as.data.frame(leave1out_study) # Convert to dataframe
names(leave1out_study_df) <-c('author', 'N_rows', 'Mean_N', 
                              'es', 'ci.lb', 'ci.ub') # Rename columns
# Convert columns from character to numeric
leave1out_study_df <- leave1out_study_df %>% mutate_at(c('N_rows', 'Mean_N', 
                                                         'es', 'ci.lb', 'ci.ub'), as.numeric)

# Check studies that when removed, led to the minimum and maximum effect size
# bullying obj and subj agreement meta-analysis
leave1out_study_df[which.min(leave1out_study_df$es),c('author','es', 'ci.lb', 'ci.ub')]
```

    ##            author        es     ci.lb     ci.ub
    ## 1 Gromann et al., 0.3212353 0.2572112 0.3824517

``` r
leave1out_study_df[which.max(leave1out_study_df$es),c('author','es', 'ci.lb', 'ci.ub')]
```

    ##                        author        es     ci.lb     ci.ub
    ## 11 De Los Reyes & Prinstein., 0.3612806 0.2825932 0.4351383

#### 2. Meta analysis of objective and subjective experiences of childhood adversity and their association with psychopathology

##### 2.1 Examine data

``` r
# Load packages, define paths, read in data
library("readxl") # read in Excel data
library(dplyr) # for data manipulation
library(metafor) # for meta-analysis
library(mgsub) #for manipulation of string variables
library(psych)

# Define paths for working directories 
Data <- "/Users/emma/Desktop"

# Read in data 
setwd(Data)
data <- data.frame(read_excel("Data-extraction_june22.xlsx"))
head(data)
```

    ##             Author Year        Cohort Country percent_female N_Included
    ## 1 Danese & Widom., 2020 Widom_Midwest   U.S.A           48.7       1196
    ## 2 Danese & Widom., 2020 Widom_Midwest   U.S.A           48.7       1196
    ## 3 Danese & Widom., 2020 Widom_Midwest   U.S.A           48.7       1196
    ## 4 Danese & Widom., 2020 Widom_Midwest   U.S.A           48.7       1196
    ## 5 Danese & Widom., 2020 Widom_Midwest   U.S.A           48.7       1196
    ## 6 Danese & Widom., 2020 Widom_Midwest   U.S.A           48.7       1196
    ##   Subj_vs_none Obj_vs_none  Exposure_Type_Obj Exposure_Type_Subj
    ## 1          514         435 Child_Maltreatment Child_Maltreatment
    ## 2          514         435 Child_Maltreatment Child_Maltreatment
    ## 3          514         435 Child_Maltreatment Child_Maltreatment
    ## 4          514         435 Child_Maltreatment Child_Maltreatment
    ## 5          514         435 Child_Maltreatment Child_Maltreatment
    ## 6          514         435 Child_Maltreatment Child_Maltreatment
    ##   Obj_measure_type Obj_type_neigh     Subj_measure_type Subj_type_neigh
    ## 1    Crime_records           <NA> self_report_interview            <NA>
    ## 2    Crime_records           <NA> self_report_interview            <NA>
    ## 3    Crime_records           <NA> self_report_interview            <NA>
    ## 4    Crime_records           <NA> self_report_interview            <NA>
    ## 5    Crime_records           <NA> self_report_interview            <NA>
    ## 6    Crime_records           <NA> self_report_interview            <NA>
    ##   Obj_Obs_Period_ACE Subj_Obs_Period_ACE Age_subj_Assessment_ACE Age_self
    ## 1               0-12                0-12                    28.7     28.7
    ## 2               0-12                0-12                    28.7     28.7
    ## 3               0-12                0-12                    28.7     28.7
    ## 4               0-12                0-12                    28.7     28.7
    ## 5               0-12                0-12                    28.7     28.7
    ## 6               0-12                0-12                    28.7     28.7
    ##   Type_of_obj_measure Type_of_subj_measure
    ## 1              binary               binary
    ## 2              binary               binary
    ## 3              binary               binary
    ## 4              binary               binary
    ## 5              binary               binary
    ## 6              binary               binary
    ##                               MH_Outcome                    Measure_MH
    ## 1          Any_Psychopathology_Diagnosis diagnostic_interview_schedule
    ## 2   Any_Internalising_Disorder_Diagnosis diagnostic_interview_schedule
    ## 3   Any_Externalising_Disorder_Diagnosis diagnostic_interview_schedule
    ## 4                   Depression_Diagnosis diagnostic_interview_schedule
    ## 5                    Dysthymia_Diagnosis diagnostic_interview_schedule
    ## 6 Generalized_Anxiety_Disorder_Diagnosis diagnostic_interview_schedule
    ##   Informant_MH Age_Assessment_MH Type_of_psychopathology_measure SD_Outcome
    ## 1  self-report              28.7                          binary         NA
    ## 2  self-report              28.7                          binary         NA
    ## 3  self-report              28.7                          binary         NA
    ## 4  self-report              28.7                          binary         NA
    ## 5  self-report              28.7                          binary         NA
    ## 6  self-report              28.7                          binary         NA
    ##   prevalence_unexposed Type_of_effect_size_obj Effect_size_obj     SE_obj
    ## 1                 0.61              risk_ratio            0.92 0.08301592
    ## 2                 0.26              risk_ratio            0.87 0.17267517
    ## 3                 0.51              risk_ratio            0.96 0.09799549
    ## 4                 0.15              risk_ratio            0.63 0.27089564
    ## 5                 0.06              risk_ratio            0.91 0.40806988
    ## 6                 0.05              risk_ratio            0.83 0.49874605
    ##   Std_obj    CI_obj p_value_obj Type_of_effect_size_subj Effect_size_subj
    ## 1      NA 0.78-1.08          NA               risk_ratio             1.29
    ## 2      NA 0.62-1.22          NA               risk_ratio             1.73
    ## 3      NA 0.79-1.16          NA               risk_ratio             1.27
    ## 4      NA 0.37-1.07          NA               risk_ratio             1.67
    ## 5      NA 0.41-2.03          NA               risk_ratio             1.87
    ## 6      NA 0.31-2.19          NA               risk_ratio             1.32
    ##     SE_subj Std_subj   CI_subj p_value_subj Q_Exposed_Rep Q_Control_Selec
    ## 1 0.3359100       NA 1.15-1.45           NA             0               1
    ## 2 0.7164352       NA 1.35-2.21           NA             0               1
    ## 3 0.3677283       NA 1.10-1.48           NA             0               1
    ## 4 0.8206669       NA 1.18-2.37           NA             0               1
    ## 5 1.2275086       NA 1.02-3.43           NA             0               1
    ## 6 1.1769176       NA 0.61-2.86           NA             0               1
    ##   Q_Exposure_Subj Q_Exposure_type_comparison Q_exposure_time_comparison
    ## 1               1                          1                          1
    ## 2               1                          1                          1
    ## 3               1                          1                          1
    ## 4               1                          1                          1
    ## 5               1                          1                          1
    ## 6               1                          1                          1
    ##   Q_MH_Control Q_Confound Q_Longitud Q_total
    ## 1            0          1          0       5
    ## 2            0          1          0       5
    ## 3            0          1          0       5
    ## 4            0          1          0       5
    ## 5            0          1          0       5
    ## 6            0          1          0       5

``` r
str(data)
```

    ## 'data.frame':    141 obs. of  48 variables:
    ##  $ Author                         : chr  "Danese & Widom.," "Danese & Widom.," "Danese & Widom.," "Danese & Widom.," ...
    ##  $ Year                           : num  2020 2020 2020 2020 2020 2020 2020 2020 2020 2020 ...
    ##  $ Cohort                         : chr  "Widom_Midwest" "Widom_Midwest" "Widom_Midwest" "Widom_Midwest" ...
    ##  $ Country                        : chr  "U.S.A" "U.S.A" "U.S.A" "U.S.A" ...
    ##  $ percent_female                 : num  48.7 48.7 48.7 48.7 48.7 48.7 48.7 48.7 48.7 48.7 ...
    ##  $ N_Included                     : num  1196 1196 1196 1196 1196 ...
    ##  $ Subj_vs_none                   : num  514 514 514 514 514 514 514 514 514 514 ...
    ##  $ Obj_vs_none                    : num  435 435 435 435 435 435 435 435 435 435 ...
    ##  $ Exposure_Type_Obj              : chr  "Child_Maltreatment" "Child_Maltreatment" "Child_Maltreatment" "Child_Maltreatment" ...
    ##  $ Exposure_Type_Subj             : chr  "Child_Maltreatment" "Child_Maltreatment" "Child_Maltreatment" "Child_Maltreatment" ...
    ##  $ Obj_measure_type               : chr  "Crime_records" "Crime_records" "Crime_records" "Crime_records" ...
    ##  $ Obj_type_neigh                 : chr  NA NA NA NA ...
    ##  $ Subj_measure_type              : chr  "self_report_interview" "self_report_interview" "self_report_interview" "self_report_interview" ...
    ##  $ Subj_type_neigh                : chr  NA NA NA NA ...
    ##  $ Obj_Obs_Period_ACE             : chr  "0-12" "0-12" "0-12" "0-12" ...
    ##  $ Subj_Obs_Period_ACE            : chr  "0-12" "0-12" "0-12" "0-12" ...
    ##  $ Age_subj_Assessment_ACE        : chr  "28.7" "28.7" "28.7" "28.7" ...
    ##  $ Age_self                       : num  28.7 28.7 28.7 28.7 28.7 28.7 28.7 28.7 28.7 28.7 ...
    ##  $ Type_of_obj_measure            : chr  "binary" "binary" "binary" "binary" ...
    ##  $ Type_of_subj_measure           : chr  "binary" "binary" "binary" "binary" ...
    ##  $ MH_Outcome                     : chr  "Any_Psychopathology_Diagnosis" "Any_Internalising_Disorder_Diagnosis" "Any_Externalising_Disorder_Diagnosis" "Depression_Diagnosis" ...
    ##  $ Measure_MH                     : chr  "diagnostic_interview_schedule" "diagnostic_interview_schedule" "diagnostic_interview_schedule" "diagnostic_interview_schedule" ...
    ##  $ Informant_MH                   : chr  "self-report" "self-report" "self-report" "self-report" ...
    ##  $ Age_Assessment_MH              : num  28.7 28.7 28.7 28.7 28.7 28.7 28.7 28.7 28.7 28.7 ...
    ##  $ Type_of_psychopathology_measure: chr  "binary" "binary" "binary" "binary" ...
    ##  $ SD_Outcome                     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ prevalence_unexposed           : num  0.61 0.26 0.51 0.15 0.06 0.05 0.14 0.03 0.46 0.28 ...
    ##  $ Type_of_effect_size_obj        : chr  "risk_ratio" "risk_ratio" "risk_ratio" "risk_ratio" ...
    ##  $ Effect_size_obj                : num  0.92 0.87 0.96 0.63 0.91 0.83 1.15 2.19 0.97 0.56 ...
    ##  $ SE_obj                         : num  0.083 0.173 0.098 0.271 0.408 ...
    ##  $ Std_obj                        : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ CI_obj                         : chr  "0.78-1.08" "0.62-1.22" "0.79-1.16" "0.37-1.07" ...
    ##  $ p_value_obj                    : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Type_of_effect_size_subj       : chr  "risk_ratio" "risk_ratio" "risk_ratio" "risk_ratio" ...
    ##  $ Effect_size_subj               : num  1.29 1.73 1.27 1.67 1.87 1.32 1.94 5.66 1.24 1.44 ...
    ##  $ SE_subj                        : num  0.336 0.716 0.368 0.821 1.228 ...
    ##  $ Std_subj                       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ CI_subj                        : chr  "1.15-1.45" "1.35-2.21" "1.10-1.48" "1.18-2.37" ...
    ##  $ p_value_subj                   : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Q_Exposed_Rep                  : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Q_Control_Selec                : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Q_Exposure_Subj                : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Q_Exposure_type_comparison     : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Q_exposure_time_comparison     : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Q_MH_Control                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Q_Confound                     : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Q_Longitud                     : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Q_total                        : num  5 5 5 5 5 5 5 5 5 5 ...

##### 2.2 Derive new variables

``` r
# Derive broad exposure variable for bullying, maltreatment, neighbourhood disadvantage 
table(data$Exposure_Type_Subj)
```

    ## 
    ##        Acceptance_And_Self-Esteem                          Autonomy 
    ##                                 3                                 3 
    ##            Bullying_Victimization         Bullying_Victimization_T1 
    ##                                27                                 2 
    ##         Bullying_Victimization_T2                Child_Maltreatment 
    ##                                 1                                10 
    ##                     Child_Neglect              Child_Physical_Abuse 
    ##                                 3                                 3 
    ##                Child_Sexual_Abuse                         Emotional 
    ##                                 3                                 4 
    ##                   Emotional_Abuse                   Family_Violence 
    ##                                 9                                 4 
    ##                           Neglect            Neighbourhood_Disorder 
    ##                                11                                 1 
    ##              Neighbourhood_Safety                Peer_Victimization 
    ##                                 1                                15 
    ##                    Physical_Abuse                 Physical_Violence 
    ##                                12                                 4 
    ##               Psychological_Abuse Psychological_Safety_And_Security 
    ##                                 3                                 3 
    ##                       Restriction                            Sexual 
    ##                                 3                                 4 
    ##                      Sexual_Abuse 
    ##                                12

``` r
# Bullying
data$exposure_type_broad <- NA
data$exposure_type_broad[data$Exposure_Type_Subj=="Bullying_Victimization_T1" |
                           data$Exposure_Type_Subj=="Bullying_Victimization_T2"  |
                           data$Exposure_Type_Subj=="Bullying_Victimization" | 
                           data$Exposure_Type_Subj=="Peer_Victimization"] <- "bullying"
# Maltreatment
data$exposure_type_broad[data$Exposure_Type_Subj=="Acceptance_And_Self-Esteem" |
                           data$Exposure_Type_Subj=="Autonomy"|
                           data$Exposure_Type_Subj=="Child_Maltreatment" | 
                           data$Exposure_Type_Subj=="Child_Neglect" |
                           data$Exposure_Type_Subj=="Child_Physical_Abuse"|
                           data$Exposure_Type_Subj=="Child_Sexual_Abuse" | 
                           data$Exposure_Type_Subj=="Emotional" |
                           data$Exposure_Type_Subj=="Emotional_Abuse"|
                           data$Exposure_Type_Subj=="Family_Violence" | 
                           data$Exposure_Type_Subj=="Neglect"|
                           data$Exposure_Type_Subj=="Physical_Abuse" | 
                           data$Exposure_Type_Subj=="Physical_Violence" |
                           data$Exposure_Type_Subj=="Psychological_Abuse"|
                          data$Exposure_Type_Subj=="Psychological_Safety_And_Security"|
                           data$Exposure_Type_Subj=="Restriction" |
                           data$Exposure_Type_Subj=="Sexual"|
                           data$Exposure_Type_Subj=="Sexual_Abuse"] <- "maltreatment"

# Neighbourhood 
data$exposure_type_broad[data$Exposure_Type_Subj=="Neighbourhood_Disorder" |
                           data$Exposure_Type_Subj=="Neighbourhood_Safety"] <- "neighbourhood"
table(data$exposure_type_broad)
```

    ## 
    ##      bullying  maltreatment neighbourhood 
    ##            45            94             2

``` r
#check any outcomes that haven't been coded - should be 0
data %>% dplyr::filter (is.na(exposure_type_broad)) %>%
  dplyr::select(Author, Exposure_Type_Subj)
```

    ## [1] Author             Exposure_Type_Subj
    ## <0 rows> (or 0-length row.names)

``` r
# Compare original coding to new broad categories of adversity type
data %>%  dplyr::select(Author, Exposure_Type_Subj, exposure_type_broad)
```

    ##                     Author                Exposure_Type_Subj
    ## 1         Danese & Widom.,                Child_Maltreatment
    ## 2         Danese & Widom.,                Child_Maltreatment
    ## 3         Danese & Widom.,                Child_Maltreatment
    ## 4         Danese & Widom.,                Child_Maltreatment
    ## 5         Danese & Widom.,                Child_Maltreatment
    ## 6         Danese & Widom.,                Child_Maltreatment
    ## 7         Danese & Widom.,                Child_Maltreatment
    ## 8         Danese & Widom.,                Child_Maltreatment
    ## 9         Danese & Widom.,                Child_Maltreatment
    ## 10        Danese & Widom.,                Child_Maltreatment
    ## 11        Danese & Widom.,              Child_Physical_Abuse
    ## 12        Danese & Widom.,              Child_Physical_Abuse
    ## 13        Danese & Widom.,              Child_Physical_Abuse
    ## 14        Danese & Widom.,                Child_Sexual_Abuse
    ## 15        Danese & Widom.,                Child_Sexual_Abuse
    ## 16        Danese & Widom.,                Child_Sexual_Abuse
    ## 17        Danese & Widom.,                     Child_Neglect
    ## 18        Danese & Widom.,                     Child_Neglect
    ## 19        Danese & Widom.,                     Child_Neglect
    ## 20         Gromann et al.,            Bullying_Victimization
    ## 21  Zimmer-Gembeck et al.,            Bullying_Victimization
    ## 22  Zimmer-Gembeck et al.,            Bullying_Victimization
    ## 23          Bouman et al.,            Bullying_Victimization
    ## 24          Bouman et al.,            Bullying_Victimization
    ## 25      Graham & Juvonen.,            Bullying_Victimization
    ## 26         Graham, et al.,            Bullying_Victimization
    ## 27         Graham, et al.,            Bullying_Victimization
    ## 28         Graham, et al.,            Bullying_Victimization
    ## 29         Graham, et al.,            Bullying_Victimization
    ## 30           White et al., Psychological_Safety_And_Security
    ## 31           White et al.,        Acceptance_And_Self-Esteem
    ## 32           White et al.,                          Autonomy
    ## 33           White et al.,                       Restriction
    ## 34           White et al., Psychological_Safety_And_Security
    ## 35           White et al.,        Acceptance_And_Self-Esteem
    ## 36           White et al.,                          Autonomy
    ## 37           White et al.,                       Restriction
    ## 38           White et al., Psychological_Safety_And_Security
    ## 39           White et al.,        Acceptance_And_Self-Esteem
    ## 40           White et al.,                          Autonomy
    ## 41           White et al.,                       Restriction
    ## 42         Newbury et al.,            Neighbourhood_Disorder
    ## 43  Goldman-Mellor et al.,              Neighbourhood_Safety
    ## 44           McGee et al.,                 Physical_Violence
    ## 45           McGee et al.,                 Physical_Violence
    ## 46           McGee et al.,                 Physical_Violence
    ## 47           McGee et al.,                 Physical_Violence
    ## 48           McGee et al.,                   Family_Violence
    ## 49           McGee et al.,                   Family_Violence
    ## 50           McGee et al.,                   Family_Violence
    ## 51           McGee et al.,                   Family_Violence
    ## 52           McGee et al.,                            Sexual
    ## 53           McGee et al.,                            Sexual
    ## 54           McGee et al.,                            Sexual
    ## 55           McGee et al.,                            Sexual
    ## 56           McGee et al.,                         Emotional
    ## 57           McGee et al.,                         Emotional
    ## 58           McGee et al.,                         Emotional
    ## 59           McGee et al.,                         Emotional
    ## 60           McGee et al.,                           Neglect
    ## 61           McGee et al.,                           Neglect
    ## 62           McGee et al.,                           Neglect
    ## 63           McGee et al.,                           Neglect
    ## 64          Kochel et al.,                Peer_Victimization
    ## 65          Kochel et al.,                Peer_Victimization
    ## 66          Kochel et al.,                Peer_Victimization
    ## 67          Kochel et al.,                Peer_Victimization
    ## 68          Kochel et al.,                Peer_Victimization
    ## 69          Kochel et al.,                Peer_Victimization
    ## 70          Kochel et al.,                Peer_Victimization
    ## 71          Kochel et al.,                Peer_Victimization
    ## 72          Kochel et al.,                Peer_Victimization
    ## 73          Kochel et al.,                Peer_Victimization
    ## 74          Kochel et al.,                Peer_Victimization
    ## 75          Kochel et al.,                Peer_Victimization
    ## 76          Kochel et al.,                Peer_Victimization
    ## 77          Kochel et al.,                Peer_Victimization
    ## 78          Kochel et al.,                Peer_Victimization
    ## 79          Mulder et al.,         Bullying_Victimization_T1
    ## 80          Mulder et al.,         Bullying_Victimization_T2
    ## 81          Mulder et al.,         Bullying_Victimization_T1
    ## 82         Mcclain et al.,            Bullying_Victimization
    ## 83         Mcclain et al.,            Bullying_Victimization
    ## 84         Mcclain et al.,            Bullying_Victimization
    ## 85         Mcclain et al.,            Bullying_Victimization
    ## 86         Mcclain et al.,            Bullying_Victimization
    ## 87         Mcclain et al.,            Bullying_Victimization
    ## 88         Mcclain et al.,            Bullying_Victimization
    ## 89         Mcclain et al.,            Bullying_Victimization
    ## 90         Mcclain et al.,            Bullying_Victimization
    ## 91         Mcclain et al.,            Bullying_Victimization
    ## 92         Mcclain et al.,            Bullying_Victimization
    ## 93         Mcclain et al.,            Bullying_Victimization
    ## 94         Mcclain et al.,            Bullying_Victimization
    ## 95         Mcclain et al.,            Bullying_Victimization
    ## 96         Mcclain et al.,            Bullying_Victimization
    ## 97         Mcclain et al.,            Bullying_Victimization
    ## 98         Negriff et al.,                      Sexual_Abuse
    ## 99         Negriff et al.,                      Sexual_Abuse
    ## 100        Negriff et al.,                      Sexual_Abuse
    ## 101        Negriff et al.,                      Sexual_Abuse
    ## 102        Negriff et al.,                      Sexual_Abuse
    ## 103        Negriff et al.,                      Sexual_Abuse
    ## 104        Negriff et al.,                      Sexual_Abuse
    ## 105        Negriff et al.,                    Physical_Abuse
    ## 106        Negriff et al.,                    Physical_Abuse
    ## 107        Negriff et al.,                    Physical_Abuse
    ## 108        Negriff et al.,                    Physical_Abuse
    ## 109        Negriff et al.,                    Physical_Abuse
    ## 110        Negriff et al.,                    Physical_Abuse
    ## 111        Negriff et al.,                    Physical_Abuse
    ## 112        Negriff et al.,                   Emotional_Abuse
    ## 113        Negriff et al.,                   Emotional_Abuse
    ## 114        Negriff et al.,                   Emotional_Abuse
    ## 115        Negriff et al.,                   Emotional_Abuse
    ## 116        Negriff et al.,                   Emotional_Abuse
    ## 117        Negriff et al.,                   Emotional_Abuse
    ## 118        Negriff et al.,                   Emotional_Abuse
    ## 119        Negriff et al.,                           Neglect
    ## 120        Negriff et al.,                           Neglect
    ## 121        Negriff et al.,                           Neglect
    ## 122        Negriff et al.,                           Neglect
    ## 123        Negriff et al.,                           Neglect
    ## 124        Negriff et al.,                           Neglect
    ## 125        Negriff et al.,                           Neglect
    ## 126        Everson et al.,                    Physical_Abuse
    ## 127        Everson et al.,                    Physical_Abuse
    ## 128        Everson et al.,                    Physical_Abuse
    ## 129        Everson et al.,                      Sexual_Abuse
    ## 130        Everson et al.,                      Sexual_Abuse
    ## 131        Everson et al.,                      Sexual_Abuse
    ## 132        Everson et al.,               Psychological_Abuse
    ## 133        Everson et al.,               Psychological_Abuse
    ## 134        Everson et al.,               Psychological_Abuse
    ## 135       Flanagan et al.,            Bullying_Victimization
    ## 136            Cho et al.,                    Physical_Abuse
    ## 137            Cho et al.,                    Physical_Abuse
    ## 138            Cho et al.,                      Sexual_Abuse
    ## 139            Cho et al.,                      Sexual_Abuse
    ## 140            Cho et al.,                   Emotional_Abuse
    ## 141            Cho et al.,                   Emotional_Abuse
    ##     exposure_type_broad
    ## 1          maltreatment
    ## 2          maltreatment
    ## 3          maltreatment
    ## 4          maltreatment
    ## 5          maltreatment
    ## 6          maltreatment
    ## 7          maltreatment
    ## 8          maltreatment
    ## 9          maltreatment
    ## 10         maltreatment
    ## 11         maltreatment
    ## 12         maltreatment
    ## 13         maltreatment
    ## 14         maltreatment
    ## 15         maltreatment
    ## 16         maltreatment
    ## 17         maltreatment
    ## 18         maltreatment
    ## 19         maltreatment
    ## 20             bullying
    ## 21             bullying
    ## 22             bullying
    ## 23             bullying
    ## 24             bullying
    ## 25             bullying
    ## 26             bullying
    ## 27             bullying
    ## 28             bullying
    ## 29             bullying
    ## 30         maltreatment
    ## 31         maltreatment
    ## 32         maltreatment
    ## 33         maltreatment
    ## 34         maltreatment
    ## 35         maltreatment
    ## 36         maltreatment
    ## 37         maltreatment
    ## 38         maltreatment
    ## 39         maltreatment
    ## 40         maltreatment
    ## 41         maltreatment
    ## 42        neighbourhood
    ## 43        neighbourhood
    ## 44         maltreatment
    ## 45         maltreatment
    ## 46         maltreatment
    ## 47         maltreatment
    ## 48         maltreatment
    ## 49         maltreatment
    ## 50         maltreatment
    ## 51         maltreatment
    ## 52         maltreatment
    ## 53         maltreatment
    ## 54         maltreatment
    ## 55         maltreatment
    ## 56         maltreatment
    ## 57         maltreatment
    ## 58         maltreatment
    ## 59         maltreatment
    ## 60         maltreatment
    ## 61         maltreatment
    ## 62         maltreatment
    ## 63         maltreatment
    ## 64             bullying
    ## 65             bullying
    ## 66             bullying
    ## 67             bullying
    ## 68             bullying
    ## 69             bullying
    ## 70             bullying
    ## 71             bullying
    ## 72             bullying
    ## 73             bullying
    ## 74             bullying
    ## 75             bullying
    ## 76             bullying
    ## 77             bullying
    ## 78             bullying
    ## 79             bullying
    ## 80             bullying
    ## 81             bullying
    ## 82             bullying
    ## 83             bullying
    ## 84             bullying
    ## 85             bullying
    ## 86             bullying
    ## 87             bullying
    ## 88             bullying
    ## 89             bullying
    ## 90             bullying
    ## 91             bullying
    ## 92             bullying
    ## 93             bullying
    ## 94             bullying
    ## 95             bullying
    ## 96             bullying
    ## 97             bullying
    ## 98         maltreatment
    ## 99         maltreatment
    ## 100        maltreatment
    ## 101        maltreatment
    ## 102        maltreatment
    ## 103        maltreatment
    ## 104        maltreatment
    ## 105        maltreatment
    ## 106        maltreatment
    ## 107        maltreatment
    ## 108        maltreatment
    ## 109        maltreatment
    ## 110        maltreatment
    ## 111        maltreatment
    ## 112        maltreatment
    ## 113        maltreatment
    ## 114        maltreatment
    ## 115        maltreatment
    ## 116        maltreatment
    ## 117        maltreatment
    ## 118        maltreatment
    ## 119        maltreatment
    ## 120        maltreatment
    ## 121        maltreatment
    ## 122        maltreatment
    ## 123        maltreatment
    ## 124        maltreatment
    ## 125        maltreatment
    ## 126        maltreatment
    ## 127        maltreatment
    ## 128        maltreatment
    ## 129        maltreatment
    ## 130        maltreatment
    ## 131        maltreatment
    ## 132        maltreatment
    ## 133        maltreatment
    ## 134        maltreatment
    ## 135            bullying
    ## 136        maltreatment
    ## 137        maltreatment
    ## 138        maltreatment
    ## 139        maltreatment
    ## 140        maltreatment
    ## 141        maltreatment

``` r
# Derive variable for internalizing mental health outcomes depression, anxiety, eating disorder, self-harm, suicide)
table(data$MH_Outcome)
```

    ## 
    ## Alcohol_Abuse_And_Or_Dependence_Diagnosis 
    ##                                         1 
    ##                               Alcohol_Use 
    ##                                         4 
    ## Antisocial_Personality_Disorder_Diagnosis 
    ##                                         1 
    ##                          Anxiety_Symptoms 
    ##                                        10 
    ##                       Anxiety_Symptoms_T1 
    ##                                         4 
    ##                       Anxiety_Symptoms_T2 
    ##                                         4 
    ##      Any_Externalising_Disorder_Diagnosis 
    ##                                         4 
    ##      Any_Internalising_Disorder_Diagnosis 
    ##                                         4 
    ##             Any_Psychopathology_Diagnosis 
    ##                                         4 
    ##                      Depression_Diagnosis 
    ##                                         1 
    ##                       Depression_Symptoms 
    ##                                        10 
    ##                    Depression_Symptoms_T1 
    ##                                         4 
    ##                    Depression_Symptoms_T2 
    ##                                         4 
    ##                       Depressive_Symptoms 
    ##                                         1 
    ##              Depressive_Symptoms_Grade_10 
    ##                                         6 
    ##               Depressive_Symptoms_Grade_5 
    ##                                         2 
    ##               Depressive_Symptoms_Grade_6 
    ##                                         4 
    ##               Depressive_Symptoms_Grade_9 
    ##                                         3 
    ##              Drug_Abuse_And_Or_Dependence 
    ##                                         1 
    ##                       Dysthymia_Diagnosis 
    ##                                         1 
    ##                    Externalising_Symptoms 
    ##                                         4 
    ##               Externalising_Symptoms_CBCL 
    ##                                         5 
    ##                Externalising_Symptoms_YSR 
    ##                                         5 
    ##    Generalized_Anxiety_Disorder_Diagnosis 
    ##                                         1 
    ##                    Internalising_Symptoms 
    ##                                         4 
    ##               Internalising_Symptoms_CBCL 
    ##                                         5 
    ##                Internalising_Symptoms_YSR 
    ##                                         5 
    ##                             Marijuana_Use 
    ##                                         4 
    ##             Person_Offences_Externalising 
    ##                                         4 
    ##           Property_Offences_Externalising 
    ##                                         4 
    ##    Psychological_Adjustment_Symptoms_CBCL 
    ##                                         3 
    ##     Psychological_Adjustment_Symptoms_TSC 
    ##                                         3 
    ##     Psychological_Adjustment_Symptoms_YSR 
    ##                                         3 
    ##            Psychotic_Experiences_Symptoms 
    ##                                         2 
    ##                            PTSD_Diagnosis 
    ##                                         1 
    ##                             PTSD_Symptoms 
    ##                                         4 
    ##   Serious_Psychological_Distress_Symptoms 
    ##                                         1 
    ##                   Social_Anxiety_Symptoms 
    ##                                         3 
    ##                Social_Anxiety_Symptoms_T1 
    ##                                         1 
    ##                Social_Anxiety_Symptoms_T2 
    ##                                         2 
    ##                         Suicidal_Thoughts 
    ##                                         4

``` r
# Create MHoutcome_broad var
data$MHoutcome_broad <- NA
# Code internalising problems
data$MHoutcome_broad[data$MH_Outcome=="Anxiety_Symptoms" |
                       data$MH_Outcome=="Anxiety_Symptoms_T1"  |
                       data$MH_Outcome=="Anxiety_Symptoms_T2" | 
                       data$MH_Outcome=="Any_Internalising_Disorder_Diagnosis" |
                       data$MH_Outcome=="Depression_Diagnosis"  |
                       data$MH_Outcome=="Depression_Symptoms" | 
                       data$MH_Outcome=="Depression_Symptoms_T1" |
                       data$MH_Outcome=="Depression_Symptoms_T2" | 
                       data$MH_Outcome=="Depressive_Symptoms" |
                       data$MH_Outcome=="Depressive_Symptoms_Grade_10"  |
                       data$MH_Outcome=="Depressive_Symptoms_Grade_5" | 
                       data$MH_Outcome=="Depressive_Symptoms_Grade_6" |
                       data$MH_Outcome=="Depressive_Symptoms_Grade_9" |
                       data$MH_Outcome=="Dysthymia_Diagnosis" |
                       data$MH_Outcome=="Generalized_Anxiety_Disorder_Diagnosis" |
                       data$MH_Outcome=="Internalising_Symptoms" |
                       data$MH_Outcome=="Internalising_Symptoms_CBCL" |
                       data$MH_Outcome=="Internalising_Symptoms_YSR" |
                       data$MH_Outcome=="PTSD_Diagnosis" |
                       data$MH_Outcome=="PTSD_Symptoms" |
                       data$MH_Outcome=="Serious_Psychological_Distress_Symptoms" |
                       data$MH_Outcome=="Social_Anxiety_Symptoms" |
                       data$MH_Outcome=="Social_Anxiety_Symptoms_T1" |
                       data$MH_Outcome=="Social_Anxiety_Symptoms_T2" |
                       data$MH_Outcome=="Suicidal_Thoughts" ] <- "internalising"
# Code externalising problems
data$MHoutcome_broad[data$MH_Outcome=="Alcohol_Abuse_And_Or_Dependence_Diagnosis" |
                       data$MH_Outcome=="Alcohol_Use"  |
                       data$MH_Outcome=="Antisocial_Personality_Disorder_Diagnosis" | 
                       data$MH_Outcome=="Any_Externalising_Disorder_Diagnosis" |
                       data$MH_Outcome=="Drug_Abuse_And_Or_Dependence"  |
                       data$MH_Outcome=="Externalising_Symptoms" | 
                       data$MH_Outcome=="Marijuana_Use" |
                       data$MH_Outcome=="Person_Offences_Externalising_Problems" | 
                       data$MH_Outcome=="Property_Offences_Externalising_Problems" |
                       data$MH_Outcome=="Externalising_Symptoms" |
                      data$MH_Outcome=="Person_Offences_Externalising" |
                      data$MH_Outcome=="Externalising_Symptoms" |
                      data$MH_Outcome=="Externalising_Symptoms_YSR" | 
                       data$MH_Outcome=="Externalising_Symptoms_CBCL" |
                      data$MH_Outcome=="Property_Offences_Externalising"] <- "externalising"
# Code psychotic experiences 
data$MHoutcome_broad[data$MH_Outcome=="Psychotic_Experiences_Symptoms" |
                       data$MH_Outcome=="Non_Clinical_Psychotic_Experiences_Symptoms"] <- "psychotic_experiences"

table(data$MHoutcome_broad, useNA = "always")
```

    ## 
    ##         externalising         internalising psychotic_experiences 
    ##                    37                    89                     2 
    ##                  <NA> 
    ##                    13

``` r
# Compare original coding to new broad categories of mental health type
data %>%  dplyr::select(Author, MH_Outcome, MHoutcome_broad)
```

    ##                     Author                                MH_Outcome
    ## 1         Danese & Widom.,             Any_Psychopathology_Diagnosis
    ## 2         Danese & Widom.,      Any_Internalising_Disorder_Diagnosis
    ## 3         Danese & Widom.,      Any_Externalising_Disorder_Diagnosis
    ## 4         Danese & Widom.,                      Depression_Diagnosis
    ## 5         Danese & Widom.,                       Dysthymia_Diagnosis
    ## 6         Danese & Widom.,    Generalized_Anxiety_Disorder_Diagnosis
    ## 7         Danese & Widom.,                            PTSD_Diagnosis
    ## 8         Danese & Widom., Antisocial_Personality_Disorder_Diagnosis
    ## 9         Danese & Widom., Alcohol_Abuse_And_Or_Dependence_Diagnosis
    ## 10        Danese & Widom.,              Drug_Abuse_And_Or_Dependence
    ## 11        Danese & Widom.,             Any_Psychopathology_Diagnosis
    ## 12        Danese & Widom.,      Any_Internalising_Disorder_Diagnosis
    ## 13        Danese & Widom.,      Any_Externalising_Disorder_Diagnosis
    ## 14        Danese & Widom.,             Any_Psychopathology_Diagnosis
    ## 15        Danese & Widom.,      Any_Internalising_Disorder_Diagnosis
    ## 16        Danese & Widom.,      Any_Externalising_Disorder_Diagnosis
    ## 17        Danese & Widom.,             Any_Psychopathology_Diagnosis
    ## 18        Danese & Widom.,      Any_Internalising_Disorder_Diagnosis
    ## 19        Danese & Widom.,      Any_Externalising_Disorder_Diagnosis
    ## 20         Gromann et al.,            Psychotic_Experiences_Symptoms
    ## 21  Zimmer-Gembeck et al.,                       Depressive_Symptoms
    ## 22  Zimmer-Gembeck et al.,                   Social_Anxiety_Symptoms
    ## 23          Bouman et al.,                       Depression_Symptoms
    ## 24          Bouman et al.,                          Anxiety_Symptoms
    ## 25      Graham & Juvonen.,                   Social_Anxiety_Symptoms
    ## 26         Graham, et al.,                          Anxiety_Symptoms
    ## 27         Graham, et al.,                       Depression_Symptoms
    ## 28         Graham, et al.,                    Internalising_Symptoms
    ## 29         Graham, et al.,                    Externalising_Symptoms
    ## 30           White et al.,                          Anxiety_Symptoms
    ## 31           White et al.,                          Anxiety_Symptoms
    ## 32           White et al.,                          Anxiety_Symptoms
    ## 33           White et al.,                          Anxiety_Symptoms
    ## 34           White et al.,                       Depression_Symptoms
    ## 35           White et al.,                       Depression_Symptoms
    ## 36           White et al.,                       Depression_Symptoms
    ## 37           White et al.,                       Depression_Symptoms
    ## 38           White et al.,                         Suicidal_Thoughts
    ## 39           White et al.,                         Suicidal_Thoughts
    ## 40           White et al.,                         Suicidal_Thoughts
    ## 41           White et al.,                         Suicidal_Thoughts
    ## 42         Newbury et al.,            Psychotic_Experiences_Symptoms
    ## 43  Goldman-Mellor et al.,   Serious_Psychological_Distress_Symptoms
    ## 44           McGee et al.,               Internalising_Symptoms_CBCL
    ## 45           McGee et al.,               Externalising_Symptoms_CBCL
    ## 46           McGee et al.,                Internalising_Symptoms_YSR
    ## 47           McGee et al.,                Externalising_Symptoms_YSR
    ## 48           McGee et al.,               Internalising_Symptoms_CBCL
    ## 49           McGee et al.,               Externalising_Symptoms_CBCL
    ## 50           McGee et al.,                Internalising_Symptoms_YSR
    ## 51           McGee et al.,                Externalising_Symptoms_YSR
    ## 52           McGee et al.,               Internalising_Symptoms_CBCL
    ## 53           McGee et al.,               Externalising_Symptoms_CBCL
    ## 54           McGee et al.,                Internalising_Symptoms_YSR
    ## 55           McGee et al.,                Externalising_Symptoms_YSR
    ## 56           McGee et al.,               Internalising_Symptoms_CBCL
    ## 57           McGee et al.,               Externalising_Symptoms_CBCL
    ## 58           McGee et al.,                Internalising_Symptoms_YSR
    ## 59           McGee et al.,                Externalising_Symptoms_YSR
    ## 60           McGee et al.,               Internalising_Symptoms_CBCL
    ## 61           McGee et al.,               Externalising_Symptoms_CBCL
    ## 62           McGee et al.,                Internalising_Symptoms_YSR
    ## 63           McGee et al.,                Externalising_Symptoms_YSR
    ## 64          Kochel et al.,               Depressive_Symptoms_Grade_5
    ## 65          Kochel et al.,               Depressive_Symptoms_Grade_5
    ## 66          Kochel et al.,               Depressive_Symptoms_Grade_6
    ## 67          Kochel et al.,               Depressive_Symptoms_Grade_6
    ## 68          Kochel et al.,               Depressive_Symptoms_Grade_9
    ## 69          Kochel et al.,               Depressive_Symptoms_Grade_9
    ## 70          Kochel et al.,               Depressive_Symptoms_Grade_9
    ## 71          Kochel et al.,              Depressive_Symptoms_Grade_10
    ## 72          Kochel et al.,              Depressive_Symptoms_Grade_10
    ## 73          Kochel et al.,              Depressive_Symptoms_Grade_10
    ## 74          Kochel et al.,               Depressive_Symptoms_Grade_6
    ## 75          Kochel et al.,               Depressive_Symptoms_Grade_6
    ## 76          Kochel et al.,              Depressive_Symptoms_Grade_10
    ## 77          Kochel et al.,              Depressive_Symptoms_Grade_10
    ## 78          Kochel et al.,              Depressive_Symptoms_Grade_10
    ## 79          Mulder et al.,                Social_Anxiety_Symptoms_T1
    ## 80          Mulder et al.,                Social_Anxiety_Symptoms_T2
    ## 81          Mulder et al.,                Social_Anxiety_Symptoms_T2
    ## 82         Mcclain et al.,                    Depression_Symptoms_T2
    ## 83         Mcclain et al.,                    Depression_Symptoms_T2
    ## 84         Mcclain et al.,                    Depression_Symptoms_T2
    ## 85         Mcclain et al.,                    Depression_Symptoms_T2
    ## 86         Mcclain et al.,                       Anxiety_Symptoms_T2
    ## 87         Mcclain et al.,                       Anxiety_Symptoms_T2
    ## 88         Mcclain et al.,                       Anxiety_Symptoms_T2
    ## 89         Mcclain et al.,                       Anxiety_Symptoms_T2
    ## 90         Mcclain et al.,                    Depression_Symptoms_T1
    ## 91         Mcclain et al.,                    Depression_Symptoms_T1
    ## 92         Mcclain et al.,                    Depression_Symptoms_T1
    ## 93         Mcclain et al.,                    Depression_Symptoms_T1
    ## 94         Mcclain et al.,                       Anxiety_Symptoms_T1
    ## 95         Mcclain et al.,                       Anxiety_Symptoms_T1
    ## 96         Mcclain et al.,                       Anxiety_Symptoms_T1
    ## 97         Mcclain et al.,                       Anxiety_Symptoms_T1
    ## 98         Negriff et al.,                       Depression_Symptoms
    ## 99         Negriff et al.,                             PTSD_Symptoms
    ## 100        Negriff et al.,                          Anxiety_Symptoms
    ## 101        Negriff et al.,                             Marijuana_Use
    ## 102        Negriff et al.,                               Alcohol_Use
    ## 103        Negriff et al.,             Person_Offences_Externalising
    ## 104        Negriff et al.,           Property_Offences_Externalising
    ## 105        Negriff et al.,                       Depression_Symptoms
    ## 106        Negriff et al.,                             PTSD_Symptoms
    ## 107        Negriff et al.,                          Anxiety_Symptoms
    ## 108        Negriff et al.,                             Marijuana_Use
    ## 109        Negriff et al.,                               Alcohol_Use
    ## 110        Negriff et al.,             Person_Offences_Externalising
    ## 111        Negriff et al.,           Property_Offences_Externalising
    ## 112        Negriff et al.,                       Depression_Symptoms
    ## 113        Negriff et al.,                             PTSD_Symptoms
    ## 114        Negriff et al.,                          Anxiety_Symptoms
    ## 115        Negriff et al.,                             Marijuana_Use
    ## 116        Negriff et al.,                               Alcohol_Use
    ## 117        Negriff et al.,             Person_Offences_Externalising
    ## 118        Negriff et al.,           Property_Offences_Externalising
    ## 119        Negriff et al.,                       Depression_Symptoms
    ## 120        Negriff et al.,                             PTSD_Symptoms
    ## 121        Negriff et al.,                          Anxiety_Symptoms
    ## 122        Negriff et al.,                             Marijuana_Use
    ## 123        Negriff et al.,                               Alcohol_Use
    ## 124        Negriff et al.,             Person_Offences_Externalising
    ## 125        Negriff et al.,           Property_Offences_Externalising
    ## 126        Everson et al.,     Psychological_Adjustment_Symptoms_TSC
    ## 127        Everson et al.,     Psychological_Adjustment_Symptoms_YSR
    ## 128        Everson et al.,    Psychological_Adjustment_Symptoms_CBCL
    ## 129        Everson et al.,     Psychological_Adjustment_Symptoms_TSC
    ## 130        Everson et al.,     Psychological_Adjustment_Symptoms_YSR
    ## 131        Everson et al.,    Psychological_Adjustment_Symptoms_CBCL
    ## 132        Everson et al.,     Psychological_Adjustment_Symptoms_TSC
    ## 133        Everson et al.,     Psychological_Adjustment_Symptoms_YSR
    ## 134        Everson et al.,    Psychological_Adjustment_Symptoms_CBCL
    ## 135       Flanagan et al.,                   Social_Anxiety_Symptoms
    ## 136            Cho et al.,                    Internalising_Symptoms
    ## 137            Cho et al.,                    Externalising_Symptoms
    ## 138            Cho et al.,                    Internalising_Symptoms
    ## 139            Cho et al.,                    Externalising_Symptoms
    ## 140            Cho et al.,                    Internalising_Symptoms
    ## 141            Cho et al.,                    Externalising_Symptoms
    ##           MHoutcome_broad
    ## 1                    <NA>
    ## 2           internalising
    ## 3           externalising
    ## 4           internalising
    ## 5           internalising
    ## 6           internalising
    ## 7           internalising
    ## 8           externalising
    ## 9           externalising
    ## 10          externalising
    ## 11                   <NA>
    ## 12          internalising
    ## 13          externalising
    ## 14                   <NA>
    ## 15          internalising
    ## 16          externalising
    ## 17                   <NA>
    ## 18          internalising
    ## 19          externalising
    ## 20  psychotic_experiences
    ## 21          internalising
    ## 22          internalising
    ## 23          internalising
    ## 24          internalising
    ## 25          internalising
    ## 26          internalising
    ## 27          internalising
    ## 28          internalising
    ## 29          externalising
    ## 30          internalising
    ## 31          internalising
    ## 32          internalising
    ## 33          internalising
    ## 34          internalising
    ## 35          internalising
    ## 36          internalising
    ## 37          internalising
    ## 38          internalising
    ## 39          internalising
    ## 40          internalising
    ## 41          internalising
    ## 42  psychotic_experiences
    ## 43          internalising
    ## 44          internalising
    ## 45          externalising
    ## 46          internalising
    ## 47          externalising
    ## 48          internalising
    ## 49          externalising
    ## 50          internalising
    ## 51          externalising
    ## 52          internalising
    ## 53          externalising
    ## 54          internalising
    ## 55          externalising
    ## 56          internalising
    ## 57          externalising
    ## 58          internalising
    ## 59          externalising
    ## 60          internalising
    ## 61          externalising
    ## 62          internalising
    ## 63          externalising
    ## 64          internalising
    ## 65          internalising
    ## 66          internalising
    ## 67          internalising
    ## 68          internalising
    ## 69          internalising
    ## 70          internalising
    ## 71          internalising
    ## 72          internalising
    ## 73          internalising
    ## 74          internalising
    ## 75          internalising
    ## 76          internalising
    ## 77          internalising
    ## 78          internalising
    ## 79          internalising
    ## 80          internalising
    ## 81          internalising
    ## 82          internalising
    ## 83          internalising
    ## 84          internalising
    ## 85          internalising
    ## 86          internalising
    ## 87          internalising
    ## 88          internalising
    ## 89          internalising
    ## 90          internalising
    ## 91          internalising
    ## 92          internalising
    ## 93          internalising
    ## 94          internalising
    ## 95          internalising
    ## 96          internalising
    ## 97          internalising
    ## 98          internalising
    ## 99          internalising
    ## 100         internalising
    ## 101         externalising
    ## 102         externalising
    ## 103         externalising
    ## 104         externalising
    ## 105         internalising
    ## 106         internalising
    ## 107         internalising
    ## 108         externalising
    ## 109         externalising
    ## 110         externalising
    ## 111         externalising
    ## 112         internalising
    ## 113         internalising
    ## 114         internalising
    ## 115         externalising
    ## 116         externalising
    ## 117         externalising
    ## 118         externalising
    ## 119         internalising
    ## 120         internalising
    ## 121         internalising
    ## 122         externalising
    ## 123         externalising
    ## 124         externalising
    ## 125         externalising
    ## 126                  <NA>
    ## 127                  <NA>
    ## 128                  <NA>
    ## 129                  <NA>
    ## 130                  <NA>
    ## 131                  <NA>
    ## 132                  <NA>
    ## 133                  <NA>
    ## 134                  <NA>
    ## 135         internalising
    ## 136         internalising
    ## 137         externalising
    ## 138         internalising
    ## 139         externalising
    ## 140         internalising
    ## 141         externalising

``` r
# Check which outcomes haven't been coded 
data %>% dplyr::filter (is.na(MHoutcome_broad)) %>%
  dplyr::select(Author, MH_Outcome)
```

    ##              Author                             MH_Outcome
    ## 1  Danese & Widom.,          Any_Psychopathology_Diagnosis
    ## 2  Danese & Widom.,          Any_Psychopathology_Diagnosis
    ## 3  Danese & Widom.,          Any_Psychopathology_Diagnosis
    ## 4  Danese & Widom.,          Any_Psychopathology_Diagnosis
    ## 5   Everson et al.,  Psychological_Adjustment_Symptoms_TSC
    ## 6   Everson et al.,  Psychological_Adjustment_Symptoms_YSR
    ## 7   Everson et al., Psychological_Adjustment_Symptoms_CBCL
    ## 8   Everson et al.,  Psychological_Adjustment_Symptoms_TSC
    ## 9   Everson et al.,  Psychological_Adjustment_Symptoms_YSR
    ## 10  Everson et al., Psychological_Adjustment_Symptoms_CBCL
    ## 11  Everson et al.,  Psychological_Adjustment_Symptoms_TSC
    ## 12  Everson et al.,  Psychological_Adjustment_Symptoms_YSR
    ## 13  Everson et al., Psychological_Adjustment_Symptoms_CBCL

##### 2.3 Convert effect sizes

###### 2.3.1 Prepare data

``` r
# Make new variable to input correlation coefficient and variance for objective measure
data$r_objective_es <- NA
data$r_objective_var <- NA

# Look at original effect sizes prior to converting
table(data$Type_of_effect_size_obj)
```

    ## 
    ##            cohens_d         correlation            log_odds          odds_ratio 
    ##                  33                  65                   4                   8 
    ##          risk_ratio   standardised_beta unstandardised_beta 
    ##                  19                   1                  11

``` r
# Include results for studies that have correlations or standardised betas already
data$r_objective_es[data$Type_of_effect_size_obj=="correlation" |
                      data$Type_of_effect_size_obj=="standardised_beta"] <- 
  data$Effect_size_obj[data$Type_of_effect_size_obj=="correlation" |
                                                        data$Type_of_effect_size_obj=="standardised_beta"]
data$r_objective_var[data$Type_of_effect_size_obj=="correlation" |
                     data$Type_of_effect_size_obj=="standardised_beta"] <- 
  data$SE_obj[data$Type_of_effect_size_obj=="correlation" |
                                               data$Type_of_effect_size_obj=="standardised_beta"]^2

describe(data$r_objective_es)
```

    ##    vars  n mean  sd median trimmed  mad   min  max range  skew kurtosis   se
    ## X1    1 66 0.02 0.1      0    0.02 0.05 -0.34 0.32  0.66 -0.36     2.43 0.01

``` r
table(data$Type_of_effect_size_obj[is.na(data$r_objective_es)])
```

    ## 
    ##            cohens_d            log_odds          odds_ratio          risk_ratio 
    ##                  33                   4                   8                  19 
    ## unstandardised_beta 
    ##                  11

``` r
# Repeat for subjective effect sizes

# Make new variable to input correlation coefficient and variance for subjective measure
data$r_subjective_es <- NA
data$r_subjective_var <- NA

# Look at effect sizes prior to converting
table(data$Type_of_effect_size_subj)
```

    ## 
    ##            cohens_d         correlation            log_odds          odds_ratio 
    ##                  33                  65                   4                   8 
    ##          risk_ratio   standardised_beta unstandardised_beta 
    ##                  19                   1                  11

``` r
# Include results for studies that have correlations or standardised betas already
data$r_subjective_es[data$Type_of_effect_size_subj=="correlation" |
                       data$Type_of_effect_size_subj=="standardised_beta"] <- 
  data$Effect_size_subj[data$Type_of_effect_size_subj=="correlation" |
                          data$Type_of_effect_size_subj=="standardised_beta"]
data$r_subjective_var[data$Type_of_effect_size_subj=="correlation" |
                        data$Type_of_effect_size_subj=="standardised_beta"] <- 
  data$SE_subj[data$Type_of_effect_size_subj=="correlation" |
                 data$Type_of_effect_size_subj=="standardised_beta"]^2

describe(data$r_subjective_es)
```

    ##    vars  n mean   sd median trimmed  mad   min  max range skew kurtosis   se
    ## X1    1 66 0.13 0.11   0.11    0.12 0.13 -0.11 0.37  0.48 0.16     -0.8 0.01

``` r
table(data$Type_of_effect_size_subj[is.na(data$r_subjective_es)])
```

    ## 
    ##            cohens_d            log_odds          odds_ratio          risk_ratio 
    ##                  33                   4                   8                  19 
    ## unstandardised_beta 
    ##                  11

###### 2.3.2 Convert Cohen’s d to r

``` r
# Function to convert cohen's d to r 
# Conversion equation from: https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
# Produces the same results as "d_to_r" function from "effectsize" 
cohens_d_to_r <- function(d, se_d) {
  r = d / sqrt(d^2 + 4) # convert cohen's d to r
  return(r)
}

# Function to convert cohen's d SE to variance of r 
# Conversion equation from: https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
cohens_d_to_r_variance <- function(d, se_d) {
  var_d = se_d^2 # convert se of cohen's d to variance 
  var_r = 4^2 * var_d / (d^2 + 4)^3 # convert cohen's d and variance of d to variance of r
  return(var_r)
}

# Check studies reporting cohen's d
data %>% dplyr::filter (Type_of_effect_size_obj == "cohens_d") %>% 
  group_by(Author) %>%   summarise(n = n())
```

    ## # A tibble: 3 × 2
    ##   Author              n
    ##   <chr>           <int>
    ## 1 Graham, et al.,     4
    ## 2 Gromann et al.,     1
    ## 3 Negriff et al.,    28

``` r
# Convert effect sizes for the objective measure 
# Convert effect sizes
data$r_objective_es[data$Type_of_effect_size_obj == "cohens_d"] <- 
  cohens_d_to_r(d = data$Effect_size_obj[data$Type_of_effect_size_obj == "cohens_d"],
                se_d = data$SE_obj[data$Type_of_effect_size_obj == "cohens_d"])
# Convert SE to variance
data$r_objective_var[data$Type_of_effect_size_obj == "cohens_d"] <- 
  cohens_d_to_r_variance(d = data$Effect_size_obj[data$Type_of_effect_size_obj == "cohens_d"],
                se_d = data$SE_obj[data$Type_of_effect_size_obj == "cohens_d"])

# Check results before and after converting
# effect sizes
data$Effect_size_obj[data$Type_of_effect_size_obj == "cohens_d"] # before converting
```

    ##  [1]  0.012100000 -0.120000000  0.010000000  0.100000000  1.000000000
    ##  [6]  0.370541600  0.448240100  0.306447900  0.504467300  0.242202900
    ## [11]  0.460465300  0.460576300  0.285849100  0.169362700  0.145756200
    ## [16]  0.166999000 -0.139718500  0.290775600  0.290414800 -0.057397550
    ## [21] -0.092914720 -0.054347820 -0.218700000 -0.441488200 -0.338541800
    ## [26] -0.091442740  0.354131600  0.135902100 -0.143088900  0.195108900
    ## [31] -0.009346203 -0.047740990 -0.013121170

``` r
data$r_objective_es[data$Type_of_effect_size_obj == "cohens_d"] # after converting
```

    ##  [1]  0.006049889 -0.059892291  0.004999938  0.049937617  0.447213595
    ##  [6]  0.182170651  0.218694843  0.151456350  0.244573504  0.120223089
    ## [11]  0.224363015  0.224414377  0.141486750  0.084379351  0.072685332
    ## [16]  0.083209927 -0.069689404  0.143875163  0.143700328 -0.028686964
    ## [21] -0.046407307 -0.027163883 -0.108702033 -0.215554792 -0.166896770
    ## [26] -0.045673656  0.174353699  0.067794714 -0.071362046  0.097093531
    ## [31] -0.004673050 -0.023863697 -0.006560444

``` r
# variance
data$SE_obj[data$Type_of_effect_size_obj == "cohens_d"]^2 # var before converting
```

    ##  [1] 0.032041000 0.007540076 0.007528599 0.007536544 0.008331087 0.050989072
    ##  [7] 0.051182999 0.050856790 0.051184628 0.050598319 0.051057515 0.051057831
    ## [13] 0.031691726 0.031484599 0.031455541 0.030993462 0.030961288 0.031211406
    ## [19] 0.031210593 0.046555951 0.046586988 0.046553970 0.046306650 0.047151908
    ## [25] 0.046690437 0.046079804 0.048635245 0.048144698 0.048153870 0.048082447
    ## [31] 0.047909805 0.047919767 0.047910192

``` r
data$r_objective_var[data$Type_of_effect_size_obj == "cohens_d"] # after converting
```

    ##  [1] 0.008009370 0.001864806 0.001882009 0.001870075 0.001066379 0.011519821
    ##  [7] 0.011046196 0.011859161 0.010634519 0.012108974 0.010932156 0.010931428
    ## [13] 0.007456577 0.007704219 0.007739904 0.007588531 0.007628094 0.007328254
    ## [19] 0.007329192 0.011610277 0.011571660 0.011612748 0.011171118 0.010219996
    ## [25] 0.010724121 0.011448007 0.011083323 0.011870977 0.011855483 0.011683846
    ## [31] 0.011976667 0.011959486 0.011976002

``` r
# Repeat for subjective measure
# Convert effect sizes
data$r_subjective_es[data$Type_of_effect_size_subj == "cohens_d"] <- 
  cohens_d_to_r(d = data$Effect_size_subj[data$Type_of_effect_size_subj == "cohens_d"],
                se_d = data$SE_subj[data$Type_of_effect_size_subj == "cohens_d"])
# Convert SE to variance
data$r_subjective_var[data$Type_of_effect_size_subj == "cohens_d"] <- 
  cohens_d_to_r_variance(d = data$Effect_size_subj[data$Type_of_effect_size_subj == "cohens_d"],
                         se_d = data$SE_subj[data$Type_of_effect_size_subj == "cohens_d"])

# Check results before and after converting
# effect sizes
data$Effect_size_subj[data$Type_of_effect_size_subj == "cohens_d"] # before converting
```

    ##  [1]  0.31170000  0.04000000  0.47000000  0.13000000  0.15000000  0.68164570
    ##  [7]  0.65307910  0.22547540  0.24689370  0.08385069  0.53593310  0.56062810
    ## [13]  0.28161200  0.35552660  0.15285950  0.46544030  0.63158140  0.72193280
    ## [19]  0.60201900  0.14793620  0.23724450  0.15364890  0.18183710  0.29393470
    ## [25]  0.20937580  0.07239088  0.79386010  0.24071780  0.30570250  0.54229030
    ## [31]  0.41120210  0.06437042 -0.16564690

``` r
data$r_subjective_es[data$Type_of_effect_size_subj == "cohens_d"] # after converting
```

    ##  [1]  0.15399106  0.01999600  0.22876803  0.06486312  0.07478995  0.32260080
    ##  [7]  0.31040946  0.11202802  0.12251685  0.04188855  0.25883469  0.26991032
    ## [13]  0.13943059  0.17501952  0.07620749  0.22666319  0.30113243  0.33952405
    ## [19]  0.28823460  0.07376658  0.11779637  0.07659874  0.09054509  0.14540540
    ## [25]  0.10411891  0.03617175  0.36892956  0.11949648  0.15109637  0.26169585
    ## [31]  0.20138858  0.03216855 -0.08254083

``` r
# variance
data$SE_subj[data$Type_of_effect_size_subj == "cohens_d"]^2 # var before converting
```

    ##  [1] 0.014472090 0.007529803 0.007705806 0.007542082 0.007546578 0.051987055
    ##  [7] 0.051870791 0.050725445 0.048792525 0.048632039 0.049465941 0.049546531
    ## [13] 0.042864940 0.043091366 0.042596007 0.044105712 0.044981956 0.045569868
    ## [19] 0.044806602 0.041708014 0.041885306 0.041716878 0.041258831 0.041530935
    ## [25] 0.041313815 0.041116885 0.087470961 0.081748302 0.082103395 0.084109628
    ## [31] 0.082859695 0.081210266 0.081443228

``` r
data$r_subjective_var[data$Type_of_effect_size_subj == "cohens_d"] # after converting
```

    ##  [1] 0.003366693 0.001880194 0.001639543 0.001861822 0.001855162 0.009346642
    ##  [7] 0.009568808 0.012209865 0.011657039 0.012094123 0.010043791 0.009871907
    ## [13] 0.010103308 0.009812881 0.010464542 0.009412758 0.008455271 0.007889334
    ## [19] 0.008635302 0.010257712 0.010041447 0.010246718 0.010063089 0.009738001
    ## [25] 0.009996177 0.010238926 0.014098754 0.019574029 0.019151877 0.016996350
    ## [31] 0.018295336 0.020239603 0.019947482

``` r
## Check p-values before and after converting to r
# Derive function to calculate p-value (see box in https://www.bmj.com/content/343/bmj.d2304)
p_function <- function (es, se) {
  z <- es/se
  p <- exp(-0.717*z - 0.416*z^2)
  return(p)
} 
# P-values 
p_function(data$Effect_size_subj[data$Type_of_effect_size_subj=="cohens_d"], 
           data$SE_subj[data$Type_of_effect_size_subj=="cohens_d"]) # before converting to r
```

    ##  [1] 9.556475e-03 6.577651e-01 1.424526e-07 1.346003e-01 8.388241e-02
    ##  [6] 2.846726e-03 4.183530e-03 3.215071e-01 2.668377e-01 7.169371e-01
    ## [11] 1.587135e-02 1.173927e-02 1.746611e-01 8.644566e-02 4.680236e-01
    ## [16] 2.645483e-02 2.955176e-03 7.596610e-04 4.498147e-03 4.782303e-01
    ## [21] 2.490316e-01 4.607979e-01 3.770997e-01 1.496338e-01 3.072788e-01
    ## [26] 7.341864e-01 7.286221e-03 4.071701e-01 2.898276e-01 6.110381e-02
    ## [31] 1.536388e-01 8.326162e-01 1.317874e+00

``` r
p_function(data$r_subjective_es[data$Type_of_effect_size_subj=="cohens_d"], 
           sqrt(data$r_subjective_var[data$Type_of_effect_size_subj=="cohens_d"])) # after converting to r
```

    ##  [1] 7.962787e-03 6.576316e-01 2.976941e-08 1.329378e-01 8.214281e-02
    ##  [6] 8.897882e-04 1.558266e-03 3.152074e-01 2.594249e-01 7.164420e-01
    ## [11] 9.787475e-03 6.619590e-03 1.661167e-01 7.688987e-02 4.653281e-01
    ## [16] 1.933783e-02 1.103156e-03 1.478640e-04 1.976865e-03 4.757325e-01
    ## [21] 2.422673e-01 4.580549e-01 3.730340e-01 1.409021e-01 3.018507e-01
    ## [26] 7.338382e-01 1.942395e-03 4.001668e-01 2.783919e-01 4.435732e-02
    ## [31] 1.367318e-01 8.324399e-01 1.319095e+00

###### 2.3.3 Convert odds ratio to r

``` r
# Function to convert odds ratio to R
# Conversion equation from: https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
# gives same results as "lores" function from "compute.es" but does not require Ns for separate groups
OR_to_r <- function(OR, logSE) {
  log_odds = log(OR) # log transform odds ratio 
  d = log_odds * (sqrt(3)/3.14159) # convert log odds to cohen's d
  var_d = logSE^2 * (3 / 3.14159^2) # convert log odds SE to variance of cohen's d
  r = d / sqrt(d^2 + 4) # convert cohen's d to r
  var_r = 4^2 * var_d / (d^2 + 4)^3 # convert cohen's d and variance of d to variance of r
  se_r = sqrt(var_r) 
  return(r)
}

# Function to convert odds ratio SE to variance of R
# Conversion equation from: https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
OR_to_r_variance <- function(OR, logSE) {
  log_odds = log(OR) # log transform odds ratio 
  d = log_odds * (sqrt(3)/3.14159) # convert log odds to cohen's d
  var_d = logSE^2 * (3 / 3.14159^2) # convert log odds SE to variance of cohen's d
  r = d / sqrt(d^2 + 4) # convert cohen's d to r
  var_r = 4^2 * var_d / (d^2 + 4)^3 # convert cohen's d and variance of d to variance of r
  se_r = sqrt(var_r)
  return(var_r)
}

# Check studies reporting odds ratio
data %>% dplyr::filter (Type_of_effect_size_obj == "odds_ratio") %>% 
  group_by(Author) %>%   summarise(n = n())
```

    ## # A tibble: 3 × 2
    ##   Author                     n
    ##   <chr>                  <int>
    ## 1 Cho et al.,                6
    ## 2 Goldman-Mellor et al.,     1
    ## 3 Newbury et al.,            1

``` r
# Check studies reporting log odds 
data %>% dplyr::filter (Type_of_effect_size_obj == "log_odds") %>% 
  group_by(Author) %>%   summarise(n = n())
```

    ## # A tibble: 1 × 2
    ##   Author            n
    ##   <chr>         <int>
    ## 1 White et al.,     4

``` r
## Convert log odds to odds ratios
# Objective measure
data$Effect_size_obj[data$Type_of_effect_size_obj == "log_odds"] <- 
  exp(data$Effect_size_obj[data$Type_of_effect_size_obj == "log_odds"])
# Subjective measure
data$Effect_size_subj[data$Type_of_effect_size_subj == "log_odds"] <- 
  exp(data$Effect_size_subj[data$Type_of_effect_size_subj == "log_odds"])
# Specify that previous log odds values have now been converted to odds ratios
data$Type_of_effect_size_obj[data$Type_of_effect_size_obj == "log_odds"] <- "odds_ratio"
data$Type_of_effect_size_subj[data$Type_of_effect_size_subj == "log_odds"] <- "odds_ratio"

# Check studies reporting odds ratio
data %>% dplyr::filter (Type_of_effect_size_obj == "odds_ratio") %>% 
  group_by(Author) %>%   summarise(n = n())
```

    ## # A tibble: 4 × 2
    ##   Author                     n
    ##   <chr>                  <int>
    ## 1 Cho et al.,                6
    ## 2 Goldman-Mellor et al.,     1
    ## 3 Newbury et al.,            1
    ## 4 White et al.,              4

``` r
# Convert effect sizes for the objective measure 
# Convert effect sizes
data$r_objective_es[data$Type_of_effect_size_obj == "odds_ratio"] <- 
  OR_to_r(OR = data$Effect_size_obj[data$Type_of_effect_size_obj == "odds_ratio"],
          logSE = data$SE_obj[data$Type_of_effect_size_obj == "odds_ratio"])
# Convert SE to variance
data$r_objective_var[data$Type_of_effect_size_obj == "odds_ratio"] <- 
  OR_to_r_variance(OR = data$Effect_size_obj[data$Type_of_effect_size_obj == "odds_ratio"],
                         logSE = data$SE_obj[data$Type_of_effect_size_obj == "odds_ratio"])

# Check results before and after converting
# effect sizes
data$Effect_size_obj[data$Type_of_effect_size_obj == "odds_ratio"] # before converting
```

    ##  [1] 1.0202013 1.0100502 1.0202013 0.9512294 1.1100000 1.2100000 1.8750000
    ##  [8] 5.8333330 2.0222220 1.9908467 2.2500000 1.2000000

``` r
data$r_objective_es[data$Type_of_effect_size_obj == "odds_ratio"] # after converting
```

    ##  [1]  0.005513210  0.002756636  0.005513210 -0.013781925  0.028756473
    ##  [6]  0.052474903  0.170740685  0.437227594  0.190564850  0.186482072
    ## [11]  0.218160284  0.050196255

``` r
# variance
data$SE_obj[data$Type_of_effect_size_obj == "odds_ratio"]^2 # var before converting
```

    ##  [1] 0.000100000 0.000400000 0.000900000 0.001600000 0.002573255 0.003240385
    ##  [7] 1.495834180 1.326189470 0.188766788 0.125520129 1.361111889 1.033333241

``` r
data$r_objective_var[data$Type_of_effect_size_obj == "odds_ratio"] # after converting
```

    ##  [1] 7.598409e-06 3.039571e-05 6.838568e-05 1.215164e-04 1.950595e-04
    ##  [6] 2.442116e-04 1.040157e-01 5.332647e-02 1.283788e-02 8.577497e-03
    ## [11] 8.935576e-02 7.793197e-02

``` r
# Repeat for subjective measure
# Convert effect sizes 
data$r_subjective_es[data$Type_of_effect_size_subj == "odds_ratio"] <- 
  OR_to_r(OR = data$Effect_size_subj[data$Type_of_effect_size_subj == "odds_ratio"],
          logSE = data$SE_subj[data$Type_of_effect_size_subj == "odds_ratio"])
# Convert SE to variance
data$r_subjective_var[data$Type_of_effect_size_subj == "odds_ratio"] <- 
  OR_to_r_variance(OR = data$Effect_size_subj[data$Type_of_effect_size_subj == "odds_ratio"],
                   logSE = data$SE_subj[data$Type_of_effect_size_subj == "odds_ratio"])

# Check results before and after converting
# effect sizes
data$Effect_size_subj[data$Type_of_effect_size_subj == "odds_ratio"] # before converting
```

    ##  [1] 1.0202013 1.0000000 0.9704455 1.0100502 2.3900000 2.8000000 3.3898310
    ##  [8] 7.5555560 3.2552846 2.2684310 2.0281690 0.9802817

``` r
data$r_subjective_es[data$Type_of_effect_size_subj == "odds_ratio"] # after converting
```

    ##  [1]  0.005513210  0.000000000 -0.008269658  0.002756636  0.233542859
    ##  [6]  0.273044572  0.318949745  0.486921564  0.309396915  0.220249076
    ## [11]  0.191330475 -0.005489862

``` r
# variance
data$SE_subj[data$Type_of_effect_size_subj == "odds_ratio"]^2 # var before converting
```

    ##  [1] 0.00010000 0.00010000 0.00040000 0.00090000 0.01016437 0.10765947
    ##  [7] 1.16694870 1.15163375 0.14750047 0.10688407 0.65644559 0.39799254

``` r
data$r_subjective_var[data$Type_of_effect_size_subj == "odds_ratio"] # after converting
```

    ##  [1] 7.598409e-06 7.599102e-06 3.039017e-05 6.839036e-05 6.527836e-04
    ##  [6] 6.484383e-03 6.427414e-02 3.885904e-02 8.288103e-03 6.996622e-03
    ## [11] 4.460371e-02 3.024112e-02

``` r
# P-values 
p_function(data$Effect_size_subj[data$Type_of_effect_size_subj=="odds_ratio"], 
           data$SE_subj[data$Type_of_effect_size_subj=="odds_ratio"]) # before converting to r
```

    ##  [1]  0.000000e+00  0.000000e+00  0.000000e+00 5.251168e-216 1.226294e-109
    ##  [6]  1.535319e-16  1.753226e-03  7.112239e-12  2.404541e-16  1.385101e-11
    ## [11]  1.225767e-02  1.202055e-01

``` r
p_function(data$r_subjective_es[data$Type_of_effect_size_subj=="odds_ratio"], 
           sqrt(data$r_subjective_var[data$Type_of_effect_size_subj=="odds_ratio"])) # after converting to r
```

    ##  [1] 4.513286e-02 1.000000e+00 1.149636e+00 7.518453e-01 1.143866e-18
    ##  [6] 7.361387e-04 2.100410e-01 1.344408e-02 7.162554e-04 8.461785e-03
    ## [11] 3.712130e-01 1.022469e+00

``` r
# Check effect sizes left to convert
table(data$Type_of_effect_size_obj[is.na(data$r_objective_es)])
```

    ## 
    ##          risk_ratio unstandardised_beta 
    ##                  19                  11

``` r
table(data$Type_of_effect_size_subj[is.na(data$r_subjective_es)])
```

    ## 
    ##          risk_ratio unstandardised_beta 
    ##                  19                  11

###### 2.3.4 Convert unstandardised betas to r

``` r
# Function to convert unstandardised beta to R
unstand_beta_to_r <- function(beta, SD_exposure, SD_outcome) {
  r = beta * (SD_exposure / SD_outcome)
  return(r)
}

# Function to convert SE of unstandardised beta to variance of R
# Equation: https://stats.stackexchange.com/questions/451358/calculating-the-standard-errors-of-the-standardized-regression-coefficients-from
unstand_beta_to_r_variance <- function(beta, se_beta, SD_exposure, SD_outcome) {
  r = beta * (SD_exposure / SD_outcome)
  se_r = se_beta * (r/beta)
  var_r = se_r^2
  return(var_r)
}

# Check studies reporting unstandardised beta
data %>% dplyr::filter (Type_of_effect_size_obj == "unstandardised_beta") %>% 
  group_by(Author) %>%   summarise(n = n())
```

    ## # A tibble: 3 × 2
    ##   Author                     n
    ##   <chr>                  <int>
    ## 1 Flanagan et al.,           1
    ## 2 White et al.,              8
    ## 3 Zimmer-Gembeck et al.,     2

``` r
# Convert effect sizes for the objective measure 
# Convert effect sizes
data$r_objective_es[data$Type_of_effect_size_obj == "unstandardised_beta"] <- 
  unstand_beta_to_r(beta = data$Effect_size_obj[data$Type_of_effect_size_obj == "unstandardised_beta"],
                    SD_exposure = data$Std_obj[data$Type_of_effect_size_obj == "unstandardised_beta"],
                      SD_outcome = data$SD_Outcome[data$Type_of_effect_size_obj == "unstandardised_beta"])
# Convert SE to variance
data$r_objective_var[data$Type_of_effect_size_obj == "unstandardised_beta"] <- 
  unstand_beta_to_r_variance(beta = data$Effect_size_obj[data$Type_of_effect_size_obj == "unstandardised_beta"],
                             se_beta = data$SE_obj[data$Type_of_effect_size_obj == "unstandardised_beta"],
                             SD_exposure = data$Std_obj[data$Type_of_effect_size_obj == "unstandardised_beta"],
                             SD_outcome = data$SD_Outcome[data$Type_of_effect_size_obj == "unstandardised_beta"])

# Check results before and after converting
# effect sizes
data$Effect_size_obj[data$Type_of_effect_size_obj == "unstandardised_beta"] # before converting
```

    ##  [1] -0.060 -0.350  0.480 -0.130  0.070 -0.150  0.570 -0.500 -0.180  0.290
    ## [11]  0.013

``` r
data$r_objective_es[data$Type_of_effect_size_obj == "unstandardised_beta"] # after converting
```

    ##  [1] -0.0005982906 -0.0033604136  0.2706040268 -0.0174496644  0.0042281879
    ##  [6] -0.0073825503  0.2669888476 -0.0557620818 -0.0090334572  0.0118587361
    ## [11]  0.0056564885

``` r
# variance
data$SE_obj[data$Type_of_effect_size_obj == "unstandardised_beta"]^2 # var before converting
```

    ##  [1] 0.012100 1.144900 0.025600 0.102400 0.372100 0.672400 0.040000 0.152100
    ##  [9] 0.532900 0.960400 0.000036

``` r
data$r_objective_var[data$Type_of_effect_size_obj == "unstandardised_beta"] # after converting
```

    ##  [1] 1.203115e-06 1.055400e-04 8.136282e-03 1.844962e-03 1.357601e-03
    ##  [6] 1.628763e-03 8.775998e-03 1.891765e-03 1.342174e-03 1.605953e-03
    ## [11] 6.815687e-06

``` r
# Repeat for subjective measure
# Convert effect sizes
data$r_subjective_es[data$Type_of_effect_size_subj == "unstandardised_beta"] <- 
  unstand_beta_to_r(beta = data$Effect_size_subj[data$Type_of_effect_size_subj == "unstandardised_beta"],
                    SD_exposure = data$Std_subj[data$Type_of_effect_size_subj == "unstandardised_beta"],
                    SD_outcome = data$SD_Outcome[data$Type_of_effect_size_subj == "unstandardised_beta"])
# Convert SE to variance
data$r_subjective_var[data$Type_of_effect_size_subj == "unstandardised_beta"] <- 
  unstand_beta_to_r_variance(beta = data$Effect_size_subj[data$Type_of_effect_size_subj == "unstandardised_beta"],
                             se_beta = data$SE_subj[data$Type_of_effect_size_subj == "unstandardised_beta"],
                             SD_exposure = data$Std_subj[data$Type_of_effect_size_subj == "unstandardised_beta"],
                             SD_outcome = data$SD_Outcome[data$Type_of_effect_size_subj == "unstandardised_beta"])

# Check results before and after converting
# effect sizes
data$Effect_size_subj[data$Type_of_effect_size_subj == "unstandardised_beta"] # before converting
```

    ##  [1]  0.040  0.120  0.460 -0.020  0.430  0.110  0.540  0.030  0.250  0.250
    ## [11]  0.484

``` r
data$r_subjective_es[data$Type_of_effect_size_subj == "unstandardised_beta"] # after converting
```

    ##  [1]  0.001424501  0.004874446  0.117315436 -0.010917226  0.048098434
    ##  [6]  0.009105145  0.114423792  0.013605948  0.023234201  0.017193309
    ## [11]  0.023276336

``` r
# variance
data$SE_subj[data$Type_of_effect_size_subj == "unstandardised_beta"]^2 # var before converting
```

    ##  [1] 0.000100 0.012100 0.010000 0.010000 0.144400 0.270400 0.014400 0.014400
    ##  [9] 0.211600 0.384400 0.002916

``` r
data$r_subjective_var[data$Type_of_effect_size_subj == "unstandardised_beta"] # after converting
```

    ##  [1] 1.268253e-07 1.996519e-05 6.504212e-04 2.979646e-03 1.806725e-03
    ##  [6] 1.852657e-03 6.465582e-04 2.961949e-03 1.827642e-03 1.818119e-03
    ## [11] 6.744131e-06

###### 2.3.5 Convert risk ratios to r

``` r
# Check studies reporting unstandardised beta
data %>% dplyr::filter (Type_of_effect_size_obj == "risk_ratio") %>% 
  group_by(Author) %>%   summarise(n = n())
```

    ## # A tibble: 1 × 2
    ##   Author               n
    ##   <chr>            <int>
    ## 1 Danese & Widom.,    19

``` r
## First define function to convert RR to OR
# RR = OR/ (1-p + (p * OR)) 
# OR = ((1 - p) * RR) / (1 - RR * p).
# where RR is the relative risk, OR is the odds ratio, and p is the control event rate
# e.g. the prevalence rate in the non-exposed individuals
# formula recommended by Grant et al., BMJ 2014: https://www.bmj.com/content/348/bmj.f7450
# and referenced in https://www.r-bloggers.com/2014/01/how-to-convert-odds-ratios-to-relative-risks/
# and https://www.researchgate.net/post/Can-we-Convert-Risk-Ratio-to-Odd-Ratios-what-is-the-baseline-risk-p-of-a-selection-of-populations-to-either-suffer-die-from-coronary-heart-disease

RR_to_OR <- function(RR, p) {
  OR <- ((1 - p) * RR) / (1 - RR * p)
  return(OR)
}

RR_to_OR_SE <- function(RR, RR_lowCI, RR_upCI, p) {
  OR <- ((1 - p) * RR) / (1 - RR * p)
  log_OR <- log(OR)
  OR_lowCI <- ((1 - p) * RR_lowCI) / (1 - RR_lowCI * p)
  OR_upperCI <- ((1 - p) * RR_upCI) / (1 - RR_upCI * p)
  log_OR_SE <- (log(OR_upperCI) - log(OR_lowCI))/3.92
  return(log_OR_SE)
}

# Create specific columns for upper and lower CIs for the risk ratios
# Objective
data$lower_CI_obj <- as.numeric(substr(data$CI_obj, 1, 4)) 
data$upper_CI_obj <- as.numeric(substr(data$CI_obj, 6, 10))  
# Subjective
data$lower_CI_subj <- as.numeric(substr(data$CI_subj, 1, 4))  
data$upper_CI_subj <- as.numeric(substr(data$CI_subj, 6, 10)) 

# Add a column with the prevalence of the MH outcome in unexposed participants (e.g., coded as "none" on graphs)
# Tool for calculating prevalence from graphs (specific to the exposure and outcome): https://apps.automeris.io/wpd/

# Step 1. Convert relative risks to odds ratios 
# Objective measures
# Convert RR to odds ratio
data$Effect_size_obj[data$Type_of_effect_size_obj == "risk_ratio"] <- 
  RR_to_OR(RR = data$Effect_size_obj[data$Type_of_effect_size_obj == "risk_ratio"],
           p = data$prevalence_unexposed[data$Type_of_effect_size_obj == "risk_ratio"]) # to change the column name with prevalence in unexposed

# Convert SE of RR to SE of log odds ratio
data$SE_obj[data$Type_of_effect_size_obj == "risk_ratio"] <- 
  RR_to_OR_SE(RR = data$Effect_size_obj[data$Type_of_effect_size_obj == "risk_ratio"],
              RR_lowCI = data$lower_CI_obj[data$Type_of_effect_size_obj == "risk_ratio"],
              RR_upCI = data$upper_CI_obj[data$Type_of_effect_size_obj == "risk_ratio"],
              p = data$prevalence_unexposed[data$Type_of_effect_size_obj == "risk_ratio"]) # to change the column name with prevalence in unexposed
# Specify that effects have been changed to odds ratios
data$Type_of_effect_size_obj[data$Type_of_effect_size_obj == "risk_ratio"] <- "odds_ratio"

# Repeat for subjective measure
# Convert RR to odds ratio
data$Effect_size_subj[data$Type_of_effect_size_subj == "risk_ratio"] <- 
  RR_to_OR(RR = data$Effect_size_subj[data$Type_of_effect_size_subj == "risk_ratio"],
           p = data$prevalence_unexposed[data$Type_of_effect_size_subj == "risk_ratio"]) # to change the column name with prevalence in unexposed
# Convert SE of RR to SE of log odds ratio
data$SE_subj[data$Type_of_effect_size_subj == "risk_ratio"] <- 
  RR_to_OR_SE(RR = data$Effect_size_subj[data$Type_of_effect_size_subj == "risk_ratio"],
              RR_lowCI = data$lower_CI_subj[data$Type_of_effect_size_subj == "risk_ratio"],
              RR_upCI = data$upper_CI_subj[data$Type_of_effect_size_subj == "risk_ratio"],
              p = data$prevalence_unexposed[data$Type_of_effect_size_subj == "risk_ratio"]) # to change the column name with prevalence in unexposed
```

    ## Warning in log(OR): NaNs produced

``` r
# Specify that effects have been changed to odds ratios
data$Type_of_effect_size_subj[data$Type_of_effect_size_subj == "risk_ratio"] <- "odds_ratio"

## Step 2. Convert odds ratios to correlation coefficients
# Convert objective effect sizes 
data$r_objective_es[data$Type_of_effect_size_obj == "odds_ratio"] <- 
  OR_to_r(OR = data$Effect_size_obj[data$Type_of_effect_size_obj == "odds_ratio"],
          logSE = data$SE_obj[data$Type_of_effect_size_obj == "odds_ratio"])
# Convert SE to variance
data$r_objective_var[data$Type_of_effect_size_obj == "odds_ratio"] <- 
  OR_to_r_variance(OR = data$Effect_size_obj[data$Type_of_effect_size_obj == "odds_ratio"],
                   logSE = data$SE_obj[data$Type_of_effect_size_obj == "odds_ratio"])
mean(data$r_objective_es[!is.na(data$r_objective_es)])
```

    ## [1] 0.02998731

``` r
# Convert subjective effect sizes 
data$r_subjective_es[data$Type_of_effect_size_subj == "odds_ratio"] <- 
  OR_to_r(OR = data$Effect_size_subj[data$Type_of_effect_size_subj == "odds_ratio"],
          logSE = data$SE_subj[data$Type_of_effect_size_subj == "odds_ratio"])
# Convert SE to variance
data$r_subjective_var[data$Type_of_effect_size_subj == "odds_ratio"] <- 
  OR_to_r_variance(OR = data$Effect_size_subj[data$Type_of_effect_size_subj == "odds_ratio"],
                   logSE = data$SE_subj[data$Type_of_effect_size_subj == "odds_ratio"])
```

###### 2.3.6 Convert r to Fisher’s Z

``` r
# Convert r to Fishers-Z
# Formula: z_r = tanh^{-1}(r) = \frac{1}{2}log≤ft ( \frac{1+r}{1-r}\right )
# From: https://rdrr.io/cran/DescTools/man/FisherZ.html

# Create new variables for Fishers-Z conversion
data$z_r_subj <- NA
data$z_r_obj <- NA

# Convert subjective measure correlations to fishersz
data$z_r_subj <- fisherz(data$r_subjective_es)
# Compare variables for subjective effect sizes
list(data$z_r_subj)
```

    ## [[1]]
    ##   [1]  0.2346444084  0.2307429894  0.1562004905  0.1751572427  0.1872034279
    ##   [6]  0.0811265606  0.2265204604  0.4996895705  0.1220439516  0.1516708113
    ##  [11]  0.2992306559  0.2228977607  0.2190809172  0.2816278080  0.4100903822
    ##  [16]  0.0858411686  0.1728438419  0.2877603888  0.1613312629  0.1552258853
    ##  [21]  0.0014245024  0.0048744847  0.2338333456  0.2422281341  0.2554128119
    ##  [26]  0.0199986669  0.2328890704  0.0649543160  0.0749298649  0.1178581254
    ##  [31] -0.0109176597  0.0481355768  0.0091053970  0.1149271279  0.0136067876
    ##  [36]  0.0232383829  0.0171950030  0.0055132657  0.0000000000 -0.0082698462
    ##  [41]  0.0027566433  0.2379334497  0.2801507221  0.2479599190  0.0009616589
    ##  [46]  0.2082732228  0.0092561793  0.0250052103  0.0420247222  0.2586153849
    ##  [51]  0.1716666635 -0.1134846845 -0.0701146707 -0.0170016380  0.3884230997
    ##  [56]  0.1521636112 -0.0050000417  0.3791856017  0.2037744382  0.0310099361
    ##  [61] -0.0300090049  0.2479599190  0.0330119868  0.0597078022  0.0636022638
    ##  [66]  0.0154984708  0.0509723393  0.0290377587  0.0330580170  0.2156916725
    ##  [71]  0.1222353761  0.0655170027  0.2895093978  0.0232302478  0.0615603168
    ##  [76]  0.1102090437  0.0189110941  0.1863443437  0.1055549868  0.1172281649
    ##  [81]  0.0768726266  0.0472276918  0.0559903652  0.2491724547  0.2443680614
    ##  [86]  0.0633170181  0.0712367470  0.1103281806  0.1176739584  0.2066864279
    ##  [91]  0.2097877160  0.3258745277  0.2311501939  0.0909913902  0.0908518832
    ##  [96]  0.1548380524  0.1572634710  0.3345473059  0.3209984648  0.1125002436
    ## [101]  0.1231354438  0.0419130724  0.2648590138  0.2767670979  0.1403448251
    ## [106]  0.1768401539  0.0763555341  0.2306691180  0.3107644996  0.3535544637
    ## [111]  0.2966398388  0.0739008155  0.1183458033  0.0767490804  0.0907937554
    ## [116]  0.1464433594  0.1044976148  0.0361875413  0.3871834363  0.1200701863
    ## [121]  0.1522622318  0.2679280685  0.2041794094  0.0321796559 -0.0827290501
    ## [126]  0.2790223479  0.2942044712  0.1337926421  0.2511506657  0.1861197302
    ## [131] -0.0741355202 -0.0741355202  0.3586223661  0.0922608889  0.0232805409
    ## [136]  0.3304774762  0.5320172332  0.3198783443  0.2239178688  0.1937178364
    ## [141] -0.0054899171

``` r
list(data$r_subjective_es)
```

    ## [[1]]
    ##   [1]  0.2304308301  0.2267332648  0.1549424098  0.1733876775  0.1850467988
    ##   [6]  0.0809490492  0.2227239928  0.4618729857  0.1214416030  0.1505184007
    ##  [11]  0.2906083995  0.2192782354  0.2156418912  0.2744109837  0.3885494201
    ##  [16]  0.0856309421  0.1711429287  0.2800721747  0.1599459853  0.1539910588
    ##  [21]  0.0014245014  0.0048744461  0.2296626900  0.2375992080  0.2500000000
    ##  [26]  0.0199960012  0.2287680269  0.0648631211  0.0747899482  0.1173154362
    ##  [31] -0.0109172260  0.0480984340  0.0091051454  0.1144237918  0.0136059480
    ##  [36]  0.0232342007  0.0171933086  0.0055132098  0.0000000000 -0.0082696576
    ##  [41]  0.0027566363  0.2335428589  0.2730445715  0.2430000000  0.0009616586
    ##  [46]  0.2053131000  0.0092559150  0.0250000000  0.0420000000  0.2530000000
    ##  [51]  0.1700000000 -0.1130000000 -0.0700000000 -0.0170000000  0.3700000000
    ##  [56]  0.1510000000 -0.0050000000  0.3620000000  0.2010000000  0.0310000000
    ##  [61] -0.0300000000  0.2430000000  0.0330000000  0.0596369500  0.0635166400
    ##  [66]  0.0154972300  0.0509282400  0.0290296000  0.0330459800  0.2124079000
    ##  [71]  0.1216302000  0.0654234200  0.2816832000  0.0232260700  0.0614826700
    ##  [76]  0.1097650000  0.0189088400  0.1842170000  0.1051647000  0.1166941000
    ##  [81]  0.0767215600  0.0471926100  0.0559319300  0.2441406000  0.2396173000
    ##  [86]  0.0632325400  0.0711164900  0.1098827000  0.1171338000  0.2037927000
    ##  [91]  0.2067633000  0.3148090000  0.2271195000  0.0907411000  0.0906027400
    ##  [96]  0.1536124000  0.1559797000  0.3226007960  0.3104094593  0.1120280216
    ## [101]  0.1225168549  0.0418885467  0.2588346880  0.2699103241  0.1394305855
    ## [106]  0.1750195171  0.0762074908  0.2266631897  0.3011324313  0.3395240481
    ## [111]  0.2882346034  0.0737665765  0.1177963747  0.0765987398  0.0905450891
    ## [116]  0.1454054043  0.1041189062  0.0361717533  0.3689295561  0.1194964832
    ## [121]  0.1510963705  0.2616958483  0.2013885784  0.0321685528 -0.0825408302
    ## [126]  0.2720000000  0.2860000000  0.1330000000  0.2460000000  0.1840000000
    ## [131] -0.0740000000 -0.0740000000  0.3440000000  0.0920000000  0.0232763359
    ## [136]  0.3189497452  0.4869215637  0.3093969154  0.2202490763  0.1913304753
    ## [141] -0.0054898620

``` r
# Convert objective measure correlations to fishersz
data$z_r_obj <-fisherz(data$r_objective_es)
# Compare variables for objective effect sizes
list(data$z_r_obj)
```

    ## [[1]]
    ##   [1] -0.0554569700 -0.0506800178 -0.0224954654 -0.1443015851 -0.0275737034
    ##   [6] -0.0537940397  0.0453268036  0.2245394119 -0.0153521771 -0.2020009741
    ##  [11] -0.0637244019 -0.2298055750  0.0056271239 -0.0254299745 -0.0205493988
    ##  [16] -0.0273124662  0.0208394970  0.0351269813  0.0184449428  0.0060499631
    ##  [21] -0.0005982907 -0.0033604262  0.0690579803  0.0054564011 -0.0200026673
    ##  [26] -0.0599640582  0.0049999792  0.0499791901  0.4812118251  0.2775154602
    ##  [31] -0.0174514358  0.0042282131 -0.0073826845  0.2736187308 -0.0558199856
    ##  [36] -0.0090337030  0.0118592920  0.0055132657  0.0027566433  0.0055132657
    ##  [41] -0.0137827976  0.0287644036  0.0525231481  0.0520469455  0.1201809867
    ##  [46]  0.0358369438 -0.0262110709  0.0841984087  0.0420247222 -0.2205056985
    ##  [51] -0.2152645384  0.3260865532  0.2184078190  0.1266732165 -0.3586223661
    ##  [56]  0.0952873493  0.1511404359  0.0852056003 -0.0100003334  0.1074111759
    ##  [61]  0.0020000027  0.0902441879 -0.0330119868  0.0016878596  0.0030237052
    ##  [66]  0.0062548296  0.0051701061  0.0066133274  0.0035754332  0.0028070004
    ##  [71]  0.0056254013  0.0019967007  0.0013852119  0.0022953500  0.0014151229
    ##  [76]  0.0013698519  0.0036801616  0.0017144377  0.1729232398  0.0809009169
    ##  [81]  0.1201482181 -0.0039801200 -0.0465077226 -0.0434733265 -0.0328627951
    ##  [86]  0.0245187021 -0.0125085223 -0.0174998662  0.0271622380  0.0485682227
    ##  [91]  0.0364330715 -0.1440668081 -0.0035725312 -0.0268268838 -0.0274107630
    ##  [96]  0.0146660414  0.0082963323  0.1842269332  0.2222849827  0.1526306425
    ## [101]  0.2496328495  0.1208073831  0.2282456964  0.2282997811  0.1424423725
    ## [106]  0.0845804678  0.0728137418  0.0834027744 -0.0698025519  0.1448804197
    ## [111]  0.1447018943 -0.0286948370 -0.0464406648 -0.0271705668 -0.1091332403
    ## [116] -0.2189895718 -0.1684728052 -0.0457054553  0.1761533786  0.0678988661
    ## [121] -0.0714835555  0.0974003734 -0.0046730845 -0.0238682287 -0.0065605379
    ## [126] -0.1348107879 -0.1501175746 -0.0410229969 -0.0250052103 -0.0060000720
    ## [131]  0.1985693327  0.1985693327 -0.0771527213  0.0922608889  0.0056565489
    ## [136]  0.1724294906  0.4687979809  0.1929232392  0.1886900007  0.2217236417
    ## [141]  0.0502384782

``` r
list(data$r_objective_es)
```

    ## [[1]]
    ##   [1] -0.0554001877 -0.0506366724 -0.0224916716 -0.1433082631 -0.0275667174
    ##   [6] -0.0537422099  0.0452957875  0.2208403875 -0.0153509711 -0.1992975806
    ##  [11] -0.0636382845 -0.2258438521  0.0056270645 -0.0254244942 -0.0205465068
    ##  [16] -0.0273056768  0.0208364807  0.0351125406  0.0184428514  0.0060498893
    ##  [21] -0.0005982906 -0.0033604136  0.0689484100  0.0054563470 -0.0200000000
    ##  [26] -0.0598922907  0.0049999375  0.0499376169  0.4472135955  0.2706040268
    ##  [31] -0.0174496644  0.0042281879 -0.0073825503  0.2669888476 -0.0557620818
    ##  [36] -0.0090334572  0.0118587361  0.0055132098  0.0027566363  0.0055132098
    ##  [41] -0.0137819250  0.0287564730  0.0524749031  0.0520000000  0.1196057000
    ##  [46]  0.0358216100 -0.0262050700  0.0840000000  0.0420000000 -0.2170000000
    ##  [51] -0.2120000000  0.3150000000  0.2150000000  0.1260000000 -0.3440000000
    ##  [56]  0.0950000000  0.1500000000  0.0850000000 -0.0100000000  0.1070000000
    ##  [61]  0.0020000000  0.0900000000 -0.0330000000  0.0016878580  0.0030236960
    ##  [66]  0.0062547480  0.0051700600  0.0066132310  0.0035754180  0.0028069930
    ##  [71]  0.0056253420  0.0019966980  0.0013852110  0.0022953460  0.0014151220
    ##  [76]  0.0013698510  0.0036801450  0.0017144360  0.1712200000  0.0807248800
    ##  [81]  0.1195734000 -0.0039800990 -0.0464742200 -0.0434459600 -0.0328509700
    ##  [86]  0.0245137900 -0.0125078700 -0.0174980800  0.0271555600  0.0485300700
    ##  [91]  0.0364169600 -0.1430783000 -0.0035725160 -0.0268204500 -0.0274039000
    ##  [96]  0.0146649900  0.0082961420  0.1821706506  0.2186948433  0.1514563503
    ## [101]  0.2445735045  0.1202230892  0.2243630153  0.2244143769  0.1414867504
    ## [106]  0.0843793512  0.0726853318  0.0832099271 -0.0696894039  0.1438751629
    ## [111]  0.1437003285 -0.0286869639 -0.0464073069 -0.0271638827 -0.1087020332
    ## [116] -0.2155547918 -0.1668967698 -0.0456736558  0.1743536991  0.0677947145
    ## [121] -0.0713620459  0.0970935307 -0.0046730505 -0.0238636972 -0.0065604438
    ## [126] -0.1340000000 -0.1490000000 -0.0410000000 -0.0250000000 -0.0060000000
    ## [131]  0.1960000000  0.1960000000 -0.0770000000  0.0920000000  0.0056564885
    ## [136]  0.1707406852  0.4372275944  0.1905648499  0.1864820721  0.2181602843
    ## [141]  0.0501962551

``` r
# Derive variables for variance of Fisher's Z
data$z_var_subj <- NA
data$z_var_obj <- NA

# Calculate variance of Fisher's Z
data$z_var_subj <- 1 / (data$Subj_vs_none-3) 
data$z_var_obj <- 1 / (data$Obj_vs_none-3)

mean(data$z_r_subj[!is.na(data$z_r_subj)])
```

    ## [1] 0.144897

``` r
mean(data$z_r_obj[!is.na(data$z_r_obj)])
```

    ## [1] 0.03074388

``` r
library(metafor)
data$es_id <- 1:nrow(data)
data$ref <- paste0(data$Author, "_", data$Year)
data$cohort_name <- paste0(data$Cohort)

# Check included studies
data %>% dplyr::filter (!is.na(z_r_subj) & !is.na(z_var_subj)) %>%
  dplyr::select(ref, cohort_name)
```

    ##                             ref                               cohort_name
    ## 1         Danese & Widom.,_2020                             Widom_Midwest
    ## 2         Danese & Widom.,_2020                             Widom_Midwest
    ## 3         Danese & Widom.,_2020                             Widom_Midwest
    ## 4         Danese & Widom.,_2020                             Widom_Midwest
    ## 5         Danese & Widom.,_2020                             Widom_Midwest
    ## 6         Danese & Widom.,_2020                             Widom_Midwest
    ## 7         Danese & Widom.,_2020                             Widom_Midwest
    ## 8         Danese & Widom.,_2020                             Widom_Midwest
    ## 9         Danese & Widom.,_2020                             Widom_Midwest
    ## 10        Danese & Widom.,_2020                             Widom_Midwest
    ## 11        Danese & Widom.,_2020                             Widom_Midwest
    ## 12        Danese & Widom.,_2020                             Widom_Midwest
    ## 13        Danese & Widom.,_2020                             Widom_Midwest
    ## 14        Danese & Widom.,_2020                             Widom_Midwest
    ## 15        Danese & Widom.,_2020                             Widom_Midwest
    ## 16        Danese & Widom.,_2020                             Widom_Midwest
    ## 17        Danese & Widom.,_2020                             Widom_Midwest
    ## 18        Danese & Widom.,_2020                             Widom_Midwest
    ## 19        Danese & Widom.,_2020                             Widom_Midwest
    ## 20         Gromann et al.,_2013                       Gromann_Netherlands
    ## 21  Zimmer-Gembeck et al.,_2012                  Zimmer-Gembeck_Australia
    ## 22  Zimmer-Gembeck et al.,_2012                  Zimmer-Gembeck_Australia
    ## 23          Bouman et al.,_2012                        Bouman_Netherlands
    ## 24          Bouman et al.,_2012                        Bouman_Netherlands
    ## 25      Graham & Juvonen.,_1998                             Graham_USA_LA
    ## 26         Graham, et al.,_2003                             Graham_USA_LA
    ## 27         Graham, et al.,_2003                             Graham_USA_LA
    ## 28         Graham, et al.,_2003                             Graham_USA_LA
    ## 29         Graham, et al.,_2003                             Graham_USA_LA
    ## 30           White et al.,_2016                                  LONGSCAN
    ## 31           White et al.,_2016                                  LONGSCAN
    ## 32           White et al.,_2016                                  LONGSCAN
    ## 33           White et al.,_2016                                  LONGSCAN
    ## 34           White et al.,_2016                                  LONGSCAN
    ## 35           White et al.,_2016                                  LONGSCAN
    ## 36           White et al.,_2016                                  LONGSCAN
    ## 37           White et al.,_2016                                  LONGSCAN
    ## 38           White et al.,_2016                                  LONGSCAN
    ## 39           White et al.,_2016                                  LONGSCAN
    ## 40           White et al.,_2016                                  LONGSCAN
    ## 41           White et al.,_2016                                  LONGSCAN
    ## 42         Newbury et al.,_2017                                    E-RISK
    ## 43  Goldman-Mellor et al.,_2016 California Health Interview Survey (CHIS)
    ## 44           McGee et al.,_1995                          Carnochan_Canada
    ## 45           McGee et al.,_1995                          Carnochan_Canada
    ## 46           McGee et al.,_1995                          Carnochan_Canada
    ## 47           McGee et al.,_1995                          Carnochan_Canada
    ## 48           McGee et al.,_1995                          Carnochan_Canada
    ## 49           McGee et al.,_1995                          Carnochan_Canada
    ## 50           McGee et al.,_1995                          Carnochan_Canada
    ## 51           McGee et al.,_1995                          Carnochan_Canada
    ## 52           McGee et al.,_1995                          Carnochan_Canada
    ## 53           McGee et al.,_1995                          Carnochan_Canada
    ## 54           McGee et al.,_1995                          Carnochan_Canada
    ## 55           McGee et al.,_1995                          Carnochan_Canada
    ## 56           McGee et al.,_1995                          Carnochan_Canada
    ## 57           McGee et al.,_1995                          Carnochan_Canada
    ## 58           McGee et al.,_1995                          Carnochan_Canada
    ## 59           McGee et al.,_1995                          Carnochan_Canada
    ## 60           McGee et al.,_1995                          Carnochan_Canada
    ## 61           McGee et al.,_1995                          Carnochan_Canada
    ## 62           McGee et al.,_1995                          Carnochan_Canada
    ## 63           McGee et al.,_1995                          Carnochan_Canada
    ## 64          Kochel et al.,_2017                               Rudolph_USA
    ## 65          Kochel et al.,_2017                               Rudolph_USA
    ## 66          Kochel et al.,_2017                               Rudolph_USA
    ## 67          Kochel et al.,_2017                               Rudolph_USA
    ## 68          Kochel et al.,_2017                               Rudolph_USA
    ## 69          Kochel et al.,_2017                               Rudolph_USA
    ## 70          Kochel et al.,_2017                               Rudolph_USA
    ## 71          Kochel et al.,_2017                               Rudolph_USA
    ## 72          Kochel et al.,_2017                               Rudolph_USA
    ## 73          Kochel et al.,_2017                               Rudolph_USA
    ## 74          Kochel et al.,_2017                               Rudolph_USA
    ## 75          Kochel et al.,_2017                               Rudolph_USA
    ## 76          Kochel et al.,_2017                               Rudolph_USA
    ## 77          Kochel et al.,_2017                               Rudolph_USA
    ## 78          Kochel et al.,_2017                               Rudolph_USA
    ## 79          Mulder et al.,_2017                      van_Aken_Netherlands
    ## 80          Mulder et al.,_2017                      van_Aken_Netherlands
    ## 81          Mulder et al.,_2017                      van_Aken_Netherlands
    ## 82         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 83         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 84         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 85         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 86         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 87         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 88         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 89         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 90         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 91         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 92         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 93         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 94         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 95         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 96         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 97         Mcclain et al.,_2020                   Elledge_US_Southeastern
    ## 98         Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 99         Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 100        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 101        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 102        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 103        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 104        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 105        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 106        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 107        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 108        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 109        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 110        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 111        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 112        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 113        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 114        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 115        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 116        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 117        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 118        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 119        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 120        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 121        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 122        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 123        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 124        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 125        Negriff et al.,_2017                     Trickett_US_WestCoast
    ## 126        Everson et al.,_2008                                  LONGSCAN
    ## 127        Everson et al.,_2008                                  LONGSCAN
    ## 128        Everson et al.,_2008                                  LONGSCAN
    ## 129        Everson et al.,_2008                                  LONGSCAN
    ## 130        Everson et al.,_2008                                  LONGSCAN
    ## 131        Everson et al.,_2008                                  LONGSCAN
    ## 132        Everson et al.,_2008                                  LONGSCAN
    ## 133        Everson et al.,_2008                                  LONGSCAN
    ## 134        Everson et al.,_2008                                  LONGSCAN
    ## 135       Flanagan et al.,_2008                  Bierman_US_Pennsylvania.
    ## 136            Cho et al.,_2016                                     SPARK
    ## 137            Cho et al.,_2016                                     SPARK
    ## 138            Cho et al.,_2016                                     SPARK
    ## 139            Cho et al.,_2016                                     SPARK
    ## 140            Cho et al.,_2016                                     SPARK
    ## 141            Cho et al.,_2016                                     SPARK

##### 2.4 Descriptive statistics

``` r
# Number of studies
k_studies <- data %>%
  group_by(Author) %>%
  dplyr::summarise(m = max(Author)) %>% nrow()
k_studies
```

    ## [1] 17

``` r
# Number of cohorts
k_cohort <- data %>%
  group_by(cohort_name) %>%
  dplyr::summarise(m = max(cohort_name)) %>% nrow()
k_cohort
```

    ## [1] 15

``` r
# Total sample size
data %>%
  group_by(cohort_name) %>%
  dplyr::summarise(m = max(N_Included))%>%
  dplyr::summarise(sum = sum(m))
```

    ## # A tibble: 1 × 1
    ##     sum
    ##   <dbl>
    ## 1 14789

``` r
# Average proportion female
perc_female <- data %>%
  group_by(cohort_name) %>%
  dplyr::summarise(mean_percent_female = mean(percent_female)) %>%
  dplyr::summarise(overall=mean(mean_percent_female, na.rm=TRUE))
perc_female
```

    ## # A tibble: 1 × 1
    ##   overall
    ##     <dbl>
    ## 1    54.1

``` r
# Age at self-reported adversity
Age_self <- data %>%
  group_by(cohort_name) %>%
  dplyr::summarise(mean_age_self = mean(Age_self)) %>%
  dplyr::summarise(overall=mean(mean_age_self, na.rm=TRUE))
Age_self
```

    ## # A tibble: 1 × 1
    ##   overall
    ##     <dbl>
    ## 1    14.3

``` r
describe(data$Age_self)
```

    ##    vars   n  mean   sd median trimmed  mad  min  max range skew kurtosis   se
    ## X1    1 141 15.91 5.77   13.8   15.16 2.82 9.16 28.7 19.54 1.23     0.52 0.49

``` r
# Number of effect sizes
n_es_obj <- length(data$r_objective_es[!is.na(data$r_objective_es)])
n_es_obj
```

    ## [1] 141

``` r
n_es_subj<-length(data$r_subjective_es[!is.na(data$r_subjective_es)])
n_es_subj
```

    ## [1] 141

``` r
# Effect sizes by exposure type
# Maltreatment
n_es_obj_mal <- length(data$r_objective_es[!is.na(data$r_objective_es) & data$exposure_type_broad=="maltreatment"])
n_es_obj_mal*2
```

    ## [1] 188

``` r
n_es_subj_mal <- length(data$r_subjective_es[!is.na(data$r_subjective_es) & data$exposure_type_broad=="maltreatment"])
n_es_subj_mal
```

    ## [1] 94

``` r
# Bullying
n_es_obj_bully <- length(data$r_objective_es[!is.na(data$r_objective_es) & data$exposure_type_broad=="bullying"])
n_es_obj_bully*2
```

    ## [1] 90

``` r
n_es_subj_bully <- length(data$r_subjective_es[!is.na(data$r_subjective_es) & data$exposure_type_broad=="bullying"])
n_es_subj_bully
```

    ## [1] 45

``` r
# Neighbourhood
n_es_obj_neigh <- length(data$r_objective_es[!is.na(data$r_objective_es) & data$exposure_type_broad=="neighbourhood"])
n_es_obj_neigh
```

    ## [1] 2

``` r
n_es_subj_neigh <- length(data$r_subjective_es[!is.na(data$r_subjective_es) & data$exposure_type_broad=="neighbourhood"])
n_es_subj_neigh
```

    ## [1] 2

``` r
# Number of studies by adversity
# Maltreatment
k_studies <- data %>%
  group_by(Author) %>%
  filter(exposure_type_broad=="maltreatment") %>%
  dplyr::summarise(m = max(Author)) %>% nrow()
k_studies
```

    ## [1] 6

``` r
# Bullying
k_studies <- data %>%
  group_by(Author) %>%
  filter(exposure_type_broad=="bullying") %>%
  dplyr::summarise(m = max(Author)) %>% nrow()
k_studies
```

    ## [1] 9

``` r
# Neighbourhood
k_studies <- data %>%
  group_by(Author) %>%
  filter(exposure_type_broad=="neighbourhood") %>%
  dplyr::summarise(m = max(Author)) %>% nrow()
k_studies
```

    ## [1] 2

##### 2.5 Do objective and subjective measures of childhood adversity independently predict psychopathology?

###### 2.5.1 Maltreatment

``` r
# Check included studies & cohorts
data %>% dplyr::filter (!is.na(z_r_subj) & !is.na(z_var_subj) & exposure_type_broad=="maltreatment") %>%
  dplyr::select(ref, cohort_name)
```

    ##                      ref           cohort_name
    ## 1  Danese & Widom.,_2020         Widom_Midwest
    ## 2  Danese & Widom.,_2020         Widom_Midwest
    ## 3  Danese & Widom.,_2020         Widom_Midwest
    ## 4  Danese & Widom.,_2020         Widom_Midwest
    ## 5  Danese & Widom.,_2020         Widom_Midwest
    ## 6  Danese & Widom.,_2020         Widom_Midwest
    ## 7  Danese & Widom.,_2020         Widom_Midwest
    ## 8  Danese & Widom.,_2020         Widom_Midwest
    ## 9  Danese & Widom.,_2020         Widom_Midwest
    ## 10 Danese & Widom.,_2020         Widom_Midwest
    ## 11 Danese & Widom.,_2020         Widom_Midwest
    ## 12 Danese & Widom.,_2020         Widom_Midwest
    ## 13 Danese & Widom.,_2020         Widom_Midwest
    ## 14 Danese & Widom.,_2020         Widom_Midwest
    ## 15 Danese & Widom.,_2020         Widom_Midwest
    ## 16 Danese & Widom.,_2020         Widom_Midwest
    ## 17 Danese & Widom.,_2020         Widom_Midwest
    ## 18 Danese & Widom.,_2020         Widom_Midwest
    ## 19 Danese & Widom.,_2020         Widom_Midwest
    ## 20    White et al.,_2016              LONGSCAN
    ## 21    White et al.,_2016              LONGSCAN
    ## 22    White et al.,_2016              LONGSCAN
    ## 23    White et al.,_2016              LONGSCAN
    ## 24    White et al.,_2016              LONGSCAN
    ## 25    White et al.,_2016              LONGSCAN
    ## 26    White et al.,_2016              LONGSCAN
    ## 27    White et al.,_2016              LONGSCAN
    ## 28    White et al.,_2016              LONGSCAN
    ## 29    White et al.,_2016              LONGSCAN
    ## 30    White et al.,_2016              LONGSCAN
    ## 31    White et al.,_2016              LONGSCAN
    ## 32    McGee et al.,_1995      Carnochan_Canada
    ## 33    McGee et al.,_1995      Carnochan_Canada
    ## 34    McGee et al.,_1995      Carnochan_Canada
    ## 35    McGee et al.,_1995      Carnochan_Canada
    ## 36    McGee et al.,_1995      Carnochan_Canada
    ## 37    McGee et al.,_1995      Carnochan_Canada
    ## 38    McGee et al.,_1995      Carnochan_Canada
    ## 39    McGee et al.,_1995      Carnochan_Canada
    ## 40    McGee et al.,_1995      Carnochan_Canada
    ## 41    McGee et al.,_1995      Carnochan_Canada
    ## 42    McGee et al.,_1995      Carnochan_Canada
    ## 43    McGee et al.,_1995      Carnochan_Canada
    ## 44    McGee et al.,_1995      Carnochan_Canada
    ## 45    McGee et al.,_1995      Carnochan_Canada
    ## 46    McGee et al.,_1995      Carnochan_Canada
    ## 47    McGee et al.,_1995      Carnochan_Canada
    ## 48    McGee et al.,_1995      Carnochan_Canada
    ## 49    McGee et al.,_1995      Carnochan_Canada
    ## 50    McGee et al.,_1995      Carnochan_Canada
    ## 51    McGee et al.,_1995      Carnochan_Canada
    ## 52  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 53  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 54  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 55  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 56  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 57  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 58  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 59  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 60  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 61  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 62  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 63  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 64  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 65  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 66  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 67  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 68  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 69  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 70  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 71  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 72  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 73  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 74  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 75  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 76  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 77  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 78  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 79  Negriff et al.,_2017 Trickett_US_WestCoast
    ## 80  Everson et al.,_2008              LONGSCAN
    ## 81  Everson et al.,_2008              LONGSCAN
    ## 82  Everson et al.,_2008              LONGSCAN
    ## 83  Everson et al.,_2008              LONGSCAN
    ## 84  Everson et al.,_2008              LONGSCAN
    ## 85  Everson et al.,_2008              LONGSCAN
    ## 86  Everson et al.,_2008              LONGSCAN
    ## 87  Everson et al.,_2008              LONGSCAN
    ## 88  Everson et al.,_2008              LONGSCAN
    ## 89      Cho et al.,_2016                 SPARK
    ## 90      Cho et al.,_2016                 SPARK
    ## 91      Cho et al.,_2016                 SPARK
    ## 92      Cho et al.,_2016                 SPARK
    ## 93      Cho et al.,_2016                 SPARK
    ## 94      Cho et al.,_2016                 SPARK

``` r
# Subjective
subj_maltreatment <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref,  ~ 1 | cohort_name),
                            data = data, subset=exposure_type_broad=="maltreatment" & !is.na(exposure_type_broad), slab=paste(ref, es_id, sep=", "))
subj_maltreatment
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0091  0.0956     94     no        es_id 
    ## sigma^2.2  0.0058  0.0761      6     no          ref 
    ## sigma^2.3  0.0005  0.0217      5     no  cohort_name 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 93) = 551.2306, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub     ​ 
    ##   0.1572  0.0356  4.4192  <.0001  0.0875  0.2269  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
round(fisherz2r(c(subj_maltreatment$beta, subj_maltreatment$ci.lb, subj_maltreatment$ci.ub)),2)
```

    ## [1] 0.16 0.09 0.22

``` r
# Objective 
obj_maltreatment <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref,  ~ 1 | cohort_name),
                           data = data, subset=exposure_type_broad=="maltreatment" & !is.na(exposure_type_broad), slab=paste(ref, es_id, sep=", "))
obj_maltreatment
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0100  0.1000     94     no        es_id 
    ## sigma^2.2  0.0000  0.0000      6     no          ref 
    ## sigma^2.3  0.0063  0.0792      5     no  cohort_name 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 93) = 405.4683, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub   ​ 
    ##   0.0559  0.0383  1.4580  0.1448  -0.0192  0.1311    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
round(fisherz2r(c(obj_maltreatment$beta, obj_maltreatment$ci.lb, obj_maltreatment$ci.ub)),2)
```

    ## [1]  0.06 -0.02  0.13

###### 2.5.2 Bullying

``` r
# Check included studies & cohorts
data %>% dplyr::filter (!is.na(z_r_subj) & !is.na(z_var_subj) & exposure_type_broad=="bullying") %>%
  dplyr::select(ref, cohort_name)
```

    ##                            ref              cohort_name
    ## 1         Gromann et al.,_2013      Gromann_Netherlands
    ## 2  Zimmer-Gembeck et al.,_2012 Zimmer-Gembeck_Australia
    ## 3  Zimmer-Gembeck et al.,_2012 Zimmer-Gembeck_Australia
    ## 4          Bouman et al.,_2012       Bouman_Netherlands
    ## 5          Bouman et al.,_2012       Bouman_Netherlands
    ## 6      Graham & Juvonen.,_1998            Graham_USA_LA
    ## 7         Graham, et al.,_2003            Graham_USA_LA
    ## 8         Graham, et al.,_2003            Graham_USA_LA
    ## 9         Graham, et al.,_2003            Graham_USA_LA
    ## 10        Graham, et al.,_2003            Graham_USA_LA
    ## 11         Kochel et al.,_2017              Rudolph_USA
    ## 12         Kochel et al.,_2017              Rudolph_USA
    ## 13         Kochel et al.,_2017              Rudolph_USA
    ## 14         Kochel et al.,_2017              Rudolph_USA
    ## 15         Kochel et al.,_2017              Rudolph_USA
    ## 16         Kochel et al.,_2017              Rudolph_USA
    ## 17         Kochel et al.,_2017              Rudolph_USA
    ## 18         Kochel et al.,_2017              Rudolph_USA
    ## 19         Kochel et al.,_2017              Rudolph_USA
    ## 20         Kochel et al.,_2017              Rudolph_USA
    ## 21         Kochel et al.,_2017              Rudolph_USA
    ## 22         Kochel et al.,_2017              Rudolph_USA
    ## 23         Kochel et al.,_2017              Rudolph_USA
    ## 24         Kochel et al.,_2017              Rudolph_USA
    ## 25         Kochel et al.,_2017              Rudolph_USA
    ## 26         Mulder et al.,_2017     van_Aken_Netherlands
    ## 27         Mulder et al.,_2017     van_Aken_Netherlands
    ## 28         Mulder et al.,_2017     van_Aken_Netherlands
    ## 29        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 30        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 31        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 32        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 33        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 34        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 35        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 36        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 37        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 38        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 39        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 40        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 41        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 42        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 43        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 44        Mcclain et al.,_2020  Elledge_US_Southeastern
    ## 45       Flanagan et al.,_2008 Bierman_US_Pennsylvania.

``` r
# Subjective 
subj_bullying <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref,  ~ 1 | cohort_name),
                        data = data, subset=exposure_type_broad=="bullying" & !is.na(exposure_type_broad), slab=paste(ref, es_id, sep=", "))
subj_bullying
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0037  0.0607     45     no        es_id 
    ## sigma^2.2  0.0026  0.0509      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 44) = 158.0148, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub     ​ 
    ##   0.1222  0.0230  5.3223  <.0001  0.0772  0.1672  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
round(fisherz2r(c(subj_bullying$beta, subj_bullying$ci.lb, subj_bullying$ci.ub)),2)
```

    ## [1] 0.12 0.08 0.17

``` r
# Objective 
obj_bullying <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref,  ~ 1 | cohort_name),
                       data = data, subset=exposure_type_broad=="bullying" & !is.na(exposure_type_broad), slab=paste(ref, es_id, sep=", "))
obj_bullying
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0053  0.0727     45     no        es_id 
    ## sigma^2.2  0.0018  0.0429      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 44) = 231.9681, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub   ​ 
    ##   0.0333  0.0219  1.5166  0.1294  -0.0097  0.0763    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
round(fisherz2r(c(obj_bullying$beta, obj_bullying$ci.lb, obj_bullying$ci.ub)),2)
```

    ## [1]  0.03 -0.01  0.08

###### 2.5.3 Neighbourhood

``` r
# Subjective 
subj_neighbour<- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref),
                        data = data, subset=exposure_type_broad=="neighbourhood" & !is.na(exposure_type_broad), slab=paste(ref, es_id, sep=", "))
subj_neighbour
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 2; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed  factor 
    ## sigma^2.1  0.0003  0.0164      2     no   es_id 
    ## sigma^2.2  0.0003  0.0164      2     no     ref 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 1) = 2.5138, p-val = 0.1129
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval   ci.lb   ci.ub     ​ 
    ##   0.2621  0.0209  12.5528  <.0001  0.2212  0.3031  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
round(fisherz2r(c(subj_neighbour$beta, subj_neighbour$ci.lb, subj_neighbour$ci.ub)),2)
```

    ## [1] 0.26 0.22 0.29

``` r
# Objective
obj_neighbour <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref),
                        data = data, subset=exposure_type_broad=="neighbourhood" & !is.na(exposure_type_broad), slab=paste(ref, es_id, sep=", "))
obj_neighbour
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 2; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed  factor 
    ## sigma^2.1  0.0000  0.0000      2     no   es_id 
    ## sigma^2.2  0.0000  0.0000      2     no     ref 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 1) = 0.7962, p-val = 0.3722
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub     ​ 
    ##   0.0450  0.0124  3.6348  0.0003  0.0207  0.0693  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
round(fisherz2r(c(obj_neighbour$beta, obj_neighbour$ci.lb, obj_neighbour$ci.ub)),2)
```

    ## [1] 0.04 0.02 0.07

###### 2.5.4 Comparing Estimates of Independent Meta-Analyses or Subgroups

``` r
## Comparing effect sizes from neighbourhood objective & subjective meta-analyses
# Example: https://www.metafor-project.org/doku.php/tips:comp_two_independent_estimates 
# subj_neighbour Subjective meta-analysis
# obj_neighbour Objective meta-analysis

# Fit two separate random-effects models within each subset - define by subsetting
res1 <- rma(z_r_subj, z_var_subj, data=data, subset=Subj_type_neigh=="self_report")
res2 <- rma(z_r_obj, z_var_obj, data=data, subset=Obj_type_neigh=="records")

# Combine estimates and standard errors from each model (effect of subjective measures, 
# then effect of objective measures) into a data frame
dat.comp <- data.frame(estimate = c(coef(obj_neighbour), coef(subj_neighbour)), 
                       stderror = c(obj_neighbour$se, subj_neighbour$se),
                       meta = c("records", "self_report"), tau2 = round(c(obj_neighbour$tau2, subj_neighbour$tau2),3))
dat.comp
```

    ##     estimate   stderror        meta tau2
    ## 1 0.04500792 0.01238254     records    0
    ## 2 0.26212691 0.02088201 self_report    0

``` r
# Compare the two estimates by feeding them back to the rma() function and 
# using the variable to distinguish the two estimates as a moderator
rma(estimate, sei=stderror, mods = ~ meta, method="FE", data=dat.comp, digits=3)
```

    ## 
    ## Fixed-Effects with Moderators Model (k = 2)
    ## 
    ## I^2 (residual heterogeneity / unaccounted variability): 0.00%
    ## H^2 (unaccounted variability / sampling variability):   1.00
    ## R^2 (amount of heterogeneity accounted for):            NA%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 0) = 0.000, p-val = 1.000
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 79.983, p-val < .001
    ## 
    ## Model Results:
    ## 
    ##                  estimate     se   zval   pval  ci.lb  ci.ub     ​ 
    ## intrcpt             0.045  0.012  3.635  <.001  0.021  0.069  *** 
    ## metaself_report     0.217  0.024  8.943  <.001  0.170  0.265  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

###### 2.5.5 Wald-type test for confirmation

``` r
with(dat.comp, round(c(zval = (estimate[1] - estimate[2])/sqrt(stderror[1]^2 + stderror[2]^2)), 3))
```

    ##   zval 
    ## -8.943

##### 2.6 Forest plots for meta-analysis

###### 2.6.1 Subjective Maltreatment

``` r
maltreatment_data <- subset(data, exposure_type_broad=="maltreatment")
maltreatment_data2 <- maltreatment_data[order(maltreatment_data$Author, maltreatment_data$Year),] #re-order
maltreatment_data2$refPlot <- paste0(maltreatment_data2$Author, " ", maltreatment_data2$Year)

# Remove underscores
maltreatment_data2$MH_Outcome <- gsub("_Symptoms","", maltreatment_data2$MH_Outcome)
maltreatment_data2$MH_Outcome <- gsub("_Diagnosis","", maltreatment_data2$MH_Outcome)

maltreatment_data2$MH_Outcome
```

    ##  [1] "Internalising"                   "Externalising"                  
    ##  [3] "Internalising"                   "Externalising"                  
    ##  [5] "Internalising"                   "Externalising"                  
    ##  [7] "Any_Psychopathology"             "Any_Internalising_Disorder"     
    ##  [9] "Any_Externalising_Disorder"      "Depression"                     
    ## [11] "Dysthymia"                       "Generalized_Anxiety_Disorder"   
    ## [13] "PTSD"                            "Antisocial_Personality_Disorder"
    ## [15] "Alcohol_Abuse_And_Or_Dependence" "Drug_Abuse_And_Or_Dependence"   
    ## [17] "Any_Psychopathology"             "Any_Internalising_Disorder"     
    ## [19] "Any_Externalising_Disorder"      "Any_Psychopathology"            
    ## [21] "Any_Internalising_Disorder"      "Any_Externalising_Disorder"     
    ## [23] "Any_Psychopathology"             "Any_Internalising_Disorder"     
    ## [25] "Any_Externalising_Disorder"      "Psychological_Adjustment_TSC"   
    ## [27] "Psychological_Adjustment_YSR"    "Psychological_Adjustment_CBCL"  
    ## [29] "Psychological_Adjustment_TSC"    "Psychological_Adjustment_YSR"   
    ## [31] "Psychological_Adjustment_CBCL"   "Psychological_Adjustment_TSC"   
    ## [33] "Psychological_Adjustment_YSR"    "Psychological_Adjustment_CBCL"  
    ## [35] "Internalising_CBCL"              "Externalising_CBCL"             
    ## [37] "Internalising_YSR"               "Externalising_YSR"              
    ## [39] "Internalising_CBCL"              "Externalising_CBCL"             
    ## [41] "Internalising_YSR"               "Externalising_YSR"              
    ## [43] "Internalising_CBCL"              "Externalising_CBCL"             
    ## [45] "Internalising_YSR"               "Externalising_YSR"              
    ## [47] "Internalising_CBCL"              "Externalising_CBCL"             
    ## [49] "Internalising_YSR"               "Externalising_YSR"              
    ## [51] "Internalising_CBCL"              "Externalising_CBCL"             
    ## [53] "Internalising_YSR"               "Externalising_YSR"              
    ## [55] "Depression"                      "PTSD"                           
    ## [57] "Anxiety"                         "Marijuana_Use"                  
    ## [59] "Alcohol_Use"                     "Person_Offences_Externalising"  
    ## [61] "Property_Offences_Externalising" "Depression"                     
    ## [63] "PTSD"                            "Anxiety"                        
    ## [65] "Marijuana_Use"                   "Alcohol_Use"                    
    ## [67] "Person_Offences_Externalising"   "Property_Offences_Externalising"
    ## [69] "Depression"                      "PTSD"                           
    ## [71] "Anxiety"                         "Marijuana_Use"                  
    ## [73] "Alcohol_Use"                     "Person_Offences_Externalising"  
    ## [75] "Property_Offences_Externalising" "Depression"                     
    ## [77] "PTSD"                            "Anxiety"                        
    ## [79] "Marijuana_Use"                   "Alcohol_Use"                    
    ## [81] "Person_Offences_Externalising"   "Property_Offences_Externalising"
    ## [83] "Anxiety"                         "Anxiety"                        
    ## [85] "Anxiety"                         "Anxiety"                        
    ## [87] "Depression"                      "Depression"                     
    ## [89] "Depression"                      "Depression"                     
    ## [91] "Suicidal_Thoughts"               "Suicidal_Thoughts"              
    ## [93] "Suicidal_Thoughts"               "Suicidal_Thoughts"

``` r
maltreatment_data2$Exposure_Type_Subj <- gsub("_"," ", maltreatment_data2$Exposure_Type_Subj)
maltreatment_data2$MH_Outcome <- gsub("_"," ", maltreatment_data2$MH_Outcome)

maltreatment_data2$Exposure_Type_Subj
```

    ##  [1] "Physical Abuse"                    "Physical Abuse"                   
    ##  [3] "Sexual Abuse"                      "Sexual Abuse"                     
    ##  [5] "Emotional Abuse"                   "Emotional Abuse"                  
    ##  [7] "Child Maltreatment"                "Child Maltreatment"               
    ##  [9] "Child Maltreatment"                "Child Maltreatment"               
    ## [11] "Child Maltreatment"                "Child Maltreatment"               
    ## [13] "Child Maltreatment"                "Child Maltreatment"               
    ## [15] "Child Maltreatment"                "Child Maltreatment"               
    ## [17] "Child Physical Abuse"              "Child Physical Abuse"             
    ## [19] "Child Physical Abuse"              "Child Sexual Abuse"               
    ## [21] "Child Sexual Abuse"                "Child Sexual Abuse"               
    ## [23] "Child Neglect"                     "Child Neglect"                    
    ## [25] "Child Neglect"                     "Physical Abuse"                   
    ## [27] "Physical Abuse"                    "Physical Abuse"                   
    ## [29] "Sexual Abuse"                      "Sexual Abuse"                     
    ## [31] "Sexual Abuse"                      "Psychological Abuse"              
    ## [33] "Psychological Abuse"               "Psychological Abuse"              
    ## [35] "Physical Violence"                 "Physical Violence"                
    ## [37] "Physical Violence"                 "Physical Violence"                
    ## [39] "Family Violence"                   "Family Violence"                  
    ## [41] "Family Violence"                   "Family Violence"                  
    ## [43] "Sexual"                            "Sexual"                           
    ## [45] "Sexual"                            "Sexual"                           
    ## [47] "Emotional"                         "Emotional"                        
    ## [49] "Emotional"                         "Emotional"                        
    ## [51] "Neglect"                           "Neglect"                          
    ## [53] "Neglect"                           "Neglect"                          
    ## [55] "Sexual Abuse"                      "Sexual Abuse"                     
    ## [57] "Sexual Abuse"                      "Sexual Abuse"                     
    ## [59] "Sexual Abuse"                      "Sexual Abuse"                     
    ## [61] "Sexual Abuse"                      "Physical Abuse"                   
    ## [63] "Physical Abuse"                    "Physical Abuse"                   
    ## [65] "Physical Abuse"                    "Physical Abuse"                   
    ## [67] "Physical Abuse"                    "Physical Abuse"                   
    ## [69] "Emotional Abuse"                   "Emotional Abuse"                  
    ## [71] "Emotional Abuse"                   "Emotional Abuse"                  
    ## [73] "Emotional Abuse"                   "Emotional Abuse"                  
    ## [75] "Emotional Abuse"                   "Neglect"                          
    ## [77] "Neglect"                           "Neglect"                          
    ## [79] "Neglect"                           "Neglect"                          
    ## [81] "Neglect"                           "Neglect"                          
    ## [83] "Psychological Safety And Security" "Acceptance And Self-Esteem"       
    ## [85] "Autonomy"                          "Restriction"                      
    ## [87] "Psychological Safety And Security" "Acceptance And Self-Esteem"       
    ## [89] "Autonomy"                          "Restriction"                      
    ## [91] "Psychological Safety And Security" "Acceptance And Self-Esteem"       
    ## [93] "Autonomy"                          "Restriction"

``` r
# Run meta-analysis again - subjective
subj_maltreatment <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref,  ~ 1 | cohort_name),
                            data = maltreatment_data2, subset=exposure_type_broad=="maltreatment" & !is.na(exposure_type_broad), 
                            slab=paste(ref, es_id, sep=", "))
subj_maltreatment
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0091  0.0956     94     no        es_id 
    ## sigma^2.2  0.0058  0.0761      6     no          ref 
    ## sigma^2.3  0.0005  0.0217      5     no  cohort_name 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 93) = 551.2306, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub     ​ 
    ##   0.1572  0.0356  4.4192  <.0001  0.0875  0.2269  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Subjective malteatment - forest plot
forest(subj_maltreatment,
       transf=transf.ztor, 
       order = order(maltreatment_data2$Author, maltreatment_data2$Year), 
        xlim=c(-2.0, 1.5),at=c(-0.8, -0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1), 
       xlab = "Correlation",
       mlab="Multivariate Meta-Analysis Model", 
       #header=c("Author", "Year", "Correlation"),
       slab=NA, 
       ilab=cbind(maltreatment_data2$refPlot, maltreatment_data2$Exposure_Type_Subj, maltreatment_data2$MH_Outcome), 
       ilab.xpos=c(-2.0, -1.55, -1.1), cex=0.35, 
       ilab.pos = c(4, 4, 4, 4),
       top=2) 
par(font=2)
text(c(-1.86, -1.38, -1.0, 1.33), 97, c("Reference", "Maltreatment type", "Outcome", "Correlation [95% CI]"), cex=0.4)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

###### 2.6.2 Objective Maltreatment

``` r
# Remove underscores
maltreatment_data2$Exposure_Type_Obj <- gsub("_"," ", maltreatment_data$Exposure_Type_Obj)
maltreatment_data2$MH_Outcome <- gsub("_"," ", maltreatment_data2$MH_Outcome) #re-order
maltreatment_data2$refPlot <- paste0(maltreatment_data2$Author, " ", maltreatment_data2$Year)

maltreatment_data2$Exposure_Type_Obj
```

    ##  [1] "Child Maltreatment"                "Child Maltreatment"               
    ##  [3] "Child Maltreatment"                "Child Maltreatment"               
    ##  [5] "Child Maltreatment"                "Child Maltreatment"               
    ##  [7] "Child Maltreatment"                "Child Maltreatment"               
    ##  [9] "Child Maltreatment"                "Child Maltreatment"               
    ## [11] "Child Physical Abuse"              "Child Physical Abuse"             
    ## [13] "Child Physical Abuse"              "Child Sexual Abuse"               
    ## [15] "Child Sexual Abuse"                "Child Sexual Abuse"               
    ## [17] "Child Neglect"                     "Child Neglect"                    
    ## [19] "Child Neglect"                     "Psychological Safety And Security"
    ## [21] "Acceptance And Self-Esteem"        "Autonomy"                         
    ## [23] "Restriction"                       "Psychological Safety And Security"
    ## [25] "Acceptance And Self-Esteem"        "Autonomy"                         
    ## [27] "Restriction"                       "Psychological Safety And Security"
    ## [29] "Acceptance And Self-Esteem"        "Autonomy"                         
    ## [31] "Restriction"                       "Physical Violence"                
    ## [33] "Physical Violence"                 "Physical Violence"                
    ## [35] "Physical Violence"                 "Family Violence"                  
    ## [37] "Family Violence"                   "Family Violence"                  
    ## [39] "Family Violence"                   "Sexual"                           
    ## [41] "Sexual"                            "Sexual"                           
    ## [43] "Sexual"                            "Emotional"                        
    ## [45] "Emotional"                         "Emotional"                        
    ## [47] "Emotional"                         "Neglect"                          
    ## [49] "Neglect"                           "Neglect"                          
    ## [51] "Neglect"                           "Sexual Abuse"                     
    ## [53] "Sexual Abuse"                      "Sexual Abuse"                     
    ## [55] "Sexual Abuse"                      "Sexual Abuse"                     
    ## [57] "Sexual Abuse"                      "Sexual Abuse"                     
    ## [59] "Physical Abuse"                    "Physical Abuse"                   
    ## [61] "Physical Abuse"                    "Physical Abuse"                   
    ## [63] "Physical Abuse"                    "Physical Abuse"                   
    ## [65] "Physical Abuse"                    "Emotional Abuse"                  
    ## [67] "Emotional Abuse"                   "Emotional Abuse"                  
    ## [69] "Emotional Abuse"                   "Emotional Abuse"                  
    ## [71] "Emotional Abuse"                   "Emotional Abuse"                  
    ## [73] "Neglect"                           "Neglect"                          
    ## [75] "Neglect"                           "Neglect"                          
    ## [77] "Neglect"                           "Neglect"                          
    ## [79] "Neglect"                           "Physical Abuse"                   
    ## [81] "Physical Abuse"                    "Physical Abuse"                   
    ## [83] "Sexual Abuse"                      "Sexual Abuse"                     
    ## [85] "Sexual Abuse"                      "Psychological Abuse"              
    ## [87] "Psychological Abuse"               "Psychological Abuse"              
    ## [89] "Physical Abuse"                    "Physical Abuse"                   
    ## [91] "Sexual Abuse"                      "Sexual Abuse"                     
    ## [93] "Emotional Abuse"                   "Emotional Abuse"

``` r
maltreatment_data2$MH_Outcome
```

    ##  [1] "Internalising"                   "Externalising"                  
    ##  [3] "Internalising"                   "Externalising"                  
    ##  [5] "Internalising"                   "Externalising"                  
    ##  [7] "Any Psychopathology"             "Any Internalising Disorder"     
    ##  [9] "Any Externalising Disorder"      "Depression"                     
    ## [11] "Dysthymia"                       "Generalized Anxiety Disorder"   
    ## [13] "PTSD"                            "Antisocial Personality Disorder"
    ## [15] "Alcohol Abuse And Or Dependence" "Drug Abuse And Or Dependence"   
    ## [17] "Any Psychopathology"             "Any Internalising Disorder"     
    ## [19] "Any Externalising Disorder"      "Any Psychopathology"            
    ## [21] "Any Internalising Disorder"      "Any Externalising Disorder"     
    ## [23] "Any Psychopathology"             "Any Internalising Disorder"     
    ## [25] "Any Externalising Disorder"      "Psychological Adjustment TSC"   
    ## [27] "Psychological Adjustment YSR"    "Psychological Adjustment CBCL"  
    ## [29] "Psychological Adjustment TSC"    "Psychological Adjustment YSR"   
    ## [31] "Psychological Adjustment CBCL"   "Psychological Adjustment TSC"   
    ## [33] "Psychological Adjustment YSR"    "Psychological Adjustment CBCL"  
    ## [35] "Internalising CBCL"              "Externalising CBCL"             
    ## [37] "Internalising YSR"               "Externalising YSR"              
    ## [39] "Internalising CBCL"              "Externalising CBCL"             
    ## [41] "Internalising YSR"               "Externalising YSR"              
    ## [43] "Internalising CBCL"              "Externalising CBCL"             
    ## [45] "Internalising YSR"               "Externalising YSR"              
    ## [47] "Internalising CBCL"              "Externalising CBCL"             
    ## [49] "Internalising YSR"               "Externalising YSR"              
    ## [51] "Internalising CBCL"              "Externalising CBCL"             
    ## [53] "Internalising YSR"               "Externalising YSR"              
    ## [55] "Depression"                      "PTSD"                           
    ## [57] "Anxiety"                         "Marijuana Use"                  
    ## [59] "Alcohol Use"                     "Person Offences Externalising"  
    ## [61] "Property Offences Externalising" "Depression"                     
    ## [63] "PTSD"                            "Anxiety"                        
    ## [65] "Marijuana Use"                   "Alcohol Use"                    
    ## [67] "Person Offences Externalising"   "Property Offences Externalising"
    ## [69] "Depression"                      "PTSD"                           
    ## [71] "Anxiety"                         "Marijuana Use"                  
    ## [73] "Alcohol Use"                     "Person Offences Externalising"  
    ## [75] "Property Offences Externalising" "Depression"                     
    ## [77] "PTSD"                            "Anxiety"                        
    ## [79] "Marijuana Use"                   "Alcohol Use"                    
    ## [81] "Person Offences Externalising"   "Property Offences Externalising"
    ## [83] "Anxiety"                         "Anxiety"                        
    ## [85] "Anxiety"                         "Anxiety"                        
    ## [87] "Depression"                      "Depression"                     
    ## [89] "Depression"                      "Depression"                     
    ## [91] "Suicidal Thoughts"               "Suicidal Thoughts"              
    ## [93] "Suicidal Thoughts"               "Suicidal Thoughts"

``` r
# Run meta-analysis again - objective
obj_maltreatment2 <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref,  ~ 1 | cohort_name),
                                 data = maltreatment_data2, subset=exposure_type_broad=="maltreatment" & !is.na(exposure_type_broad), 
                                 slab=paste(ref, es_id, sep=", "))
obj_maltreatment2
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0100  0.1000     94     no        es_id 
    ## sigma^2.2  0.0000  0.0000      6     no          ref 
    ## sigma^2.3  0.0063  0.0792      5     no  cohort_name 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 93) = 405.4683, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub   ​ 
    ##   0.0559  0.0383  1.4580  0.1448  -0.0192  0.1311    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Objective malteatment - forest plot
forest(obj_maltreatment2,
            order = order(maltreatment_data2$Author, maltreatment_data2$Year),
            transf=transf.ztor, 
            # order studies from earliest to latest date
             xlim=c(-2.2, 1.5), at=c(-0.6,-0.4,-0.2,0,0.2,0.4,0.6), 
             xlab = "Correlation",
             mlab="Multivariate Meta-Analysis Model",
             #header=c("Author", "Year", "Correlation"),e
             slab=NA, 
             ilab=cbind(maltreatment_data2$refPlot, maltreatment_data2$Exposure_Type_Obj, maltreatment_data2$MH_Outcome), 
             ilab.xpos=c(-2.2, -1.7, -1.16), cex=0.38,
             ilab.pos=c(4, 4, 4, 4),
             top=2) 
      par(font=2)
      text(c(-2.1, -1.51, -1.05, 1.3), 97, c("Reference", "Maltreatment type", "Outcome", "Correlation [95% CI]"), cex=0.4)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

###### 2.6.3 Subjective Bullying

``` r
bullying_data <- subset(data, exposure_type_broad=="bullying")
bullying_data2 <- bullying_data[order(bullying_data$Author, bullying_data$Year),] #re-order
bullying_data2$refPlot <- paste0(bullying_data2$Author, " ", bullying_data2$Year)

# Remove underscores
bullying_data2$MH_Outcome <- gsub("_Symptoms","", bullying_data2$MH_Outcome)
bullying_data2$MH_Outcome <- gsub("_Diagnosis","", bullying_data2$MH_Outcome)

bullying_data2$MH_Outcome
```

    ##  [1] "Depression"            "Anxiety"               "Social_Anxiety"       
    ##  [4] "Social_Anxiety"        "Anxiety"               "Depression"           
    ##  [7] "Internalising"         "Externalising"         "Psychotic_Experiences"
    ## [10] "Depressive_Grade_5"    "Depressive_Grade_5"    "Depressive_Grade_6"   
    ## [13] "Depressive_Grade_6"    "Depressive_Grade_9"    "Depressive_Grade_9"   
    ## [16] "Depressive_Grade_9"    "Depressive_Grade_10"   "Depressive_Grade_10"  
    ## [19] "Depressive_Grade_10"   "Depressive_Grade_6"    "Depressive_Grade_6"   
    ## [22] "Depressive_Grade_10"   "Depressive_Grade_10"   "Depressive_Grade_10"  
    ## [25] "Depression_T2"         "Depression_T2"         "Depression_T2"        
    ## [28] "Depression_T2"         "Anxiety_T2"            "Anxiety_T2"           
    ## [31] "Anxiety_T2"            "Anxiety_T2"            "Depression_T1"        
    ## [34] "Depression_T1"         "Depression_T1"         "Depression_T1"        
    ## [37] "Anxiety_T1"            "Anxiety_T1"            "Anxiety_T1"           
    ## [40] "Anxiety_T1"            "Social_Anxiety_T1"     "Social_Anxiety_T2"    
    ## [43] "Social_Anxiety_T2"     "Depressive"            "Social_Anxiety"

``` r
bullying_data2$Exposure_Type_Subj <- gsub("_"," ", bullying_data2$Exposure_Type_Subj)
bullying_data2$MH_Outcome <- gsub("_"," ", bullying_data2$MH_Outcome)

bullying_data2$Exposure_Type_Subj
```

    ##  [1] "Bullying Victimization"    "Bullying Victimization"   
    ##  [3] "Bullying Victimization"    "Bullying Victimization"   
    ##  [5] "Bullying Victimization"    "Bullying Victimization"   
    ##  [7] "Bullying Victimization"    "Bullying Victimization"   
    ##  [9] "Bullying Victimization"    "Peer Victimization"       
    ## [11] "Peer Victimization"        "Peer Victimization"       
    ## [13] "Peer Victimization"        "Peer Victimization"       
    ## [15] "Peer Victimization"        "Peer Victimization"       
    ## [17] "Peer Victimization"        "Peer Victimization"       
    ## [19] "Peer Victimization"        "Peer Victimization"       
    ## [21] "Peer Victimization"        "Peer Victimization"       
    ## [23] "Peer Victimization"        "Peer Victimization"       
    ## [25] "Bullying Victimization"    "Bullying Victimization"   
    ## [27] "Bullying Victimization"    "Bullying Victimization"   
    ## [29] "Bullying Victimization"    "Bullying Victimization"   
    ## [31] "Bullying Victimization"    "Bullying Victimization"   
    ## [33] "Bullying Victimization"    "Bullying Victimization"   
    ## [35] "Bullying Victimization"    "Bullying Victimization"   
    ## [37] "Bullying Victimization"    "Bullying Victimization"   
    ## [39] "Bullying Victimization"    "Bullying Victimization"   
    ## [41] "Bullying Victimization T1" "Bullying Victimization T2"
    ## [43] "Bullying Victimization T1" "Bullying Victimization"   
    ## [45] "Bullying Victimization"

``` r
bullying_data2$Author <- gsub("_"," ", bullying_data2$Author)

# Run meta-analysis again - subjective
subj_bullying2 <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref,  ~ 1 | cohort_name),
                         data = bullying_data2, subset=exposure_type_broad=="bullying" & !is.na(exposure_type_broad), 
                         slab=paste(ref, es_id, sep=", "))
subj_bullying2
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0037  0.0607     45     no        es_id 
    ## sigma^2.2  0.0026  0.0509      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 44) = 158.0148, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub     ​ 
    ##   0.1222  0.0230  5.3223  <.0001  0.0772  0.1672  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Subjective bullying - forest plot
forest(subj_bullying2, 
       order = order(bullying_data2$Author, bullying_data2$Year), 
       transf=transf.ztor, 
       xlim=c(-2.0, 1.5), at=c(-0.2,0,0.2,0.4,0.6), 
       xlab = "Correlation", 
       mlab="Multivariate Meta-Analysis Model", 
       #header=c("Author", "Year", "Fishers-Z"),
       slab=NA, 
       ilab=cbind(bullying_data2$refPlot, bullying_data2$Exposure_Type_Subj, bullying_data2$MH_Outcome), 
       ilab.xpos=c(-2.0, -1.3, -0.7), cex=0.4, 
       ilab.pos = c(4, 4, 4, 4),
       top=2) 
par(font=2)
text(c(-1.82, -1.05, -0.55, 1.27), 47, c("Reference", "Bullying type", "Outcome", "Correlation [95% CI]"), cex=0.4)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

###### 2.6.4 Objective Bullying

``` r
# Remove underscores
bullying_data2$Exposure_Type_Obj <- gsub("_"," ", bullying_data2$Exposure_Type_Obj)
bullying_data2$MH_Outcome <- gsub("_"," ", bullying_data2$MH_Outcome)
bullying_data2$refPlot <- paste0(bullying_data2$Author, " ", bullying_data2$Year)

bullying_data2$Exposure_Type_Obj
```

    ##  [1] "Bullying Victimization"    "Bullying Victimization"   
    ##  [3] "Bullying Victimization"    "Bullying Victimization"   
    ##  [5] "Bullying Victimization"    "Bullying Victimization"   
    ##  [7] "Bullying Victimization"    "Bullying Victimization"   
    ##  [9] "Bullying Victimization"    "Peer Victimization"       
    ## [11] "Peer Victimization"        "Peer Victimization"       
    ## [13] "Peer Victimization"        "Peer Victimization"       
    ## [15] "Peer Victimization"        "Peer Victimization"       
    ## [17] "Peer Victimization"        "Peer Victimization"       
    ## [19] "Peer Victimization"        "Peer Victimization"       
    ## [21] "Peer Victimization"        "Peer Victimization"       
    ## [23] "Peer Victimization"        "Peer Victimization"       
    ## [25] "Overt Victimization"       "Relational Victimisation" 
    ## [27] "Overt Victimization"       "Relational Victimisation" 
    ## [29] "Overt Victimization"       "Relational Victimisation" 
    ## [31] "Overt Victimization"       "Relational Victimisation" 
    ## [33] "Overt Victimization"       "Relational Victimisation" 
    ## [35] "Overt Victimization"       "Relational Victimisation" 
    ## [37] "Overt Victimization"       "Relational Victimisation" 
    ## [39] "Overt Victimization"       "Relational Victimisation" 
    ## [41] "Bullying Victimization T1" "Bullying Victimization T1"
    ## [43] "Bullying Victimization T2" "Bullying Victimization"   
    ## [45] "Bullying Victimization"

``` r
bullying_data2$MH_Outcome
```

    ##  [1] "Depression"            "Anxiety"               "Social Anxiety"       
    ##  [4] "Social Anxiety"        "Anxiety"               "Depression"           
    ##  [7] "Internalising"         "Externalising"         "Psychotic Experiences"
    ## [10] "Depressive Grade 5"    "Depressive Grade 5"    "Depressive Grade 6"   
    ## [13] "Depressive Grade 6"    "Depressive Grade 9"    "Depressive Grade 9"   
    ## [16] "Depressive Grade 9"    "Depressive Grade 10"   "Depressive Grade 10"  
    ## [19] "Depressive Grade 10"   "Depressive Grade 6"    "Depressive Grade 6"   
    ## [22] "Depressive Grade 10"   "Depressive Grade 10"   "Depressive Grade 10"  
    ## [25] "Depression T2"         "Depression T2"         "Depression T2"        
    ## [28] "Depression T2"         "Anxiety T2"            "Anxiety T2"           
    ## [31] "Anxiety T2"            "Anxiety T2"            "Depression T1"        
    ## [34] "Depression T1"         "Depression T1"         "Depression T1"        
    ## [37] "Anxiety T1"            "Anxiety T1"            "Anxiety T1"           
    ## [40] "Anxiety T1"            "Social Anxiety T1"     "Social Anxiety T2"    
    ## [43] "Social Anxiety T2"     "Depressive"            "Social Anxiety"

``` r
# Run meta-analysis again - objective
obj_bullying2 <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref,  ~ 1 | cohort_name),
                        data = bullying_data2, subset=exposure_type_broad=="bullying" & !is.na(exposure_type_broad), 
                        slab=paste(ref, es_id, sep=", "))
obj_bullying2
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0053  0.0727     45     no        es_id 
    ## sigma^2.2  0.0018  0.0429      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 44) = 231.9681, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval    ci.lb   ci.ub   ​ 
    ##   0.0333  0.0219  1.5166  0.1294  -0.0097  0.0763    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Objective bullying - forest plot
forest(obj_bullying2,
       order = order(bullying_data2$Author, bullying_data2$Year),
       transf=transf.ztor, 
       xlim=c(-1.5, 0.9), at=c(-0.4,-0.2,0,0.2,0.4,0.6), 
       xlab = "Correlation", 
       mlab="Multivariate Meta-Analysis Model",
       slab=NA, 
      ilab=cbind(bullying_data2$refPlot, bullying_data2$Exposure_Type_Obj, bullying_data2$MH_Outcome), 
       ilab.xpos=c(-1.5, -1.0, -0.55), cex=0.35, 
       ilab.pos=c(4, 4, 4, 4),
       top=3)
par(font=2)
text(c(-1.37, -0.85, -0.45, 0.75), 47, c("Reference", "Bullying type", "Outcome", "Correlation [95% CI]"), cex=0.4)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

###### 2.6.5 Subjective Neighbourhood

``` r
neighbour_data <- subset(data, exposure_type_broad=="neighbourhood")
neighbour_data2 <- neighbour_data[order(neighbour_data$Author, neighbour_data$Year),] #re-order
neighbour_data2$refPlot <- paste0(neighbour_data2$Author, " ", neighbour_data2$Year)

# Remove underscores
neighbour_data2$MH_Outcome <- gsub("_Symptoms","", neighbour_data2$MH_Outcome)
neighbour_data2$MH_Outcome <- gsub("_Diagnosis","", neighbour_data2$MH_Outcome)

neighbour_data2$Exposure_Type_Subj <- gsub("_"," ", neighbour_data2$Exposure_Type_Subj)
neighbour_data2$MH_Outcome <- gsub("_"," ", neighbour_data2$MH_Outcome)
      
neighbour_data2$Exposure_Type_Subj
```

    ## [1] "Neighbourhood Safety"   "Neighbourhood Disorder"

``` r
neighbour_data2$MH_Outcome
```

    ## [1] "Serious Psychological Distress" "Psychotic Experiences"

``` r
# Run meta-analysis again - sujective
subj_neighbour2<- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref),
                        data = neighbour_data2, subset=exposure_type_broad=="neighbourhood" & !is.na(exposure_type_broad))

subj_neighbour2
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 2; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed  factor 
    ## sigma^2.1  0.0003  0.0164      2     no   es_id 
    ## sigma^2.2  0.0003  0.0164      2     no     ref 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 1) = 2.5138, p-val = 0.1129
    ## 
    ## Model Results:
    ## 
    ## estimate      se     zval    pval   ci.lb   ci.ub     ​ 
    ##   0.2621  0.0209  12.5528  <.0001  0.2212  0.3031  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Subjective neighbourhood - forest plot
forest(subj_neighbour2,
       order = order(neighbour_data2$Year, neighbour_data2$Author), 
       transf=transf.ztor, 
       xlim=c(-1.5, 0.9), at=c(-0.2,0,0.2,0.4,0.6), 
       xlab = "Correlation",
       mlab="Multivariate Meta-Analysis Model", 
       slab=NA, 
       ilab=cbind(neighbour_data2$refPlot, neighbour_data2$Exposure_Type_Subj, neighbour_data2$MH_Outcome),
       ilab.xpos=c(-1.52, -1.1, -0.7), cex=0.4, 
       ilab.pos = c(4, 4, 4, 4),
       top=3) 
par(font=2)
text(c(-1.39, -0.96, -0.6, 0.75), 3.2, c("Reference", "Neighbourhood type", "Outcome", "Correlation [95% CI]"), cex=0.4)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

###### 2.6.6 Objective Neighbourhood

``` r
# Remove underscores
neighbour_data$Exposure_Type_Obj <- gsub("_"," ", neighbour_data$Exposure_Type_Obj)
neighbour_data$MH_Outcome <- gsub("_"," ", neighbour_data$MH_Outcome)
neighbour_data2$refPlot <- paste0(neighbour_data2$Author, " ", neighbour_data2$Year)

neighbour_data2$Exposure_Type_Obj
```

    ## [1] "Neighbourhood_Violence" "Neighbourhood_Crime"

``` r
# Run meta-analysis again - objective
obj_neighbour2<- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref),
                        data = neighbour_data2, subset=exposure_type_broad=="neighbourhood" & !is.na(exposure_type_broad))

obj_neighbour2
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 2; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed  factor 
    ## sigma^2.1  0.0000  0.0000      2     no   es_id 
    ## sigma^2.2  0.0000  0.0000      2     no     ref 
    ## 
    ## Test for Heterogeneity:
    ## Q(df = 1) = 0.7962, p-val = 0.3722
    ## 
    ## Model Results:
    ## 
    ## estimate      se    zval    pval   ci.lb   ci.ub     ​ 
    ##   0.0450  0.0124  3.6348  0.0003  0.0207  0.0693  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Objective neighbourhood - forest plot
forest(obj_neighbour2, 
       order = order(neighbour_data2$Year, neighbour_data2$Author), 
       transf=transf.ztor,
       xlim=c(-1.5, 1.0), at=c(-0.2,0,0.2,0.4,0.6), 
       xlab = "Correlation", 
       mlab="Multivariate Meta-Analysis Model", 
       slab=NA, 
       ilab=cbind(neighbour_data2$refPlot, neighbour_data2$Exposure_Type_Obj, neighbour_data2$MH_Outcome), 
       ilab.xpos=c(-1.5, -1.05, -0.6), cex=0.5, 
       ilab.pos = c(4, 4, 4, 4),
       top=3) 
par(font=2)
text(c(-1.38, -0.87, -0.48, 0.85), 4.2, c("Reference", "Neighbourhood type", "Outcome", "Correlation [95% CI]"), cex=0.5)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

###### 2.6.7 Combined Subjective and Objective Maltreatment

``` r
library(metafor)

maltreatment_data <- subset(data, exposure_type_broad=="maltreatment")
# combine author and year into a single variable
maltreatment_data2$refPlot <- paste0(maltreatment_data2$Author, " ", maltreatment_data2$Year)

# Remove underscores
maltreatment_data2$MH_Outcome <- gsub("_Symptoms","", maltreatment_data2$MH_Outcome)
maltreatment_data2$MH_Outcome <- gsub("_Diagnosis","", maltreatment_data2$MH_Outcome)
maltreatment_data2$MH_Outcome
```

    ##  [1] "Internalising"                   "Externalising"                  
    ##  [3] "Internalising"                   "Externalising"                  
    ##  [5] "Internalising"                   "Externalising"                  
    ##  [7] "Any Psychopathology"             "Any Internalising Disorder"     
    ##  [9] "Any Externalising Disorder"      "Depression"                     
    ## [11] "Dysthymia"                       "Generalized Anxiety Disorder"   
    ## [13] "PTSD"                            "Antisocial Personality Disorder"
    ## [15] "Alcohol Abuse And Or Dependence" "Drug Abuse And Or Dependence"   
    ## [17] "Any Psychopathology"             "Any Internalising Disorder"     
    ## [19] "Any Externalising Disorder"      "Any Psychopathology"            
    ## [21] "Any Internalising Disorder"      "Any Externalising Disorder"     
    ## [23] "Any Psychopathology"             "Any Internalising Disorder"     
    ## [25] "Any Externalising Disorder"      "Psychological Adjustment TSC"   
    ## [27] "Psychological Adjustment YSR"    "Psychological Adjustment CBCL"  
    ## [29] "Psychological Adjustment TSC"    "Psychological Adjustment YSR"   
    ## [31] "Psychological Adjustment CBCL"   "Psychological Adjustment TSC"   
    ## [33] "Psychological Adjustment YSR"    "Psychological Adjustment CBCL"  
    ## [35] "Internalising CBCL"              "Externalising CBCL"             
    ## [37] "Internalising YSR"               "Externalising YSR"              
    ## [39] "Internalising CBCL"              "Externalising CBCL"             
    ## [41] "Internalising YSR"               "Externalising YSR"              
    ## [43] "Internalising CBCL"              "Externalising CBCL"             
    ## [45] "Internalising YSR"               "Externalising YSR"              
    ## [47] "Internalising CBCL"              "Externalising CBCL"             
    ## [49] "Internalising YSR"               "Externalising YSR"              
    ## [51] "Internalising CBCL"              "Externalising CBCL"             
    ## [53] "Internalising YSR"               "Externalising YSR"              
    ## [55] "Depression"                      "PTSD"                           
    ## [57] "Anxiety"                         "Marijuana Use"                  
    ## [59] "Alcohol Use"                     "Person Offences Externalising"  
    ## [61] "Property Offences Externalising" "Depression"                     
    ## [63] "PTSD"                            "Anxiety"                        
    ## [65] "Marijuana Use"                   "Alcohol Use"                    
    ## [67] "Person Offences Externalising"   "Property Offences Externalising"
    ## [69] "Depression"                      "PTSD"                           
    ## [71] "Anxiety"                         "Marijuana Use"                  
    ## [73] "Alcohol Use"                     "Person Offences Externalising"  
    ## [75] "Property Offences Externalising" "Depression"                     
    ## [77] "PTSD"                            "Anxiety"                        
    ## [79] "Marijuana Use"                   "Alcohol Use"                    
    ## [81] "Person Offences Externalising"   "Property Offences Externalising"
    ## [83] "Anxiety"                         "Anxiety"                        
    ## [85] "Anxiety"                         "Anxiety"                        
    ## [87] "Depression"                      "Depression"                     
    ## [89] "Depression"                      "Depression"                     
    ## [91] "Suicidal Thoughts"               "Suicidal Thoughts"              
    ## [93] "Suicidal Thoughts"               "Suicidal Thoughts"

``` r
maltreatment_data2$Exposure_Type_Subj <- gsub("_"," ", maltreatment_data2$Exposure_Type_Subj)
maltreatment_data2$MH_Outcome <- gsub("_"," ", maltreatment_data2$MH_Outcome)

maltreatment_data2$Exposure_Type_Subj
```

    ##  [1] "Physical Abuse"                    "Physical Abuse"                   
    ##  [3] "Sexual Abuse"                      "Sexual Abuse"                     
    ##  [5] "Emotional Abuse"                   "Emotional Abuse"                  
    ##  [7] "Child Maltreatment"                "Child Maltreatment"               
    ##  [9] "Child Maltreatment"                "Child Maltreatment"               
    ## [11] "Child Maltreatment"                "Child Maltreatment"               
    ## [13] "Child Maltreatment"                "Child Maltreatment"               
    ## [15] "Child Maltreatment"                "Child Maltreatment"               
    ## [17] "Child Physical Abuse"              "Child Physical Abuse"             
    ## [19] "Child Physical Abuse"              "Child Sexual Abuse"               
    ## [21] "Child Sexual Abuse"                "Child Sexual Abuse"               
    ## [23] "Child Neglect"                     "Child Neglect"                    
    ## [25] "Child Neglect"                     "Physical Abuse"                   
    ## [27] "Physical Abuse"                    "Physical Abuse"                   
    ## [29] "Sexual Abuse"                      "Sexual Abuse"                     
    ## [31] "Sexual Abuse"                      "Psychological Abuse"              
    ## [33] "Psychological Abuse"               "Psychological Abuse"              
    ## [35] "Physical Violence"                 "Physical Violence"                
    ## [37] "Physical Violence"                 "Physical Violence"                
    ## [39] "Family Violence"                   "Family Violence"                  
    ## [41] "Family Violence"                   "Family Violence"                  
    ## [43] "Sexual"                            "Sexual"                           
    ## [45] "Sexual"                            "Sexual"                           
    ## [47] "Emotional"                         "Emotional"                        
    ## [49] "Emotional"                         "Emotional"                        
    ## [51] "Neglect"                           "Neglect"                          
    ## [53] "Neglect"                           "Neglect"                          
    ## [55] "Sexual Abuse"                      "Sexual Abuse"                     
    ## [57] "Sexual Abuse"                      "Sexual Abuse"                     
    ## [59] "Sexual Abuse"                      "Sexual Abuse"                     
    ## [61] "Sexual Abuse"                      "Physical Abuse"                   
    ## [63] "Physical Abuse"                    "Physical Abuse"                   
    ## [65] "Physical Abuse"                    "Physical Abuse"                   
    ## [67] "Physical Abuse"                    "Physical Abuse"                   
    ## [69] "Emotional Abuse"                   "Emotional Abuse"                  
    ## [71] "Emotional Abuse"                   "Emotional Abuse"                  
    ## [73] "Emotional Abuse"                   "Emotional Abuse"                  
    ## [75] "Emotional Abuse"                   "Neglect"                          
    ## [77] "Neglect"                           "Neglect"                          
    ## [79] "Neglect"                           "Neglect"                          
    ## [81] "Neglect"                           "Neglect"                          
    ## [83] "Psychological Safety And Security" "Acceptance And Self-Esteem"       
    ## [85] "Autonomy"                          "Restriction"                      
    ## [87] "Psychological Safety And Security" "Acceptance And Self-Esteem"       
    ## [89] "Autonomy"                          "Restriction"                      
    ## [91] "Psychological Safety And Security" "Acceptance And Self-Esteem"       
    ## [93] "Autonomy"                          "Restriction"

``` r
# set layout of the plot
layout.matrix <- matrix(c(1,2), nrow = 1, ncol = 2)

layout(mat = layout.matrix,
       heights = c(1), # Heights of the two rows
       widths = c(3.5, 2)) # Widths of the two columns

layout.show(2)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
#par(mfrow=c(1,2))
par(mar=c(4,4,1,1))
plot<-forest(subj_maltreatment,
       transf=transf.ztor, 
       order = order(maltreatment_data2$Author, maltreatment_data2$Year), 
       xlim=c(-2, 1.5),at=c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8), 
       xlab = "Correlation",
       mlab="Multivariate Meta-Analysis Model", 
       #header=c("Author", "Year", "Correlation"),
       slab=NA, 
       ilab=cbind(maltreatment_data2$refPlot, maltreatment_data2$Exposure_Type_Subj, maltreatment_data2$MH_Outcome), 
       ilab.xpos=c(-2.0, -1.59, -1.1), 
       cex=0.35, 
       ilab.pos = c(4, 4, 4),
       textpos=c(-2, 1.2),
       top=2) 
par(font=2)
text(c(-1.86, -1.38, -1.0, 1.05), 97, c("Reference", "Maltreatment type", "Outcome", "Correlation [95% CI]"), cex=0.4)
text(0, 97, "A. Subjective measure of maltreatment", cex=.4, font=2)

par(mar=c(4,3,1,2))
forest(obj_maltreatment2,
       order = order(maltreatment_data2$Author, maltreatment_data2$Year),
       transf=transf.ztor, 
       # order studies from earliest to latest date
       xlim=c(1, 1.6), at=c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6, 0.8),
       xlab = "Correlation",
       mlab="",
       #header=c("Author", "Year", "Correlation"),e
       slab=NA,
       ilab.xpos=c(-1.2), cex=0.38,
       ilab.pos=c(4),
       textpos=c(-2, 1.4),
       top=2) 
par(font=2)
text(c(1.15), 97, c("Correlation [95% CI]"), cex=0.4)
text(0, 97, "B. Objective measure of maltreatment", cex=.4, font=2)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-40-2.png)<!-- -->

###### 2.6.8 Combined Subjective and Objective Bullying

``` r
bullying_data <- subset(data, exposure_type_broad=="bullying")

# Remove underscores
bullying_data2$MH_Outcome <- gsub("_Symptoms","", bullying_data2$MH_Outcome)
bullying_data2$MH_Outcome <- gsub("_Diagnosis","", bullying_data2$MH_Outcome)

bullying_data2$MH_Outcome
```

    ##  [1] "Depression"            "Anxiety"               "Social Anxiety"       
    ##  [4] "Social Anxiety"        "Anxiety"               "Depression"           
    ##  [7] "Internalising"         "Externalising"         "Psychotic Experiences"
    ## [10] "Depressive Grade 5"    "Depressive Grade 5"    "Depressive Grade 6"   
    ## [13] "Depressive Grade 6"    "Depressive Grade 9"    "Depressive Grade 9"   
    ## [16] "Depressive Grade 9"    "Depressive Grade 10"   "Depressive Grade 10"  
    ## [19] "Depressive Grade 10"   "Depressive Grade 6"    "Depressive Grade 6"   
    ## [22] "Depressive Grade 10"   "Depressive Grade 10"   "Depressive Grade 10"  
    ## [25] "Depression T2"         "Depression T2"         "Depression T2"        
    ## [28] "Depression T2"         "Anxiety T2"            "Anxiety T2"           
    ## [31] "Anxiety T2"            "Anxiety T2"            "Depression T1"        
    ## [34] "Depression T1"         "Depression T1"         "Depression T1"        
    ## [37] "Anxiety T1"            "Anxiety T1"            "Anxiety T1"           
    ## [40] "Anxiety T1"            "Social Anxiety T1"     "Social Anxiety T2"    
    ## [43] "Social Anxiety T2"     "Depressive"            "Social Anxiety"

``` r
bullying_data2$Exposure_Type_Subj <- gsub("_"," ", bullying_data2$Exposure_Type_Subj)
bullying_data2$MH_Outcome <- gsub("_"," ", bullying_data2$MH_Outcome)

bullying_data2$Exposure_Type_Subj
```

    ##  [1] "Bullying Victimization"    "Bullying Victimization"   
    ##  [3] "Bullying Victimization"    "Bullying Victimization"   
    ##  [5] "Bullying Victimization"    "Bullying Victimization"   
    ##  [7] "Bullying Victimization"    "Bullying Victimization"   
    ##  [9] "Bullying Victimization"    "Peer Victimization"       
    ## [11] "Peer Victimization"        "Peer Victimization"       
    ## [13] "Peer Victimization"        "Peer Victimization"       
    ## [15] "Peer Victimization"        "Peer Victimization"       
    ## [17] "Peer Victimization"        "Peer Victimization"       
    ## [19] "Peer Victimization"        "Peer Victimization"       
    ## [21] "Peer Victimization"        "Peer Victimization"       
    ## [23] "Peer Victimization"        "Peer Victimization"       
    ## [25] "Bullying Victimization"    "Bullying Victimization"   
    ## [27] "Bullying Victimization"    "Bullying Victimization"   
    ## [29] "Bullying Victimization"    "Bullying Victimization"   
    ## [31] "Bullying Victimization"    "Bullying Victimization"   
    ## [33] "Bullying Victimization"    "Bullying Victimization"   
    ## [35] "Bullying Victimization"    "Bullying Victimization"   
    ## [37] "Bullying Victimization"    "Bullying Victimization"   
    ## [39] "Bullying Victimization"    "Bullying Victimization"   
    ## [41] "Bullying Victimization T1" "Bullying Victimization T2"
    ## [43] "Bullying Victimization T1" "Bullying Victimization"   
    ## [45] "Bullying Victimization"

``` r
bullying_data2$Author <- gsub("_"," ", bullying_data2$Author)

# combine author and year into a single variable
bullying_data2$refPlot <- paste0(bullying_data2$Author, " ", bullying_data2$Year)

library(metafor)

# set layout of the plot
layout.matrix <- matrix(c(1,2), nrow = 1, ncol = 2)

layout(mat = layout.matrix,
       heights = c(1), # Heights of the two rows
       widths = c(3.5, 2)) # Widths of the two columns

layout.show(2)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
#par(mfrow=c(1,2))
par(mar=c(4,4,1,1))
plot<-forest(subj_bullying2,
       transf=transf.ztor, 
       order = order(bullying_data2$Author, bullying_data2$Year), 
       xlim=c(-2, 1.5),at=c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8), 
       xlab = "Correlation",
       mlab="Multivariate Meta-Analysis Model", 
       #header=c("Author", "Year", "Correlation"),
       slab=NA, 
       ilab=cbind(bullying_data2$refPlot, bullying_data2$Exposure_Type_Subj, bullying_data2$MH_Outcome), 
       ilab.xpos=c(-2.0, -1.48, -1.05), 
       cex=0.35, 
       ilab.pos = c(4, 4, 4),
       textpos=c(-2, 1.2),
       top=2) 
par(font=2)
text(c(-1.86, -1.3, -0.9, 1.05), 47, c("Reference", "Bullying type", "Outcome", "Correlation [95% CI]"), cex=0.4)
text(0, 47, "A. Subjective measure of bullying", cex=.4, font=2)

par(mar=c(4,3,1,2))
forest(obj_bullying2,
       order = order(bullying_data2$Author, bullying_data2$Year),
       transf=transf.ztor, 
       # order studies from earliest to latest date
       xlim=c(1, 1.5), at=c(-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6, 0.8), 
       xlab = "Correlation",
       mlab="",
       #header=c("Author", "Year", "Correlation"),e
       slab=NA,
       ilab.xpos=c(-1.2), cex=0.38,
       ilab.pos=c(4),
       textpos=c(-2, 1.4),
       top=2) 
par(font=2)
text(c(1.2), 47, c("Correlation [95% CI]"), cex=0.4)
text(0, 47, "B. Objective measure of bullying", cex=.4, font=2)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-41-2.png)<!-- -->

###### 2.6.9 Combined Subjective and Objective Neighbourhood

``` r
neighbour_data <- subset(data, exposure_type_broad=="neighbourhood")
# Remove underscores
neighbour_data2$MH_Outcome <- gsub("_Symptoms","", neighbour_data2$MH_Outcome)
neighbour_data2$MH_Outcome <- gsub("_Diagnosis","", neighbour_data2$MH_Outcome)

neighbour_data2$Exposure_Type_Subj <- gsub("_"," ", neighbour_data2$Exposure_Type_Subj)
neighbour_data2$MH_Outcome <- gsub("_"," ", neighbour_data2$MH_Outcome)
      
neighbour_data2$Exposure_Type_Subj
```

    ## [1] "Neighbourhood Safety"   "Neighbourhood Disorder"

``` r
neighbour_data2$MH_Outcome
```

    ## [1] "Serious Psychological Distress" "Psychotic Experiences"

``` r
# combine author and year into a single variable
neighbour_data2$refPlot <- paste0(neighbour_data2$Author, " ", neighbour_data2$Year)

library(metafor)

# set layout of the plot
layout.matrix <- matrix(c(1,2), nrow = 1, ncol = 2)

layout(mat = layout.matrix,
       heights = c(1), # Heights of the two rows
       widths = c(3.5, 2)) # Widths of the two columns

layout.show(2)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

``` r
#par(mfrow=c(1,2))
par(mar=c(4,4,1,1))
plot<-forest(subj_neighbour2,
       transf=transf.ztor, 
       order = order(neighbour_data2$Author, neighbour_data2$Year), 
         xlim=c(-2.0, 1.9), at=c(-0.4,-0.2,0,0.2,0.4), 
       xlab = "Correlation",
       mlab="Multivariate Meta-Analysis Model", 
       #header=c("Author", "Year", "Correlation"),
       slab=NA, 
       ilab=cbind(neighbour_data2$refPlot, neighbour_data2$Exposure_Type_Subj, neighbour_data2$MH_Outcome), 
       ilab.xpos=c(-2.0, -1.4, -0.85), 
       cex=0.5, 
       ilab.pos = c(4, 4, 4),
       textpos=c(-2, 1.2),
       top=2) 
par(font=2)
text(c(-1.8, -1.17, -0.7, 1.05), 3.3, c("Reference", "Neighbourhood type", "Outcome", "Correlation [95% CI]"), cex=0.5)
text(0, 3.3, "A. Subjective measure of neighbourhood", cex=.5, font=2)

par(mar=c(4,3,1,2))
forest(obj_neighbour2,
       order = order(neighbour_data2$Author, neighbour_data2$Year),
       transf=transf.ztor, 
       # order studies from earliest to latest date
         xlim=c(1, 1.9), at=c(-0.4,-0.2,0,0.2,0.4), 
       xlab = "Correlation",
       mlab="",
       #header=c("Author", "Year", "Correlation"),e
       slab=NA,
       ilab.xpos=c(-1.2), 
       cex=0.5,
       ilab.pos=c(4),
       textpos=c(-2, 1.7),
       top=2) 
par(font=2)
text(c(1.5), 3.3, c("Correlation [95% CI]"), cex=0.5)
text(0, 3.3, "B. Objective measure of neighbourhood", cex=.5, font=2)
```

![](Meta_analysis_objective_subjective_psychopathology_25June22_files/figure-gfm/unnamed-chunk-42-2.png)<!-- -->
\##### 2.7 What moderates the independent associations between objective
and subjective measure of childhood adversity and psychopathology?

##### 2.7.1 Combining moderation analyses for maltreatment & bullying

``` r
###### Informant for mental health #######
# Code as self vs other
data$Informant_MH_binary <- NA
data$Informant_MH_binary <- "self"
data$Informant_MH_binary[data$Informant_MH!="self-report"] <- "other"

## Subjective measure 
# test for overall moderation (QM, QM p value)
subj_mod_inform <- rma.mv(z_r_subj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                          data = data, mods = ~ Informant_MH_binary, subset=exposure_type_broad!="neighbourhood")
subj_mod_inform
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 139; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0062  0.0788    139     no        es_id 
    ## sigma^2.2  0.0058  0.0761     15     no          ref 
    ## sigma^2.3  0.0000  0.0000     13     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 137) = 638.1438, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 4.8731, p-val = 0.0273
    ## 
    ## Model Results:
    ## 
    ##                          estimate      se    zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt                    0.0714  0.0382  1.8681  0.0617  -0.0035  0.1463  . 
    ## Informant_MH_binaryself    0.0852  0.0386  2.2075  0.0273   0.0096  0.1608  * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# remove intercept to get effect sizes for each level
subj_mod_inform <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                   data = data, mods = ~ Informant_MH_binary -1, subset=exposure_type_broad!="neighbourhood")
subj_mod_inform
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 139; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0064  0.0797    139     no        es_id 
    ## sigma^2.2  0.0058  0.0763     15     no          ref 
    ## sigma^2.3  0.0000  0.0000     13     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 137) = 702.9785, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 40.2811, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##                           estimate      se    zval    pval    ci.lb   ci.ub​ 
    ## Informant_MH_binaryother    0.0715  0.0385  1.8591  0.0630  -0.0039  0.1469 
    ## Informant_MH_binaryself     0.1561  0.0246  6.3464  <.0001   0.1079  0.2043 
    ##  
    ## Informant_MH_binaryother    . 
    ## Informant_MH_binaryself   *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(subj_mod_inform$beta[2], subj_mod_inform$ci.lb[2], subj_mod_inform$ci.ub[2])),2) #self-report
```

    ## [1] 0.15 0.11 0.20

``` r
round(fisherz2r(c(subj_mod_inform$beta[1], subj_mod_inform$ci.lb[1], subj_mod_inform$ci.ub[1])),2) #other
```

    ## [1] 0.07 0.00 0.15

``` r
## Objective measure 
# test for overall moderation (QM, QM p value)
obj_mod_inform <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                  data = data, mods = ~ Informant_MH_binary, subset=exposure_type_broad!="neighbourhood")
obj_mod_inform
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 139; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0076  0.0870    139     no        es_id 
    ## sigma^2.2  0.0000  0.0000     15     no          ref 
    ## sigma^2.3  0.0025  0.0504     13     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 137) = 607.0090, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 6.3951, p-val = 0.0114
    ## 
    ## Model Results:
    ## 
    ##                          estimate      se     zval    pval    ci.lb    ci.ub​ 
    ## intrcpt                    0.1132  0.0334   3.3875  0.0007   0.0477   0.1787 
    ## Informant_MH_binaryself   -0.0906  0.0358  -2.5289  0.0114  -0.1607  -0.0204 
    ##  
    ## intrcpt                  *** 
    ## Informant_MH_binaryself    * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# remove intercept to get effect sizes for each level
obj_mod_inform <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                         data = data, mods = ~ Informant_MH_binary -1, subset=exposure_type_broad!="neighbourhood")
obj_mod_inform
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 139; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0076  0.0870    139     no        es_id 
    ## sigma^2.2  0.0000  0.0000     15     no          ref 
    ## sigma^2.3  0.0025  0.0504     13     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 137) = 607.0090, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 11.7633, p-val = 0.0028
    ## 
    ## Model Results:
    ## 
    ##                           estimate      se    zval    pval    ci.lb   ci.ub​ 
    ## Informant_MH_binaryother    0.1132  0.0334  3.3875  0.0007   0.0477  0.1787 
    ## Informant_MH_binaryself     0.0227  0.0201  1.1294  0.2587  -0.0167  0.0620 
    ##  
    ## Informant_MH_binaryother  *** 
    ## Informant_MH_binaryself 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(obj_mod_inform$beta[2], obj_mod_inform$ci.lb[2], obj_mod_inform$ci.ub[2])),2) #self-report
```

    ## [1]  0.02 -0.02  0.06

``` r
round(fisherz2r(c(obj_mod_inform$beta[1], obj_mod_inform$ci.lb[1], obj_mod_inform$ci.ub[1])),2) #other
```

    ## [1] 0.11 0.05 0.18

``` r
# N studies
data %>% 
  filter(exposure_type_broad!="neighbourhood") %>%
  count(ref, Informant_MH_binary) %>%
  group_by(Informant_MH_binary) %>% count(Informant_MH_binary)
```

    ## # A tibble: 2 × 2
    ## # Groups:   Informant_MH_binary [2]
    ##   Informant_MH_binary     n
    ##   <chr>               <int>
    ## 1 other                   4
    ## 2 self                   13

``` r
# No. ES
table(data$Informant_MH_binary[data$exposure_type_broad!="neighbourhood"])
```

    ## 
    ## other  self 
    ##    21   118

``` r
###### Cross-sectional vs longitudinal study #######
# Derive variable for cross-sectional v longitudinal study
data$StudyDesign <- NA
data$StudyDesign[data$Q_Longitud==0] <- "cross-sectional"
data$StudyDesign[data$Q_Longitud==1] <- "longitudinal"

## Subjective measure 
# test for overall moderation (QM, QM p value)
subj_mod_longvcross <- rma.mv(z_r_subj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                          data = data, mods = ~ StudyDesign, subset=exposure_type_broad!="neighbourhood")
subj_mod_longvcross
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 139; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0070  0.0836    139     no        es_id 
    ## sigma^2.2  0.0028  0.0528     15     no          ref 
    ## sigma^2.3  0.0000  0.0000     13     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 137) = 543.0789, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 4.1146, p-val = 0.0425
    ## 
    ## Model Results:
    ## 
    ##                          estimate      se     zval    pval    ci.lb    ci.ub​ 
    ## intrcpt                    0.1508  0.0188   8.0134  <.0001   0.1139   0.1877 
    ## StudyDesignlongitudinal   -0.0610  0.0301  -2.0285  0.0425  -0.1200  -0.0021 
    ##  
    ## intrcpt                  *** 
    ## StudyDesignlongitudinal    * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# remove intercept to get effect sizes for each level
subj_mod_longvcross <- rma.mv(z_r_subj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                          data = data, mods = ~ StudyDesign-1, subset=exposure_type_broad!="neighbourhood")
subj_mod_longvcross
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 139; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0070  0.0836    139     no        es_id 
    ## sigma^2.2  0.0028  0.0528     15     no          ref 
    ## sigma^2.3  0.0000  0.0000     13     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 137) = 543.0789, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 64.3885, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##                             estimate      se    zval    pval   ci.lb   ci.ub​ 
    ## StudyDesigncross-sectional    0.1508  0.0188  8.0134  <.0001  0.1139  0.1877 
    ## StudyDesignlongitudinal       0.0898  0.0303  2.9647  0.0030  0.0304  0.1491 
    ##  
    ## StudyDesigncross-sectional  *** 
    ## StudyDesignlongitudinal      ** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(subj_mod_longvcross$beta[1], subj_mod_longvcross$ci.lb[1], subj_mod_longvcross$ci.ub[1])),2) #cross-sectional
```

    ## [1] 0.15 0.11 0.19

``` r
round(fisherz2r(c(subj_mod_longvcross$beta[2], subj_mod_longvcross$ci.lb[2], subj_mod_longvcross$ci.ub[2])),2) #longitudinal
```

    ## [1] 0.09 0.03 0.15

``` r
## Objective measure 
# test for overall moderation (QM, QM p value)
obj_mod_longvcross <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                          data = data, mods = ~ StudyDesign, subset=exposure_type_broad!="neighbourhood")
obj_mod_longvcross
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 139; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0080  0.0894    139     no        es_id 
    ## sigma^2.2  0.0030  0.0549     15     no          ref 
    ## sigma^2.3  0.0000  0.0000     13     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 137) = 641.7718, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0585, p-val = 0.8088
    ## 
    ## Model Results:
    ## 
    ##                          estimate      se     zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt                    0.0425  0.0197   2.1620  0.0306   0.0040  0.0810  * 
    ## StudyDesignlongitudinal   -0.0076  0.0316  -0.2419  0.8088  -0.0695  0.0542    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# remove intercept to get effect sizes for each level
obj_mod_longvcross <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                          data = data, mods = ~ StudyDesign-1, subset=exposure_type_broad!="neighbourhood")
obj_mod_longvcross
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 139; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0080  0.0894    139     no        es_id 
    ## sigma^2.2  0.0030  0.0549     15     no          ref 
    ## sigma^2.3  0.0000  0.0000     13     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 137) = 641.7718, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 4.8644, p-val = 0.0878
    ## 
    ## Model Results:
    ## 
    ##                             estimate      se    zval    pval    ci.lb   ci.ub​ 
    ## StudyDesigncross-sectional    0.0425  0.0197  2.1620  0.0306   0.0040  0.0810 
    ## StudyDesignlongitudinal       0.0349  0.0317  1.0995  0.2716  -0.0273  0.0970 
    ##  
    ## StudyDesigncross-sectional  * 
    ## StudyDesignlongitudinal 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(obj_mod_longvcross$beta[1], obj_mod_longvcross$ci.lb[1], obj_mod_longvcross$ci.ub[1])),2) #cross-sectional
```

    ## [1] 0.04 0.00 0.08

``` r
round(fisherz2r(c(obj_mod_longvcross$beta[2], obj_mod_longvcross$ci.lb[2], obj_mod_longvcross$ci.ub[2])),2) #longitudinal
```

    ## [1]  0.03 -0.03  0.10

``` r
# N studies
data %>% 
  filter(exposure_type_broad!="neighbourhood") %>%
  count(ref, Q_Longitud) %>%
  group_by(Q_Longitud) %>% count(Q_Longitud)
```

    ## # A tibble: 2 × 2
    ## # Groups:   Q_Longitud [2]
    ##   Q_Longitud     n
    ##        <dbl> <int>
    ## 1          0    14
    ## 2          1     4

``` r
# No. ES
table(data$Q_Longitud[data$exposure_type_broad!="neighbourhood"])
```

    ## 
    ##   0   1 
    ## 113  26

``` r
###### Type of psychopathology #######
# Note: exclude psychotic experiences as only reported by one study
## Subjective measures
# test for overall moderation (QM, QM p value)
rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
       mods=~MHoutcome_broad, data = data, subset=exposure_type_broad!="neighbourhood" &
         MHoutcome_broad!="psychotic_experiences")
```

    ## Warning: Redundant predictors dropped from the model.

    ## 
    ## Multivariate Meta-Analysis Model (k = 125; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0061  0.0784    125     no        es_id 
    ## sigma^2.2  0.0051  0.0716     13     no          ref 
    ## sigma^2.3  0.0000  0.0000     12     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 123) = 588.8040, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0431, p-val = 0.8355
    ## 
    ## Model Results:
    ## 
    ##                               estimate      se    zval    pval    ci.lb   ci.ub​ 
    ## intrcpt                         0.1304  0.0316  4.1277  <.0001   0.0685  0.1923 
    ## MHoutcome_broadinternalising    0.0055  0.0265  0.2077  0.8355  -0.0464  0.0575 
    ##  
    ## intrcpt                       *** 
    ## MHoutcome_broadinternalising 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# remove intercept to get effect sizes for each level
subj_mod_psychtype <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                   data = data, mods = ~ MHoutcome_broad -1, subset=exposure_type_broad!="neighbourhood" &
         MHoutcome_broad!="psychotic_experiences")
```

    ## Warning: Redundant predictors dropped from the model.

``` r
subj_mod_psychtype
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 125; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0061  0.0784    125     no        es_id 
    ## sigma^2.2  0.0051  0.0716     13     no          ref 
    ## sigma^2.3  0.0000  0.0000     12     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 123) = 588.8040, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 32.9961, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##                               estimate      se    zval    pval   ci.lb   ci.ub​ 
    ## MHoutcome_broadexternalising    0.1304  0.0316  4.1277  <.0001  0.0685  0.1923 
    ## MHoutcome_broadinternalising    0.1359  0.0241  5.6420  <.0001  0.0887  0.1831 
    ##  
    ## MHoutcome_broadexternalising  *** 
    ## MHoutcome_broadinternalising  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(subj_mod_psychtype$beta[2], subj_mod_psychtype$ci.lb[2], subj_mod_psychtype$ci.ub[2])),2) #internalising
```

    ## [1] 0.14 0.09 0.18

``` r
round(fisherz2r(c(subj_mod_psychtype$beta[1], subj_mod_psychtype$ci.lb[1], subj_mod_psychtype$ci.ub[1])),2) #externalising
```

    ## [1] 0.13 0.07 0.19

``` r
## Objective measures
# test for overall moderation (QM, QM p value)
rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
       mods=~MHoutcome_broad, data = data, subset=exposure_type_broad!="neighbourhood" &
         MHoutcome_broad!="psychotic_experiences")
```

    ## Warning: Redundant predictors dropped from the model.

    ## 
    ## Multivariate Meta-Analysis Model (k = 125; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0079  0.0887    125     no        es_id 
    ## sigma^2.2  0.0032  0.0565     13     no          ref 
    ## sigma^2.3  0.0000  0.0000     12     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 123) = 569.7588, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.2949, p-val = 0.2552
    ## 
    ## Model Results:
    ## 
    ##                               estimate      se     zval    pval    ci.lb​ 
    ## intrcpt                         0.0703  0.0295   2.3847  0.0171   0.0125 
    ## MHoutcome_broadinternalising   -0.0311  0.0273  -1.1379  0.2552  -0.0846 
    ##                                ci.ub 
    ## intrcpt                       0.1281  * 
    ## MHoutcome_broadinternalising  0.0224    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# remove intercept to get effect sizes for each level
obj_mod_psychtype <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
       mods=~MHoutcome_broad-1, data = data, subset=exposure_type_broad!="neighbourhood" &
         MHoutcome_broad!="psychotic_experiences")
```

    ## Warning: Redundant predictors dropped from the model.

``` r
obj_mod_psychtype
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 125; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0079  0.0887    125     no        es_id 
    ## sigma^2.2  0.0032  0.0565     13     no          ref 
    ## sigma^2.3  0.0000  0.0000     12     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 123) = 569.7588, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 6.4410, p-val = 0.0399
    ## 
    ## Model Results:
    ## 
    ##                               estimate      se    zval    pval    ci.lb   ci.ub​ 
    ## MHoutcome_broadexternalising    0.0703  0.0295  2.3847  0.0171   0.0125  0.1281 
    ## MHoutcome_broadinternalising    0.0392  0.0211  1.8611  0.0627  -0.0021  0.0805 
    ##  
    ## MHoutcome_broadexternalising  * 
    ## MHoutcome_broadinternalising  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(obj_mod_psychtype$beta[2], obj_mod_psychtype$ci.lb[2], obj_mod_psychtype$ci.ub[2])),2) #internalising
```

    ## [1] 0.04 0.00 0.08

``` r
round(fisherz2r(c(obj_mod_psychtype$beta[1], obj_mod_psychtype$ci.lb[1], obj_mod_psychtype$ci.ub[1])),2) #externalising
```

    ## [1] 0.07 0.01 0.13

``` r
# N studies
data %>% 
  filter(exposure_type_broad!="neighbourhood") %>%
  count(ref, MHoutcome_broad) %>%
  group_by(MHoutcome_broad) %>% count(MHoutcome_broad)
```

    ## # A tibble: 4 × 2
    ## # Groups:   MHoutcome_broad [4]
    ##   MHoutcome_broad           n
    ##   <chr>                 <int>
    ## 1 externalising             5
    ## 2 internalising            13
    ## 3 psychotic_experiences     1
    ## 4 <NA>                      2

``` r
# No. ES
table(data$MHoutcome_broad[data$exposure_type_broad!="neighbourhood"])
```

    ## 
    ##         externalising         internalising psychotic_experiences 
    ##                    37                    88                     1

``` r
###### Percentage female #######
## Subjective measures
subj_mod_sex <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
       mods=percent_female, data = data, subset=exposure_type_broad!="neighbourhood")
subj_mod_sex
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 139; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0070  0.0835    139     no        es_id 
    ## sigma^2.2  0.0047  0.0688     15     no          ref 
    ## sigma^2.3  0.0000  0.0000     13     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 137) = 726.5765, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.1158, p-val = 0.7336
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt    0.1671  0.0871   1.9191  0.0550  -0.0036  0.3378  . 
    ## mods      -0.0005  0.0015  -0.3403  0.7336  -0.0035  0.0025    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
fisherz2r(c(subj_mod_sex$beta[2], subj_mod_sex$ci.lb[2], subj_mod_sex$ci.ub[2]))
```

    ## [1] -0.000519929 -0.003514537  0.002474688

``` r
## Objective measures
obj_mod_sex <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
       mods=~percent_female, data = data, subset=exposure_type_broad!="neighbourhood")
obj_mod_sex
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 139; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0079  0.0888    139     no        es_id 
    ## sigma^2.2  0.0029  0.0540     15     no          ref 
    ## sigma^2.3  0.0000  0.0000     13     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 137) = 631.8376, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.3439, p-val = 0.2464
    ## 
    ## Model Results:
    ## 
    ##                 estimate      se     zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt           0.1225  0.0726   1.6856  0.0919  -0.0199  0.2648  . 
    ## percent_female   -0.0015  0.0013  -1.1592  0.2464  -0.0039  0.0010    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
fisherz2r(c(obj_mod_sex$beta[2], obj_mod_sex$ci.lb[2], obj_mod_sex$ci.ub[2]))
```

    ## [1] -0.001465915 -0.003944349  0.001012538

``` r
###### Study quality #######
## Subjective measures
subj_mod_quality <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
       mods=Q_total, data = data, subset=exposure_type_broad!="neighbourhood")
subj_mod_quality
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 139; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0070  0.0839    139     no        es_id 
    ## sigma^2.2  0.0044  0.0664     15     no          ref 
    ## sigma^2.3  0.0000  0.0000     13     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 137) = 701.8165, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0000, p-val = 0.9971
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt    0.1388  0.0970   1.4320  0.1521  -0.0512  0.3289    
    ## mods      -0.0001  0.0219  -0.0037  0.9971  -0.0429  0.0428    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
fisherz2r(c(subj_mod_quality$beta[2], subj_mod_quality$ci.lb[2], subj_mod_quality$ci.ub[2]))
```

    ## [1] -7.998037e-05 -4.291935e-02  4.275968e-02

``` r
## Objective measures
obj_mod_quality <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
       mods=Q_total, data = data, subset=exposure_type_broad!="neighbourhood")
obj_mod_quality
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 139; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0080  0.0893    139     no        es_id 
    ## sigma^2.2  0.0016  0.0394     15     no          ref 
    ## sigma^2.3  0.0014  0.0375     13     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 137) = 635.0961, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.7355, p-val = 0.3911
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt    0.1205  0.0937   1.2871  0.1981  -0.0630  0.3041    
    ## mods      -0.0180  0.0210  -0.8576  0.3911  -0.0592  0.0232    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
fisherz2r(c(obj_mod_quality$beta[2], obj_mod_quality$ci.lb[2], obj_mod_quality$ci.ub[2]))
```

    ## [1] -0.01802325 -0.05915064  0.02316523

``` r
# Childhood maltreatment: Test for moderation by informant for adversity type, MH outcome, Cross sectional v longitudinal, Study quality, Sex
# Bullying victimisation: Test for moderation by adversity type, MH outcome, Informant, Cross sectional v longitudinal, Study quality, Sex
```

###### 2.7.2 Test for moderation by informant of mental health outcome

``` r
# Subjective measure of maltreatment
subj_mod_inform <- rma.mv(z_r_subj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                          data = data, mods = ~ Informant_MH_binary, subset=exposure_type_broad =="maltreatment",
                          slab=paste(ref, es_id, sep=", "))
subj_mod_inform
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0091  0.0952     94     no        es_id 
    ## sigma^2.2  0.0051  0.0715      6     no          ref 
    ## sigma^2.3  0.0000  0.0000      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 92) = 466.5892, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.9341, p-val = 0.1643
    ## 
    ## Model Results:
    ## 
    ##                          estimate      se     zval    pval    ci.lb   ci.ub    ​ 
    ## intrcpt                    0.2658  0.0849   3.1306  0.0017   0.0994  0.4321  ** 
    ## Informant_MH_binaryself   -0.1276  0.0918  -1.3907  0.1643  -0.3075  0.0522     
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
subj_mod_inform <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                   data = data, mods = ~ Informant_MH_binary -1, subset=exposure_type_broad =="maltreatment",
                   slab=paste(ref, es_id, sep=", "))
subj_mod_inform
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0091  0.0955     94     no        es_id 
    ## sigma^2.2  0.0052  0.0724      6     no          ref 
    ## sigma^2.3  0.0000  0.0000      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 92) = 531.4265, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 24.6447, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##                           estimate      se    zval    pval   ci.lb   ci.ub     ​ 
    ## Informant_MH_binaryother    0.2658  0.0857  3.0999  0.0019  0.0977  0.4338   ** 
    ## Informant_MH_binaryself     0.1371  0.0354  3.8776  0.0001  0.0678  0.2064  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(subj_mod_inform$beta[1], subj_mod_inform$ci.lb[1], subj_mod_inform$ci.ub[1])),2) #other
```

    ## [1] 0.26 0.10 0.41

``` r
round(fisherz2r(c(subj_mod_inform$beta[2], subj_mod_inform$ci.lb[2], subj_mod_inform$ci.ub[2])),2) #self-report
```

    ## [1] 0.14 0.07 0.20

``` r
# Objective measure of maltreatment
# report estimate QM, QM p value
obj_mod_inform <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                  data = data, mods = ~ Informant_MH_binary, subset=exposure_type_broad =="maltreatment",
                  slab=paste(ref, es_id, sep=", "))
obj_mod_inform
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0100  0.1000     94     no        es_id 
    ## sigma^2.2  0.0000  0.0000      6     no          ref 
    ## sigma^2.3  0.0012  0.0340      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 92) = 338.5582, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 9.6166, p-val = 0.0019
    ## 
    ## Model Results:
    ## 
    ##                          estimate      se     zval    pval    ci.lb    ci.ub​ 
    ## intrcpt                    0.2158  0.0584   3.6937  0.0002   0.1013   0.3303 
    ## Informant_MH_binaryself   -0.1936  0.0624  -3.1011  0.0019  -0.3160  -0.0713 
    ##  
    ## intrcpt                  *** 
    ## Informant_MH_binaryself   ** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
obj_mod_inform <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                         data = data, mods = ~ Informant_MH_binary -1, subset=exposure_type_broad =="maltreatment",
                         slab=paste(ref, es_id, sep=", "))
obj_mod_inform
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0100  0.1000     94     no        es_id 
    ## sigma^2.2  0.0000  0.0000      6     no          ref 
    ## sigma^2.3  0.0012  0.0340      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 92) = 338.5582, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 14.6533, p-val = 0.0007
    ## 
    ## Model Results:
    ## 
    ##                           estimate      se    zval    pval    ci.lb   ci.ub​ 
    ## Informant_MH_binaryother    0.2158  0.0584  3.6937  0.0002   0.1013  0.3303 
    ## Informant_MH_binaryself     0.0222  0.0220  1.0050  0.3149  -0.0211  0.0654 
    ##  
    ## Informant_MH_binaryother  *** 
    ## Informant_MH_binaryself 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(obj_mod_inform$beta[1], obj_mod_inform$ci.lb[1], obj_mod_inform$ci.ub[1])),2) #other
```

    ## [1] 0.21 0.10 0.32

``` r
round(fisherz2r(c(obj_mod_inform$beta[2], obj_mod_inform$ci.lb[2], obj_mod_inform$ci.ub[2])),2) #self-report
```

    ## [1]  0.02 -0.02  0.07

``` r
# N studies
data %>% 
  filter(exposure_type_broad=="maltreatment") %>%
  count(ref, Informant_MH_binary) %>%
  group_by(Informant_MH_binary) %>% count(Informant_MH_binary)
```

    ## # A tibble: 2 × 2
    ## # Groups:   Informant_MH_binary [2]
    ##   Informant_MH_binary     n
    ##   <chr>               <int>
    ## 1 other                   1
    ## 2 self                    5

``` r
# No. ES
table(data$Informant_MH_binary[data$exposure_type_broad=="maltreatment"])
```

    ## 
    ## other  self 
    ##     6    88

``` r
# Subjective measure of bullying
#  report estimate QM, QM p value
subj_mod_inform <- rma.mv(z_r_subj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                          data = data, mods = ~ Informant_MH_binary, subset=exposure_type_broad =="bullying", 
                          slab=paste(ref, es_id, sep=", "))
subj_mod_inform
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0016  0.0394     45     no        es_id 
    ## sigma^2.2  0.0014  0.0369      9     no          ref 
    ## sigma^2.3  0.0018  0.0427      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 110.7577, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 20.3741, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##                          estimate      se    zval    pval    ci.lb   ci.ub     ​ 
    ## intrcpt                    0.0262  0.0313  0.8354  0.4035  -0.0352  0.0876      
    ## Informant_MH_binaryself    0.1274  0.0282  4.5138  <.0001   0.0721  0.1827  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
subj_mod_inform <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                   data = data, mods = ~ Informant_MH_binary -1, subset=exposure_type_broad =="bullying",
                   slab=paste(ref, es_id, sep=", "))
subj_mod_inform
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0016  0.0394     45     no        es_id 
    ## sigma^2.2  0.0014  0.0368      9     no          ref 
    ## sigma^2.3  0.0018  0.0427      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 110.7578, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 46.9820, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##                           estimate      se    zval    pval    ci.lb   ci.ub​ 
    ## Informant_MH_binaryother    0.0262  0.0313  0.8371  0.4025  -0.0351  0.0875 
    ## Informant_MH_binaryself     0.1535  0.0245  6.2768  <.0001   0.1056  0.2015 
    ##  
    ## Informant_MH_binaryother 
    ## Informant_MH_binaryself   *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(subj_mod_inform$beta[1], subj_mod_inform$ci.lb[1], subj_mod_inform$ci.ub[1])),2) #other
```

    ## [1]  0.03 -0.04  0.09

``` r
round(fisherz2r(c(subj_mod_inform$beta[2], subj_mod_inform$ci.lb[2], subj_mod_inform$ci.ub[2])),2) #self
```

    ## [1] 0.15 0.11 0.20

``` r
# Objective measure of bullying
# report estimate QM, QM p value
obj_mod_inform <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                  data = data, mods = ~ Informant_MH_binary, subset=exposure_type_broad =="bullying",
                  slab=paste(ref, es_id, sep=", "))
obj_mod_inform
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0046  0.0681     45     no        es_id 
    ## sigma^2.2  0.0028  0.0529      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 229.0290, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 3.2959, p-val = 0.0695
    ## 
    ## Model Results:
    ## 
    ##                          estimate      se     zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt                    0.0823  0.0360   2.2857  0.0223   0.0117  0.1530  * 
    ## Informant_MH_binaryself   -0.0664  0.0366  -1.8155  0.0695  -0.1382  0.0053  . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
obj_mod_inform <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                         data = data, mods = ~ Informant_MH_binary -1, subset=exposure_type_broad =="bullying",
                         slab=paste(ref, es_id, sep=", "))
obj_mod_inform
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0046  0.0681     45     no        es_id 
    ## sigma^2.2  0.0028  0.0529      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 229.0290, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 5.2594, p-val = 0.0721
    ## 
    ## Model Results:
    ## 
    ##                           estimate      se    zval    pval    ci.lb   ci.ub   ​ 
    ## Informant_MH_binaryother    0.0823  0.0360  2.2857  0.0223   0.0117  0.1530  * 
    ## Informant_MH_binaryself     0.0159  0.0262  0.6065  0.5442  -0.0355  0.0673    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(obj_mod_inform$beta[1], obj_mod_inform$ci.lb[1], obj_mod_inform$ci.ub[1])),2) #other
```

    ## [1] 0.08 0.01 0.15

``` r
round(fisherz2r(c(obj_mod_inform$beta[2], obj_mod_inform$ci.lb[2], obj_mod_inform$ci.ub[2])),2) #self-report
```

    ## [1]  0.02 -0.04  0.07

``` r
# N studies
data %>% 
  filter(exposure_type_broad=="bullying") %>%
  count(ref, Informant_MH_binary) %>%
  group_by(Informant_MH_binary) %>% count(Informant_MH_binary)
```

    ## # A tibble: 2 × 2
    ## # Groups:   Informant_MH_binary [2]
    ##   Informant_MH_binary     n
    ##   <chr>               <int>
    ## 1 other                   3
    ## 2 self                    8

``` r
# No. ES
table(data$Informant_MH_binary[data$exposure_type_broad=="bullying"])
```

    ## 
    ## other  self 
    ##    15    30

###### 2.7.3 Test for moderation by study type

###### Cross-sectional vs longitudinal study

``` r
# Intercept is the lowest level (cross sectional coded as 0)
# Mods is longitudinal studies

# Subjective measure of maltreatment
# Obtain the QM and QM p value for subjective maltreatment
subj_mal <-rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                        data = data, mods = ~ StudyDesign, subset = exposure_type_broad =="maltreatment"& !is.na(Q_Longitud), slab=paste(ref, es_id, sep=", "))
subj_mal
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0092  0.0958     94     no        es_id 
    ## sigma^2.2  0.0011  0.0335      6     no          ref 
    ## sigma^2.3  0.0011  0.0335      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 92) = 349.5249, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 7.1479, p-val = 0.0075
    ## 
    ## Model Results:
    ## 
    ##                          estimate      se     zval    pval    ci.lb    ci.ub​ 
    ## intrcpt                    0.1835  0.0262   6.9997  <.0001   0.1321   0.2349 
    ## StudyDesignlongitudinal   -0.1491  0.0558  -2.6736  0.0075  -0.2584  -0.0398 
    ##  
    ## intrcpt                  *** 
    ## StudyDesignlongitudinal   ** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# remove the intercept to get effect sizes in each group
subj_mal <-rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                        data = data, mods = ~ StudyDesign -1, subset = exposure_type_broad =="maltreatment"& !is.na(Q_Longitud), slab=paste(ref, es_id, sep=", "))
subj_mal
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0092  0.0958     94     no        es_id 
    ## sigma^2.2  0.0011  0.0335      6     no          ref 
    ## sigma^2.3  0.0011  0.0335      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 92) = 349.5249, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 49.1825, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##                             estimate      se    zval    pval    ci.lb   ci.ub​ 
    ## StudyDesigncross-sectional    0.1835  0.0262  6.9997  <.0001   0.1321  0.2349 
    ## StudyDesignlongitudinal       0.0344  0.0534  0.6441  0.5195  -0.0703  0.1390 
    ##  
    ## StudyDesigncross-sectional  *** 
    ## StudyDesignlongitudinal 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(subj_mal$beta[1], subj_mal$ci.lb[1], subj_mal$ci.ub[1])),2) # Cross-sectional
```

    ## [1] 0.18 0.13 0.23

``` r
round(fisherz2r(c(subj_mal$beta[2], subj_mal$ci.lb[2], subj_mal$ci.ub[2])),2)# Longitudinal
```

    ## [1]  0.03 -0.07  0.14

``` r
# Objective measure of maltreatment
# Obtain the QM and QM p value 
obj_mal <-rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                       data = data, mods = ~ StudyDesign, subset = exposure_type_broad =="maltreatment"& !is.na(Q_Longitud), slab=paste(ref, es_id, sep=", "))
obj_mal
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0101  0.1005     94     no        es_id 
    ## sigma^2.2  0.0034  0.0583      6     no          ref 
    ## sigma^2.3  0.0034  0.0583      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 92) = 404.2888, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0048, p-val = 0.9449
    ## 
    ## Model Results:
    ## 
    ##                          estimate      se    zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt                    0.0529  0.0402  1.3165  0.1880  -0.0259  0.1317    
    ## StudyDesignlongitudinal    0.0058  0.0835  0.0692  0.9449  -0.1580  0.1695    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# remove the intercept to get effect sizes in each group
obj_mal <-rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                        data = data, mods = StudyDesign -1, subset = exposure_type_broad =="maltreatment"& !is.na(Q_Longitud), slab=paste(ref, es_id, sep=", "))
```

    ## Error: Cannot find the object/variable ('StudyDesign - 1') specified for the 'mods' argument.

``` r
obj_mal
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0101  0.1005     94     no        es_id 
    ## sigma^2.2  0.0034  0.0583      6     no          ref 
    ## sigma^2.3  0.0034  0.0583      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 92) = 404.2888, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0048, p-val = 0.9449
    ## 
    ## Model Results:
    ## 
    ##                          estimate      se    zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt                    0.0529  0.0402  1.3165  0.1880  -0.0259  0.1317    
    ## StudyDesignlongitudinal    0.0058  0.0835  0.0692  0.9449  -0.1580  0.1695    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(obj_mal$beta[1], obj_mal$ci.lb[1], obj_mal$ci.ub[1])),2) # Cross-sectional
```

    ## [1]  0.05 -0.03  0.13

``` r
round(fisherz2r(c(obj_mal$beta[2], obj_mal$ci.lb[2], obj_mal$ci.ub[2])),2) # Longitudinal
```

    ## [1]  0.01 -0.16  0.17

``` r
# N studies
data %>% 
  filter(exposure_type_broad=="maltreatment") %>%
  count(ref, Q_Longitud) %>%
  group_by(Q_Longitud) %>% count(Q_Longitud)
```

    ## # A tibble: 2 × 2
    ## # Groups:   Q_Longitud [2]
    ##   Q_Longitud     n
    ##        <dbl> <int>
    ## 1          0     5
    ## 2          1     1

``` r
# No. ES
table(data$Q_Longitud[data$exposure_type_broad=="maltreatment"])
```

    ## 
    ##  0  1 
    ## 82 12

``` r
## Subjective measure of bullying
# Obtain the QM and QM p value
subj_bull <-rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                data = data, mods = ~StudyDesign, subset = exposure_type_broad =="bullying"& !is.na(Q_Longitud), slab=paste(ref, es_id, sep=", "))
subj_bull
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0036  0.0597     45     no        es_id 
    ## sigma^2.2  0.0028  0.0533      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 152.9364, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.5770, p-val = 0.2092
    ## 
    ## Model Results:
    ## 
    ##                          estimate      se     zval    pval    ci.lb   ci.ub​ 
    ## intrcpt                    0.1287  0.0241   5.3399  <.0001   0.0815  0.1759 
    ## StudyDesignlongitudinal   -0.0345  0.0275  -1.2558  0.2092  -0.0884  0.0194 
    ##  
    ## intrcpt                  *** 
    ## StudyDesignlongitudinal 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# remove the intercept to get effect sizes in each group
subj_bull <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                        data = data, mods = ~ StudyDesign -1, subset = exposure_type_broad =="bullying"& !is.na(Q_Longitud), slab=paste(ref, es_id, sep=", "))
subj_bull
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0036  0.0597     45     no        es_id 
    ## sigma^2.2  0.0028  0.0533      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 152.9364, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 28.5330, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##                             estimate      se    zval    pval   ci.lb   ci.ub​ 
    ## StudyDesigncross-sectional    0.1287  0.0241  5.3399  <.0001  0.0815  0.1759 
    ## StudyDesignlongitudinal       0.0942  0.0325  2.8950  0.0038  0.0304  0.1579 
    ##  
    ## StudyDesigncross-sectional  *** 
    ## StudyDesignlongitudinal      ** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(subj_bull$beta[1], subj_bull$ci.lb[1], subj_bull$ci.ub[1])),2) # Cross-sectional
```

    ## [1] 0.13 0.08 0.17

``` r
round(fisherz2r(c(subj_bull$beta[2], subj_bull$ci.lb[2], subj_bull$ci.ub[2])),2)# Longitudinal
```

    ## [1] 0.09 0.03 0.16

``` r
## Objective measure of bullying
# Obtain the QM and QM p value
obj_bull <-rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
       data = data, mods = ~ StudyDesign, subset = exposure_type_broad =="bullying"& !is.na(Q_Longitud), slab=paste(ref, es_id, sep=", "))
obj_bull
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0054  0.0738     45     no        es_id 
    ## sigma^2.2  0.0018  0.0421      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 230.1757, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0579, p-val = 0.8099
    ## 
    ## Model Results:
    ## 
    ##                          estimate      se     zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt                    0.0347  0.0228   1.5208  0.1283  -0.0100  0.0794    
    ## StudyDesignlongitudinal   -0.0074  0.0309  -0.2406  0.8099  -0.0680  0.0531    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# remove the intercept to get effect sizes in each group
obj_bull <-rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
       data = data, mods = ~ StudyDesign-1, subset = exposure_type_broad =="bullying"& !is.na(Q_Longitud), slab=paste(ref, es_id, sep=", "))
obj_bull
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0054  0.0738     45     no        es_id 
    ## sigma^2.2  0.0018  0.0421      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 230.1757, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 2.3546, p-val = 0.3081
    ## 
    ## Model Results:
    ## 
    ##                             estimate      se    zval    pval    ci.lb   ci.ub​ 
    ## StudyDesigncross-sectional    0.0347  0.0228  1.5208  0.1283  -0.0100  0.0794 
    ## StudyDesignlongitudinal       0.0272  0.0327  0.8326  0.4051  -0.0369  0.0914 
    ##  
    ## StudyDesigncross-sectional 
    ## StudyDesignlongitudinal 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(obj_bull$beta[1], obj_bull$ci.lb[1], obj_bull$ci.ub[1])),2) # Cross-sectional
```

    ## [1]  0.03 -0.01  0.08

``` r
round(fisherz2r(c(obj_bull$beta[2], obj_bull$ci.lb[2], obj_bull$ci.ub[2])),2)# Longitudinal
```

    ## [1]  0.03 -0.04  0.09

``` r
# N studies
data %>% 
  filter(exposure_type_broad=="bullying") %>%
  count(ref, Q_Longitud) %>%
  group_by(Q_Longitud) %>% count(Q_Longitud)
```

    ## # A tibble: 2 × 2
    ## # Groups:   Q_Longitud [2]
    ##   Q_Longitud     n
    ##        <dbl> <int>
    ## 1          0     9
    ## 2          1     3

``` r
# No. ES
table(data$Q_Longitud[data$exposure_type_broad=="bullying"])
```

    ## 
    ##  0  1 
    ## 31 14

###### 2.7.4 Test for moderation by mental health outcome

``` r
## Subjective measure of maltreatment
mal_MH_subj<- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                    data = data, mods= ~ MHoutcome_broad, subset = exposure_type_broad =="maltreatment" & !is.na(MHoutcome_broad), slab=paste(ref, es_id, sep=", "))
```

    ## Warning: Redundant predictors dropped from the model.

``` r
mal_MH_subj
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 81; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0081  0.0902     81     no        es_id 
    ## sigma^2.2  0.0041  0.0644      5     no          ref 
    ## sigma^2.3  0.0041  0.0644      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 79) = 423.9411, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0890, p-val = 0.7655
    ## 
    ## Model Results:
    ## 
    ##                               estimate      se    zval    pval    ci.lb   ci.ub​ 
    ## intrcpt                         0.1481  0.0471  3.1454  0.0017   0.0558  0.2403 
    ## MHoutcome_broadinternalising    0.0092  0.0308  0.2983  0.7655  -0.0511  0.0695 
    ##  
    ## intrcpt                       ** 
    ## MHoutcome_broadinternalising 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Subjective measures,obtain effect size and ci 
mal_MH_subj<- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                     data = data, mods= ~ MHoutcome_broad -1, subset = exposure_type_broad =="maltreatment" & !is.na(MHoutcome_broad), slab=paste(ref, es_id, sep=", "))
```

    ## Warning: Redundant predictors dropped from the model.

``` r
mal_MH_subj
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 81; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0081  0.0902     81     no        es_id 
    ## sigma^2.2  0.0041  0.0644      5     no          ref 
    ## sigma^2.3  0.0041  0.0644      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 79) = 423.9411, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 12.6246, p-val = 0.0018
    ## 
    ## Model Results:
    ## 
    ##                               estimate      se    zval    pval   ci.lb   ci.ub​ 
    ## MHoutcome_broadexternalising    0.1481  0.0471  3.1454  0.0017  0.0558  0.2403 
    ## MHoutcome_broadinternalising    0.1573  0.0451  3.4852  0.0005  0.0688  0.2457 
    ##  
    ## MHoutcome_broadexternalising   ** 
    ## MHoutcome_broadinternalising  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(mal_MH_subj$beta[1], mal_MH_subj$ci.lb[1], mal_MH_subj$ci.ub[1])),2) #externalising
```

    ## [1] 0.15 0.06 0.24

``` r
round(fisherz2r(c(mal_MH_subj$beta[2], mal_MH_subj$ci.lb[2], mal_MH_subj$ci.ub[2])),2) #internalising
```

    ## [1] 0.16 0.07 0.24

``` r
# Objective measure of maltreatment
mal_MH_obj <-rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                    data = data, mods= ~ MHoutcome_broad, subset=exposure_type_broad =="maltreatment" & !is.na(MHoutcome_broad), slab=paste(ref, es_id, sep=", "))
```

    ## Warning: Redundant predictors dropped from the model.

``` r
mal_MH_obj
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 81; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0110  0.1049     81     no        es_id 
    ## sigma^2.2  0.0030  0.0551      5     no          ref 
    ## sigma^2.3  0.0030  0.0551      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 79) = 357.3983, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0931, p-val = 0.7602
    ## 
    ## Model Results:
    ## 
    ##                               estimate      se    zval    pval    ci.lb   ci.ub​ 
    ## intrcpt                         0.0530  0.0430  1.2331  0.2175  -0.0312  0.1372 
    ## MHoutcome_broadinternalising    0.0100  0.0327  0.3052  0.7602  -0.0542  0.0741 
    ##  
    ## intrcpt 
    ## MHoutcome_broadinternalising 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
mal_MH_obj <-rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                     data = data, mods= ~ MHoutcome_broad -1, subset=exposure_type_broad =="maltreatment" & !is.na(MHoutcome_broad), slab=paste(ref, es_id, sep=", "))
```

    ## Warning: Redundant predictors dropped from the model.

``` r
mal_MH_obj
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 81; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0110  0.1049     81     no        es_id 
    ## sigma^2.2  0.0030  0.0551      5     no          ref 
    ## sigma^2.3  0.0030  0.0551      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 79) = 357.3983, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:2):
    ## QM(df = 2) = 2.4578, p-val = 0.2926
    ## 
    ## Model Results:
    ## 
    ##                               estimate      se    zval    pval    ci.lb   ci.ub​ 
    ## MHoutcome_broadexternalising    0.0530  0.0430  1.2331  0.2175  -0.0312  0.1372 
    ## MHoutcome_broadinternalising    0.0630  0.0406  1.5529  0.1205  -0.0165  0.1424 
    ##  
    ## MHoutcome_broadexternalising 
    ## MHoutcome_broadinternalising 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(mal_MH_obj$beta[1], mal_MH_obj$ci.lb[1], mal_MH_obj$ci.ub[1])),2) #externalising
```

    ## [1]  0.05 -0.03  0.14

``` r
round(fisherz2r(c(mal_MH_obj$beta[2], mal_MH_obj$ci.lb[2], mal_MH_obj$ci.ub[2])),2) #internalising
```

    ## [1]  0.06 -0.02  0.14

``` r
#N Studies
data %>% 
  filter(exposure_type_broad=="maltreatment") %>%
  count(ref, MHoutcome_broad) %>%
  group_by(MHoutcome_broad) %>% count(MHoutcome_broad)
```

    ## # A tibble: 3 × 2
    ## # Groups:   MHoutcome_broad [3]
    ##   MHoutcome_broad     n
    ##   <chr>           <int>
    ## 1 externalising       4
    ## 2 internalising       5
    ## 3 <NA>                2

``` r
# No. ES
table(data$MHoutcome_broad[data$exposure_type_broad=="maltreatment"])
```

    ## 
    ## externalising internalising 
    ##            36            45

``` r
## Subjective measure of bullying
bull_MH_subj<- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                    data = data, mods= ~ MHoutcome_broad, subset = exposure_type_broad =="bullying" & !is.na(MHoutcome_broad), slab=paste(ref, es_id, sep=", "))
bull_MH_subj
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0039  0.0625     45     no        es_id 
    ## sigma^2.2  0.0029  0.0542      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 42) = 155.5920, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 2:3):
    ## QM(df = 2) = 0.3511, p-val = 0.8390
    ## 
    ## Model Results:
    ## 
    ##                                       estimate      se    zval    pval    ci.lb​ 
    ## intrcpt                                 0.0844  0.0807  1.0457  0.2957  -0.0738 
    ## MHoutcome_broadinternalising            0.0366  0.0797  0.4593  0.6461  -0.1197 
    ## MHoutcome_broadpsychotic_experiences    0.0708  0.1220  0.5800  0.5619  -0.1684 
    ##                                        ci.ub 
    ## intrcpt                               0.2427    
    ## MHoutcome_broadinternalising          0.1929    
    ## MHoutcome_broadpsychotic_experiences  0.3100    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Subjective measures,obtain effect size and ci 
bull_MH_subj<- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                     data = data, mods= ~ MHoutcome_broad -1, subset = exposure_type_broad =="bullying" & !is.na(MHoutcome_broad), slab=paste(ref, es_id, sep=", "))
bull_MH_subj
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0039  0.0625     45     no        es_id 
    ## sigma^2.2  0.0029  0.0542      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 42) = 155.5920, p-val < .0001
    ## 
    ## Test of Moderators (coefficients 1:3):
    ## QM(df = 3) = 26.1245, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##                                       estimate      se    zval    pval    ci.lb​ 
    ## MHoutcome_broadexternalising            0.0844  0.0807  1.0457  0.2957  -0.0738 
    ## MHoutcome_broadinternalising            0.1211  0.0251  4.8204  <.0001   0.0718 
    ## MHoutcome_broadpsychotic_experiences    0.1552  0.0915  1.6962  0.0899  -0.0241 
    ##                                        ci.ub 
    ## MHoutcome_broadexternalising          0.2427      
    ## MHoutcome_broadinternalising          0.1703  *** 
    ## MHoutcome_broadpsychotic_experiences  0.3346    . 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(bull_MH_subj$beta[1], bull_MH_subj$ci.lb[1], bull_MH_subj$ci.ub[1])),2) #externalising
```

    ## [1]  0.08 -0.07  0.24

``` r
round(fisherz2r(c(bull_MH_subj$beta[2], bull_MH_subj$ci.lb[2], bull_MH_subj$ci.ub[2])),2) #internalising
```

    ## [1] 0.12 0.07 0.17

``` r
# Objective measure of bulying
bull_MH_obj <-rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                    data = data, mods= ~ MHoutcome_broad, subset=exposure_type_broad =="bullying" & !is.na(MHoutcome_broad), slab=paste(ref, es_id, sep=", "))
bull_MH_obj
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0000  0.0000     45     no        es_id 
    ## sigma^2.2  0.0000  0.0000      9     no          ref 
    ## sigma^2.3  0.0024  0.0493      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 42) = 75.6150, p-val = 0.0011
    ## 
    ## Test of Moderators (coefficients 2:3):
    ## QM(df = 2) = 145.2910, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##                                       estimate      se      zval    pval​ 
    ## intrcpt                                 0.5063  0.0439   11.5412  <.0001 
    ## MHoutcome_broadinternalising           -0.4819  0.0400  -12.0422  <.0001 
    ## MHoutcome_broadpsychotic_experiences   -0.5003  0.0775   -6.4529  <.0001 
    ##                                         ci.lb    ci.ub 
    ## intrcpt                                0.4203   0.5923  *** 
    ## MHoutcome_broadinternalising          -0.5603  -0.4035  *** 
    ## MHoutcome_broadpsychotic_experiences  -0.6522  -0.3483  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
bull_MH_obj <-rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                     data = data, mods= ~ MHoutcome_broad -1, subset=exposure_type_broad =="bullying" & !is.na(MHoutcome_broad), slab=paste(ref, es_id, sep=", "))
bull_MH_obj
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0000  0.0000     45     no        es_id 
    ## sigma^2.2  0.0000  0.0000      9     no          ref 
    ## sigma^2.3  0.0024  0.0493      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 42) = 75.6150, p-val = 0.0011
    ## 
    ## Test of Moderators (coefficients 1:3):
    ## QM(df = 3) = 148.9790, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##                                       estimate      se     zval    pval​ 
    ## MHoutcome_broadexternalising            0.5063  0.0439  11.5412  <.0001 
    ## MHoutcome_broadinternalising            0.0244  0.0209   1.1699  0.2420 
    ## MHoutcome_broadpsychotic_experiences    0.0060  0.0639   0.0946  0.9246 
    ##                                         ci.lb   ci.ub 
    ## MHoutcome_broadexternalising           0.4203  0.5923  *** 
    ## MHoutcome_broadinternalising          -0.0165  0.0654      
    ## MHoutcome_broadpsychotic_experiences  -0.1192  0.1313      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(bull_MH_obj$beta[1], bull_MH_obj$ci.lb[1], bull_MH_obj$ci.ub[1])),2) #externalising
```

    ## [1] 0.47 0.40 0.53

``` r
round(fisherz2r(c(bull_MH_obj$beta[2], bull_MH_obj$ci.lb[2], bull_MH_obj$ci.ub[2])),2) #internalising
```

    ## [1]  0.02 -0.02  0.07

``` r
#N Studies
data %>% 
  filter(exposure_type_broad=="bullying") %>%
  count(ref, MHoutcome_broad) %>%
  group_by(MHoutcome_broad) %>% count(MHoutcome_broad)
```

    ## # A tibble: 3 × 2
    ## # Groups:   MHoutcome_broad [3]
    ##   MHoutcome_broad           n
    ##   <chr>                 <int>
    ## 1 externalising             1
    ## 2 internalising             8
    ## 3 psychotic_experiences     1

``` r
# No. ES
table(data$MHoutcome_broad[data$exposure_type_broad=="bullying"])
```

    ## 
    ##         externalising         internalising psychotic_experiences 
    ##                     1                    43                     1

##### 2.7.5 Test for moderation by study quality

``` r
# Intercept is the lowest level of study quality

# Subjective measure of maltreatment
subj_mal_qual <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                        data = data, mods = Q_total, subset = exposure_type_broad =="maltreatment"& !is.na(Q_Longitud), slab=paste(ref, es_id, sep=", "))
subj_mal_qual
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0092  0.0961     94     no        es_id 
    ## sigma^2.2  0.0066  0.0813      6     no          ref 
    ## sigma^2.3  0.0000  0.0000      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 92) = 493.9882, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.2800, p-val = 0.5967
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt    0.0573  0.1898  0.3019  0.7627  -0.3148  0.4294    
    ## mods       0.0247  0.0467  0.5291  0.5967  -0.0668  0.1163    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r
fisherz2r(c(subj_mal_qual$beta[2], subj_mal_qual$ci.lb[2], subj_mal_qual$ci.ub[2])) #study quality moderator
```

    ## [1]  0.02471500 -0.06675044  0.11576852

``` r
# Objective measure of maltreatment
obj_mal_qual <-rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                      data = data, mods = Q_total, subset = exposure_type_broad =="maltreatment"& !is.na(Q_Longitud), slab=paste(ref, es_id, sep=", "))
obj_mal_qual
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0101  0.1003     94     no        es_id 
    ## sigma^2.2  0.0021  0.0461      6     no          ref 
    ## sigma^2.3  0.0039  0.0624      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 92) = 377.6176, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.6202, p-val = 0.4310
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt    0.1958  0.1838   1.0651  0.2868  -0.1645  0.5561    
    ## mods      -0.0349  0.0444  -0.7875  0.4310  -0.1219  0.0520    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r
fisherz2r(c(obj_mal_qual$beta[2], obj_mal_qual$ci.lb[2], obj_mal_qual$ci.ub[2])) #study quality intercept
```

    ## [1] -0.03492802 -0.12130714  0.05197612

``` r
# Subjective measure of bullying
subj_bull_qual <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                    data = data, mods = Q_total, subset = exposure_type_broad =="bullying"& !is.na(Q_Longitud), slab=paste(ref, es_id, sep=", "))
subj_bull_qual
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0037  0.0609     45     no        es_id 
    ## sigma^2.2  0.0031  0.0556      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 152.8317, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0113, p-val = 0.9154
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt    0.1338  0.1109   1.2061  0.2278  -0.0836  0.3513    
    ## mods      -0.0025  0.0233  -0.1063  0.9154  -0.0481  0.0431    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
fisherz2r(c(subj_bull_qual$beta[2], subj_bull_qual$ci.lb[2], subj_bull_qual$ci.ub[2])) 
```

    ## [1] -0.002473587 -0.048052249  0.043115354

``` r
# Objective measure of bullying
obj_bull_qual <-rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                       data = data, mods = Q_total, subset = exposure_type_broad =="bullying"& !is.na(Q_Longitud), slab=paste(ref, es_id, sep=", "))
obj_bull_qual
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0054  0.0734     45     no        es_id 
    ## sigma^2.2  0.0020  0.0447      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 231.9642, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0286, p-val = 0.8658
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt    0.0525  0.1155   0.4547  0.6493  -0.1738  0.2788    
    ## mods      -0.0041  0.0242  -0.1691  0.8658  -0.0515  0.0434    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
fisherz2r(c(obj_bull_qual$beta[2], obj_bull_qual$ci.lb[2], obj_bull_qual$ci.ub[2])) 
```

    ## [1] -0.004092318 -0.051493240  0.043327000

``` r
# Variation between quality of studies
describe(data$Q_total)
```

    ##    vars   n mean   sd median trimmed mad min max range skew kurtosis   se
    ## X1    1 141 4.31 0.69      4    4.27   0   3   6     3 0.69     0.43 0.06

##### 2.7.6 Test for moderation by sex

``` r
# Subjective measure of maltreatment
subj_mal_sex <- rma.mv(z_r_subj, z_var_subj,random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                      data = data, mods = percent_female, subset = exposure_type_broad =="maltreatment"& !is.na(percent_female), slab=paste(ref, es_id, sep=", "))
subj_mal_sex
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0091  0.0954     94     no        es_id 
    ## sigma^2.2  0.0000  0.0043      6     no          ref 
    ## sigma^2.3  0.0000  0.0000      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 92) = 331.5951, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 30.3404, p-val < .0001
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb    ci.ub     ​ 
    ## intrcpt    1.3122  0.2098   6.2541  <.0001   0.9010   1.7234  *** 
    ## mods      -0.0227  0.0041  -5.5082  <.0001  -0.0308  -0.0146  *** 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(subj_mal_sex$beta[2], subj_mal_sex$ci.lb[2], subj_mal_sex$ci.ub[2])),2)
```

    ## [1] -0.02 -0.03 -0.01

``` r
# Objective measure of maltreatment
obj_mal_sex <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                       data = data, mods = percent_female, subset = exposure_type_broad =="maltreatment"& !is.na(percent_female), slab=paste(ref, es_id, sep=", "))
obj_mal_sex
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0101  0.1003     94     no        es_id 
    ## sigma^2.2  0.0050  0.0705      6     no          ref 
    ## sigma^2.3  0.0000  0.0000      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 92) = 404.4226, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.3377, p-val = 0.2474
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt    0.5740  0.4545   1.2629  0.2066  -0.3168  1.4647    
    ## mods      -0.0103  0.0089  -1.1566  0.2474  -0.0277  0.0071    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
round(fisherz2r(c(obj_mal_sex$beta[2], obj_mal_sex$ci.lb[2], obj_mal_sex$ci.ub[2])),2)
```

    ## [1] -0.01 -0.03  0.01

``` r
# Subjective measure of bullying
subj_bull_sex <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                       data = data, mods = percent_female, subset = exposure_type_broad =="bullying"& !is.na(percent_female), slab=paste(ref, es_id, sep=", "))
subj_bull_sex
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0036  0.0597     45     no        es_id 
    ## sigma^2.2  0.0037  0.0607      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 152.7844, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.1356, p-val = 0.7127
    ## 
    ## Model Results:
    ## 
    ##          estimate      se    zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt    0.0920  0.0869  1.0577  0.2902  -0.0784  0.2624    
    ## mods       0.0005  0.0014  0.3682  0.7127  -0.0023  0.0033    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
fisherz2r(c(subj_bull_sex$beta[2], subj_bull_sex$ci.lb[2], subj_bull_sex$ci.ub[2])) #sex moderator (female)
```

    ## [1]  0.0005207722 -0.0022511345  0.0032926710

``` r
# Objective measure of bullying
obj_bull_sex <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                         data = data, mods = percent_female, subset = exposure_type_broad =="bullying"& !is.na(percent_female), slab=paste(ref, es_id, sep=", "))
obj_bull_sex
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0053  0.0726     45     no        es_id 
    ## sigma^2.2  0.0018  0.0423      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 217.4601, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.1063, p-val = 0.2929
    ## 
    ## Model Results:
    ## 
    ##          estimate      se     zval    pval    ci.lb   ci.ub   ​ 
    ## intrcpt    0.1032  0.0700   1.4745  0.1403  -0.0340  0.2404    
    ## mods      -0.0012  0.0011  -1.0518  0.2929  -0.0033  0.0010    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Convert back to r 
fisherz2r(c(obj_bull_sex$beta[2], obj_bull_sex$ci.lb[2], obj_bull_sex$ci.ub[2])) #sex mod
```

    ## [1] -0.0011538159 -0.0033038606  0.0009962394

##### 2.8 Test publication bias

###### 2.8.1 Maltreatment

``` r
# Egger's Test
res_pub_bias <- rma.mv(z_r_subj, z_var_subj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                       data=data, mods = I(1/N_Included), subset=exposure_type_broad=="maltreatment")
summary (res_pub_bias)
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ##    logLik   Deviance        AIC        BIC       AICc  ​ 
    ##   54.7775  -109.5550   -99.5550   -86.9461   -98.8574   
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0091  0.0955     94     no        es_id 
    ## sigma^2.2  0.0071  0.0843      6     no          ref 
    ## sigma^2.3  0.0011  0.0331      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 92) = 550.6769, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0179, p-val = 0.8934
    ## 
    ## Model Results:
    ## 
    ##          estimate       se     zval    pval     ci.lb    ci.ub   ​ 
    ## intrcpt    0.1675   0.0793   2.1116  0.0347    0.0120   0.3230  * 
    ## mods      -2.8514  21.2881  -0.1339  0.8934  -44.5753  38.8726    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Egger's Test
res_pub_bias <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                       data=data, mods = I(1/N_Included), subset=exposure_type_broad=="maltreatment") 
summary (res_pub_bias)
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 94; method: REML)
    ## 
    ##    logLik   Deviance        AIC        BIC       AICc  ​ 
    ##   54.1517  -108.3034   -98.3034   -85.6944   -97.6057   
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0101  0.1003     94     no        es_id 
    ## sigma^2.2  0.0000  0.0000      6     no          ref 
    ## sigma^2.3  0.0078  0.0884      5     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 92) = 367.7928, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 0.0082, p-val = 0.9276
    ## 
    ## Model Results:
    ## 
    ##          estimate       se    zval    pval     ci.lb    ci.ub   ​ 
    ## intrcpt    0.0512   0.0740  0.6922  0.4888   -0.0939   0.1963    
    ## mods       1.6438  18.1006  0.0908  0.9276  -33.8327  37.1202    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

###### 2.8.2 Bullying

``` r
# Egger's Test
res_pub_bias <- rma.mv(z_r_subj, z_var_subj,  random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                       data=data, mods = I(1/N_Included), subset=exposure_type_broad=="bullying") 
summary (res_pub_bias)
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ##   logLik  Deviance       AIC       BIC      AICc  ​ 
    ##  45.3752  -90.7504  -80.7504  -71.9444  -79.1288   
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0033  0.0570     45     no        es_id 
    ## sigma^2.2  0.0048  0.0691      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 157.9175, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.5434, p-val = 0.2141
    ## 
    ## Model Results:
    ## 
    ##          estimate       se     zval    pval     ci.lb    ci.ub     ​ 
    ## intrcpt    0.1777   0.0522   3.4049  0.0007    0.0754   0.2800  *** 
    ## mods     -26.3116  21.1793  -1.2423  0.2141  -67.8222  15.1990      
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Egger's Test
res_pub_bias <- rma.mv(z_r_obj, z_var_obj, random=list(~ 1 | es_id, ~ 1 | ref, ~ 1 | cohort_name),
                       data=data, mods = I(1/N_Included), subset=exposure_type_broad=="bullying") 
summary (res_pub_bias)
```

    ## 
    ## Multivariate Meta-Analysis Model (k = 45; method: REML)
    ## 
    ##   logLik  Deviance       AIC       BIC      AICc  ​ 
    ##  45.1054  -90.2108  -80.2108  -71.4048  -78.5892   
    ## 
    ## Variance Components:
    ## 
    ##             estim    sqrt  nlvls  fixed       factor 
    ## sigma^2.1  0.0053  0.0727     45     no        es_id 
    ## sigma^2.2  0.0005  0.0225      9     no          ref 
    ## sigma^2.3  0.0000  0.0000      8     no  cohort_name 
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 43) = 191.4197, p-val < .0001
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 4.8080, p-val = 0.0283
    ## 
    ## Model Results:
    ## 
    ##          estimate       se     zval    pval     ci.lb    ci.ub    ​ 
    ## intrcpt    0.0957   0.0346   2.7626  0.0057    0.0278   0.1636  ** 
    ## mods     -28.4485  12.9741  -2.1927  0.0283  -53.8771  -3.0198   * 
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##### 2.9 Leave one out analysis

###### 2.9.1 Maltreatment

``` r
groupids <- unique(data$ref[data$exposure_type_broad=="maltreatment"])
groupids <- na.omit(groupids)

leave1out_study <- matrix(rep(NA, length(groupids)), ncol=9, nrow=length(groupids))  

for(i in 1:length(groupids)) {  
  dataexcl <- subset(data, exposure_type_broad=="maltreatment") # subset to maltreatment data
  dataexcl <- subset(dataexcl, ref!=groupids[i])  
  print(nrow(dataexcl))
  
N_rows <- psych::describe(dataexcl$N_Included)$n
mean_N <- psych::describe(dataexcl$N_Included)$mean

# Run meta-analysis for subjective measure
meta_subj <- rma.mv(z_r_subj, z_var_subj,  random=list(~ 1 | es_id, ~ 1 | ref, ~1 | cohort_name), data=dataexcl) 
# Run meta-analysis for objective measure
meta_obj <- rma.mv(z_r_obj, z_var_obj,  random=list(~ 1 | es_id, ~ 1 | ref, ~1 | cohort_name), data=dataexcl) 

# Extract results
  leave1out_study[i,1] <- i # number study omitted
  leave1out_study[i,2] <- N_rows 
  leave1out_study[i,3] <- mean_N 
  leave1out_study[i,4] <- fisherz2r(meta_subj$beta[1]) # subjective ES
  leave1out_study[i,5] <- fisherz2r(meta_subj$ci.lb[1]) # subjective low CI
  leave1out_study[i,6] <- fisherz2r(meta_subj$ci.ub[1]) # subjective upper CI
  leave1out_study[i,7] <- fisherz2r(meta_obj$beta[1]) # objective ES
  leave1out_study[i,8] <- fisherz2r(meta_obj$ci.lb[1]) # objective low CI
  leave1out_study[i,9] <- fisherz2r(meta_obj$ci.ub[1]) # objective upper CI
}
```

    ## [1] 75
    ## [1] 82
    ## [1] 74
    ## [1] 66
    ## [1] 85
    ## [1] 88

``` r
leave1out_study
```

    ##      [,1] [,2]     [,3]      [,4]       [,5]      [,6]       [,7]          [,8]
    ## [1,]    1   75 311.6533 0.1403488 0.06332790 0.2157077 0.07749966 -0.0008134299
    ## [2,]    2   82 449.4878 0.1809683 0.13163004 0.2294122 0.05289029 -0.0258648614
    ## [3,]    3   74 579.7027 0.1721480 0.09189953 0.2501748 0.06475871 -0.0326261512
    ## [4,]    4   66 606.3333 0.1527152 0.06718148 0.2360206 0.05409237 -0.0453153831
    ## [5,]    5   85 505.3412 0.1541261 0.07061762 0.2354892 0.05900240 -0.0161860113
    ## [6,]    6   88 504.4091 0.1361021 0.06738465 0.2035344 0.02218266 -0.0210950147
    ##            [,9]
    ## [1,] 0.15486794
    ## [2,] 0.13099296
    ## [3,] 0.16092550
    ## [4,] 0.15243936
    ## [5,] 0.13352727
    ## [6,] 0.06537735

``` r
leave1out_study[,1] <- as.vector(groupids) # Name studies
leave1out_study_df <- as.data.frame(leave1out_study) # Convert to dataframe
names(leave1out_study_df) <-c('ref', 'N_rows', 'Mean_N', 
                              'subj_es', 'subj_ci.lb', 'subj_ci.ub',
                              'obj_es', 'obj_ci.lb', 'obj_ci.ub') # Rename columns
# Convert columns from character to numeric
leave1out_study_df <- leave1out_study_df %>% mutate_at(c('N_rows', 'Mean_N', 
                                                         'subj_es', 'subj_ci.lb', 'subj_ci.ub',
                                                         'obj_es', 'obj_ci.lb', 'obj_ci.ub'), as.numeric)

# Check studies that when removed, led to the minimum and maximum effect size
# Subjective meta-analysis
leave1out_study_df[which.min(leave1out_study_df$subj_es),c('ref', 'subj_es', 'subj_ci.lb', 'subj_ci.ub')]
```

    ##                ref   subj_es subj_ci.lb subj_ci.ub
    ## 6 Cho et al.,_2016 0.1361021 0.06738465  0.2035344

``` r
leave1out_study_df[which.max(leave1out_study_df$subj_es),c('ref', 'subj_es', 'subj_ci.lb', 'subj_ci.ub')]
```

    ##                  ref   subj_es subj_ci.lb subj_ci.ub
    ## 2 White et al.,_2016 0.1809683    0.13163  0.2294122

``` r
# Objective meta-analysis
leave1out_study_df[which.min(leave1out_study_df$obj_es),c('ref','obj_es', 'obj_ci.lb', 'obj_ci.ub')]
```

    ##                ref     obj_es   obj_ci.lb  obj_ci.ub
    ## 6 Cho et al.,_2016 0.02218266 -0.02109501 0.06537735

``` r
leave1out_study_df[which.max(leave1out_study_df$obj_es),c('ref','obj_es', 'obj_ci.lb', 'obj_ci.ub')]
```

    ##                     ref     obj_es     obj_ci.lb obj_ci.ub
    ## 1 Danese & Widom.,_2020 0.07749966 -0.0008134299 0.1548679

###### 2.9.2 Bullying

``` r
groupids <- unique(data$ref[data$exposure_type_broad=="bullying"])
groupids <- na.omit(groupids)

leave_one_out <- function(data, exposure_type) 
  
# Run two lines below separately
groupids <- unique(data$ref[data$exposure_type_broad=="bullying"])
groupids <- na.omit(groupids)
leave1out_study <- matrix(rep(NA, length(groupids)), ncol=9, nrow=length(groupids))  

for(i in 1:length(groupids)) {  
  dataexcl <- subset(data, exposure_type_broad=="bullying") # subset to bullying data
  dataexcl <- subset(dataexcl, ref!=groupids[i])  
  print(nrow(dataexcl))
  
  N_rows <- psych::describe(dataexcl$N_Included)$n
  mean_N <- psych::describe(dataexcl$N_Included)$mean

# Run meta-analysis for subjective measure
meta_subj <- rma.mv(z_r_subj, z_var_subj,  random=list(~ 1 | es_id, ~ 1 | ref, ~1 | cohort_name), data=dataexcl) 
# Run meta-analysis for objective measure
meta_obj <- rma.mv(z_r_obj, z_var_obj,  random=list(~ 1 | es_id, ~ 1 | ref, ~1 | cohort_name), data=dataexcl) 

# Extract results
  leave1out_study[i,1] <- i # number study omitted
  leave1out_study[i,2] <- N_rows 
  leave1out_study[i,3] <- mean_N 
  leave1out_study[i,4] <- fisherz2r(meta_subj$beta[1]) # subjective ES
  leave1out_study[i,5] <- fisherz2r(meta_subj$ci.lb[1]) # subjective low CI
  leave1out_study[i,6] <- fisherz2r(meta_subj$ci.ub[1]) # subjective upper CI
  leave1out_study[i,7] <- fisherz2r(meta_obj$beta[1]) # objective ES
  leave1out_study[i,8] <- fisherz2r(meta_obj$ci.lb[1]) # objective low CI
  leave1out_study[i,9] <- fisherz2r(meta_obj$ci.ub[1]) # objective upper CI
}
```

    ## [1] 44
    ## [1] 43
    ## [1] 43
    ## [1] 44
    ## [1] 41
    ## [1] 30
    ## [1] 42
    ## [1] 29
    ## [1] 44

``` r
leave1out_study
```

    ##       [,1] [,2]     [,3]      [,4]       [,5]      [,6]       [,7]         [,8]
    ##  [1,]    1   44 494.9773 0.1192298 0.07028336 0.1676034 0.03502799 -0.010633647
    ##  [2,]    2   43 507.7442 0.1314150 0.09023054 0.1721507 0.03654767 -0.010182582
    ##  [3,]    3   43 467.8837 0.1086072 0.07279948 0.1441352 0.03301498 -0.014915359
    ##  [4,]    4   44 501.9318 0.1134101 0.06991538 0.1564744 0.03629568 -0.008940745
    ##  [5,]    5   41 472.2683 0.1260949 0.07141865 0.1800156 0.02321487 -0.016055322
    ##  [6,]    6   30 520.3000 0.1284512 0.07371591 0.1824149 0.03945298 -0.014118392
    ##  [7,]    7   42 439.7381 0.1248503 0.07270765 0.1763124 0.01876927 -0.020627473
    ##  [8,]    8   29 643.0000 0.1154333 0.06253684 0.1676832 0.04383119 -0.004911799
    ##  [9,]    9   44 502.7273 0.1278063 0.08166804 0.1733979 0.03486560 -0.010531932
    ##             [,9]
    ##  [1,] 0.08054386
    ##  [2,] 0.08311863
    ##  [3,] 0.08079394
    ##  [4,] 0.08138384
    ##  [5,] 0.06241355
    ##  [6,] 0.09279850
    ##  [7,] 0.05810781
    ##  [8,] 0.09236640
    ##  [9,] 0.08011971

``` r
leave1out_study[,1] <- as.vector(groupids) # Name studies
leave1out_study_df <- as.data.frame(leave1out_study) # Convert to dataframe
names(leave1out_study_df) <-c('ref', 'N_rows', 'Mean_N', 
                              'subj_es', 'subj_ci.lb', 'subj_ci.ub',
                              'obj_es', 'obj_ci.lb', 'obj_ci.ub') # Rename columns
# Convert columns from character to numeric
leave1out_study_df <- leave1out_study_df %>% mutate_at(c('N_rows', 'Mean_N', 
                                                         'subj_es', 'subj_ci.lb', 'subj_ci.ub',
                                                         'obj_es', 'obj_ci.lb', 'obj_ci.ub'), as.numeric)

# Check studies that when removed, led to the minimum and maximum effect size
# Subjective meta-analysis
leave1out_study_df[which.min(leave1out_study_df$subj_es),c('ref', 'subj_es', 'subj_ci.lb', 'subj_ci.ub')]
```

    ##                   ref   subj_es subj_ci.lb subj_ci.ub
    ## 3 Bouman et al.,_2012 0.1086072 0.07279948  0.1441352

``` r
leave1out_study_df[which.max(leave1out_study_df$subj_es),c('ref', 'subj_es', 'subj_ci.lb', 'subj_ci.ub')]
```

    ##                           ref  subj_es subj_ci.lb subj_ci.ub
    ## 2 Zimmer-Gembeck et al.,_2012 0.131415 0.09023054  0.1721507

``` r
# Objective meta-analysis
leave1out_study_df[which.min(leave1out_study_df$obj_es),c('ref','obj_es', 'obj_ci.lb', 'obj_ci.ub')]
```

    ##                   ref     obj_es   obj_ci.lb  obj_ci.ub
    ## 7 Mulder et al.,_2017 0.01876927 -0.02062747 0.05810781

``` r
leave1out_study_df[which.max(leave1out_study_df$obj_es),c('ref','obj_es', 'obj_ci.lb', 'obj_ci.ub')]
```

    ##                    ref     obj_es    obj_ci.lb obj_ci.ub
    ## 8 Mcclain et al.,_2020 0.04383119 -0.004911799 0.0923664
