####################################################################################################
#
# R code to reproduce data download, cleaning and analysis for Nepal DHS data for Foster et al. 2020
#
# Written by Dugald Foster
# email: dugaldwefoster@gmail.com
# please email me if you notice any errors
#
# OUTLINE
# 1. Install rdhs package and download DHS data
# 2. Data cleaning and manipulation
# 3. Select cases for analysis
# 4. Analysis
#
####################################################################################################

#Set working directory
#setwd()

##### 1.Install rdhs package and download DHS data #####

#Install rdhs package
#install.packages("devtools")
#devtools::install_github("OJWatson/rdhs")
library(rdhs)

#Show country IDs
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))
str(ids)

#Find all surveys from 1990 onwards for Bangladesh, India, Nepal and Pakistan
survs <- dhs_surveys(countryIds = c("BD","IA", "NP", "PK"),surveyYearStart = 1990)

#Download flat Individual Recode datasets for all 4 countries
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "flat", fileType = "IR")
str(datasets)

#Keep only country-wide datasets (i.e. drop all Indian regional datsets)
irdata <- datasets[grep("BD|IA|NP|PK", datasets$FileName, ignore.case = TRUE), ]

#Download datasets
#Prior to download, create plain text file in directory called "credentials.txt", showing
#DHS credentials in following format:
#email="myemail"
#password="mypassword"
#project="projectname"

#set credentials
set_dhs_credentials(credentials="credentials.txt")

#Download IR datasets
downloads <- get_datasets(datasets[grep("NP", datasets$FileName), ])

#Read in data for IR files
npir96 <- readRDS(downloads$NPIR31FL)
npir01 <- readRDS(downloads$NPIR41FL)
npir06 <- readRDS(downloads$NPIR51FL)
npir11 <- readRDS(downloads$NPIR61FL)
npir16 <- readRDS(downloads$NPIR7HFL)

##### 2. Data cleaning and manipulation #####

### Merge all datasets together ###

#rbind.fill will add columns that are not present in all datasets
library(plyr)
allsurvs <- rbind.fill(npir01, npir06, npir11, npir16)

#Select variables to extract from allsurvs
myvars <- c("caseid", "v000", "v001", "v002", "v003", "v005", "v010", "v012", "v021", "v022", "v023", 
            "v025",  "v131", "v136", "v138", "v149", "v150", "v201", "v202", "v203", "v206", "v207", 
            "v212", "v218", "v190", "v191",
            "bidx_01", "bidx_02", "bidx_03", "bidx_04","bidx_05", "bidx_06", "bidx_07", "bidx_08", 
            "bidx_09", "bidx_10", "bidx_11", "bidx_12", "bidx_13", "bidx_14", "bidx_15", "bidx_16", 
            "bidx_17", "bidx_18", "bidx_19", "bidx_20",
            "b0_01", "b0_02", "b0_03", "b0_04", "b0_05", "b0_06", "b0_07", "b0_08", "b0_09", "b0_10", 
            "b0_11", "b0_12", "b0_13", "b0_14", "b0_15", "b0_16", "b0_17", "b0_18", "b0_19", "b0_20", 
            "b2_01", "b2_02", "b2_03", "b2_04", "b2_05", "b2_06", "b2_07", "b2_08", "b2_09", "b2_10", 
            "b2_11", "b2_12", "b2_13", "b2_14", "b2_15", "b2_16", "b2_17", "b2_18", "b2_19", "b2_20",
            "b4_01", "b4_02", "b4_03", "b4_04", "b4_05", "b4_06", "b4_07", "b4_08", "b4_09", "b4_10", 
            "b4_11", "b4_12", "b4_13", "b4_14", "b4_15", "b4_16", "b4_17", "b4_18", "b4_19", "b4_20", 
            "b5_01", "b5_02", "b5_03", "b5_04", "b5_05", "b5_06", "b5_07", "b5_08", "b5_09", "b5_10",
            "b5_11", "b5_12", "b5_13", "b5_14", "b5_15", "b5_16", "b5_17", "b5_18", "b5_19", "b5_20",
            "b7_01", "b7_02", "b7_03", "b7_04", "b7_05", "b7_06", "b7_07", "b7_08", "b7_09", "b7_10",
            "b7_11", "b7_12", "b7_13", "b7_14", "b7_15", "b7_16", "b7_17", "b7_18", "b7_19", "b7_20",
            "b11_01", "b11_02", "b11_03", "b11_04", "b11_05", "b11_06", "b11_07", "b11_08", "b11_09", 
            "b11_10", "b11_11", "b11_12", "b11_13", "b11_14", "b11_15", "b11_16", "b11_17", "b11_18", 
            "b11_19", "b11_20", "b12_01", "b12_02", "b12_03", "b12_04", "b12_05", "b12_06", "b12_07", 
            "b12_08", "b12_09", "b12_10", "b12_11", "b12_12", "b12_13", "b12_14", "b12_15", "b12_16", 
            "b12_17", "b12_18", "b12_19", "b12_20")

dhsdata <- allsurvs[myvars]

#Check var labels
(get_var_labels(dhsdata))

### Date Reformatting ###
#Gregorian calendar year is roughly equivalent to Nepali year - 57
#Once dates are reformatted to 4 digits, subtract 57 from each value
dhsdata$b2_01 <- (dhsdata$b2_01)-57
dhsdata$b2_02 <- (dhsdata$b2_02)-57
dhsdata$b2_03 <- (dhsdata$b2_03)-57
dhsdata$b2_04 <- (dhsdata$b2_04)-57
dhsdata$b2_05 <- (dhsdata$b2_05)-57
dhsdata$b2_06 <- (dhsdata$b2_06)-57
dhsdata$b2_07 <- (dhsdata$b2_07)-57
dhsdata$b2_08 <- (dhsdata$b2_08)-57
dhsdata$b2_09 <- (dhsdata$b2_09)-57
dhsdata$b2_10 <- (dhsdata$b2_10)-57
dhsdata$b2_11 <- (dhsdata$b2_11)-57
dhsdata$b2_12 <- (dhsdata$b2_12)-57
dhsdata$b2_13 <- (dhsdata$b2_13)-57
dhsdata$b2_14 <- (dhsdata$b2_14)-57
dhsdata$b2_15 <- (dhsdata$b2_15)-57
dhsdata$b2_16 <- (dhsdata$b2_16)-57
dhsdata$b2_17 <- (dhsdata$b2_17)-57
dhsdata$b2_18 <- (dhsdata$b2_18)-57
dhsdata$b2_19 <- (dhsdata$b2_19)-57
dhsdata$b2_20 <- (dhsdata$b2_20)-57

### Create new variables ###
#Create new variable for total no. children died
library(dplyr)
dhsdata <- dhsdata %>%
  mutate(tcd = v206 + v207)

#Relabel tcd
library(Hmisc)
label(dhsdata$tcd) <- "Total children died"

#Reorder vars so tcd comes after son and daughter deaths (v207)
names(dhsdata)
dhsdata <- dhsdata[,c(1:22,187,23:186)]

#Create new variable for maternal age at birth x
#Convert respondent birth dates to Gregorian calendar
dhsdata$v010 <- (dhsdata$v010)-57

#Create new maternal age at birth x variable = year of birth x - mother's year of birth
dhsdata$"matage_01" <- dhsdata$b2_01 - dhsdata$v010
dhsdata$"matage_02" <- dhsdata$b2_02 - dhsdata$v010
dhsdata$"matage_03" <- dhsdata$b2_03 - dhsdata$v010
dhsdata$"matage_04" <- dhsdata$b2_04 - dhsdata$v010
dhsdata$"matage_05" <- dhsdata$b2_05 - dhsdata$v010
dhsdata$"matage_06" <- dhsdata$b2_06 - dhsdata$v010
dhsdata$"matage_07" <- dhsdata$b2_07 - dhsdata$v010
dhsdata$"matage_08" <- dhsdata$b2_08 - dhsdata$v010
dhsdata$"matage_09" <- dhsdata$b2_09 - dhsdata$v010
dhsdata$"matage_10" <- dhsdata$b2_10 - dhsdata$v010
dhsdata$"matage_11" <- dhsdata$b2_11 - dhsdata$v010
dhsdata$"matage_12" <- dhsdata$b2_12 - dhsdata$v010
dhsdata$"matage_13" <- dhsdata$b2_13 - dhsdata$v010
dhsdata$"matage_14" <- dhsdata$b2_14 - dhsdata$v010
dhsdata$"matage_15" <- dhsdata$b2_15 - dhsdata$v010
dhsdata$"matage_16" <- dhsdata$b2_16 - dhsdata$v010
dhsdata$"matage_17" <- dhsdata$b2_17 - dhsdata$v010
dhsdata$"matage_18" <- dhsdata$b2_18 - dhsdata$v010
dhsdata$"matage_19" <- dhsdata$b2_19 - dhsdata$v010
dhsdata$"matage_20" <- dhsdata$b2_20 - dhsdata$v010

#Change labels to "mother's age at birth"
library(Hmisc)
describe(dhsdata$matage_01)
label(dhsdata$matage_01) <- "Mother's age at birth"
label(dhsdata$matage_02) <- "Mother's age at birth"
label(dhsdata$matage_03) <- "Mother's age at birth"
label(dhsdata$matage_04) <- "Mother's age at birth"
label(dhsdata$matage_05) <- "Mother's age at birth"
label(dhsdata$matage_06) <- "Mother's age at birth"
label(dhsdata$matage_07) <- "Mother's age at birth"
label(dhsdata$matage_08) <- "Mother's age at birth"
label(dhsdata$matage_09) <- "Mother's age at birth"
label(dhsdata$matage_10) <- "Mother's age at birth"
label(dhsdata$matage_11) <- "Mother's age at birth"
label(dhsdata$matage_12) <- "Mother's age at birth"
label(dhsdata$matage_13) <- "Mother's age at birth"
label(dhsdata$matage_14) <- "Mother's age at birth"
label(dhsdata$matage_15) <- "Mother's age at birth"
label(dhsdata$matage_16) <- "Mother's age at birth"
label(dhsdata$matage_17) <- "Mother's age at birth"
label(dhsdata$matage_18) <- "Mother's age at birth"
label(dhsdata$matage_19) <- "Mother's age at birth"
label(dhsdata$matage_20) <- "Mother's age at birth"

#Check maternal age at birth dates are ok
describe(dhsdata$matage_01)

##### 3. Select cases for analysis and reshape data #####
#NB Do not sort on caseid, as there are multiple duplicates (from different surveys)

#Select observations with >0 children ever born (mothers)
mums <- subset(dhsdata, dhsdata$v201 > 0)

#Check maximum CEB
#Detach dplyr package to enable plyr::count function
detach("package:dplyr", unload=TRUE)
count(mums$v201)

#Max CEB is 16, ignore all subsequent birth order columns (i.e. births 17:20 for cols bidx:matage)

#Sort by survey, cluster, household, respline
mums <- mums[
  with(mums, order(v000, v001, v002, v003)),
  ]

#Relabel relationship to head of household categories
mums$v150 <- factor(mums$v150,
                    levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                               13, 14),
                    labels = c("head", "wife", "daughter", "daughter-in-law",
                               "granddaughter", "mother", "mother-in-law", 
                               "sister", "co-wife", "other relative", 
                               "adopted daughter", "not related", 
                               "niece by (blood)", "missing"))

### Reshape data from wide to long format ###
library(reshape2)

#Reshape birth columns to correspond to single variables in long format
longmums <- reshape(mums, direction = "long",  idvar = c("v000","v001", "v002", "v003"),
                    varying = list(c("bidx_01", "bidx_02", "bidx_03", "bidx_04", "bidx_05", "bidx_06", 
                                     "bidx_07", "bidx_08", "bidx_09", "bidx_10", "bidx_11", "bidx_12", 
                                     "bidx_13", "bidx_14", "bidx_15", "bidx_16", "bidx_17", "bidx_18", 
                                     "bidx_19", "bidx_20"), 
                                   c("b0_01", "b0_02", "b0_03", "b0_04", "b0_05", "b0_06", "b0_07", "b0_08", 
                                     "b0_09", "b0_10", "b0_11", "b0_12", "b0_13", "b0_14", "b0_15", "b0_16", 
                                     "b0_17", "b0_18", "b0_19", "b0_20"), 
                                   c("b2_01", "b2_02", "b2_03", "b2_04", "b2_05", "b2_06", "b2_07", "b2_08", 
                                     "b2_09", "b2_10", "b2_11", "b2_12", "b2_13", "b2_14", "b2_15", "b2_16",
                                     "b2_17", "b2_18", "b2_19", "b2_20"), 
                                   c("b4_01", "b4_02", "b4_03", "b4_04", "b4_05", "b4_06", "b4_07", "b4_08", 
                                     "b4_09", "b4_10", "b4_11", "b4_12", "b4_13", "b4_14", "b4_15", "b4_16",
                                     "b4_17", "b4_18", "b4_19", "b4_20"), 
                                   c("b5_01", "b5_02", "b5_03", "b5_04", "b5_05", "b5_06", "b5_07", "b5_08", 
                                     "b5_09", "b5_10", "b5_11", "b5_12", "b5_13", "b5_14", "b5_15", "b5_16",
                                     "b5_17", "b5_18", "b5_19", "b5_20"), 
                                   c("b7_01", "b7_02", "b7_03", "b7_04", "b7_05", "b7_06", "b7_07", "b7_08", 
                                     "b7_09", "b7_10", "b7_11", "b7_12", "b7_13", "b7_14", "b7_15", "b7_16",
                                     "b7_17", "b7_18", "b7_19", "b7_20"), 
                                   c("b11_01", "b11_02", "b11_03", "b11_04", "b11_05", "b11_06", "b11_07", 
                                     "b11_08", "b11_09", "b11_10", "b11_11", "b11_12", "b11_13", "b11_14", 
                                     "b11_15", "b11_16", "b11_17", "b11_18", "b11_19", "b11_20"), 
                                   c("b12_01", "b12_02", "b12_03", "b12_04", "b12_05", "b12_06", "b12_07", 
                                     "b12_08", "b12_09", "b12_10", "b12_11", "b12_12", "b12_13", "b12_14", 
                                     "b12_15", "b12_16", "b12_17", "b12_18", "b12_19", "b12_20"), 
                                   c("matage_01", "matage_02", "matage_03", "matage_04", "matage_05", 
                                     "matage_06", "matage_07", "matage_08", "matage_09", "matage_10", 
                                     "matage_11", "matage_12", "matage_13", "matage_14", "matage_15", 
                                     "matage_16", "matage_17", "matage_18", "matage_19", "matage_20")),
                    v.names = c("order", "twin", "yob", "sex", "alive", "agedeath", "pbi", "sbi", "matage"), 
                    timevar = "birth", times = c("bidx_01", "bidx_02", "bidx_03", "bidx_04", "bidx_05", 
                                                 "bidx_06", "bidx_07", "bidx_08", "bidx_09", "bidx_10", 
                                                 "bidx_11", "bidx_12", "bidx_13", "bidx_14", "bidx_15", 
                                                 "bidx_16", "bidx_17", "bidx_18", "bidx_19", "bidx_20"))


#Drop cases with "NA" in "order" column
longmums <- longmums[complete.cases(longmums[ , 29]),]

#Rename "alive" column to "mortality", swap 0's and 1's
names(longmums)
colnames(longmums)[33] <- "mortality"
longmums$mortality <- !longmums$mortality
longmums$mortality <- as.numeric(longmums$mortality)

#Sort by survey, cluster, household, respline
longmums <- longmums[
  with(longmums, order(v000, v001, v002, v003)),
  ]

#Remove cases with death over the age of 5 (so we only measure child mortality)
#NB see grouping in reported age at death (1 year, 2 years, 3 years)
detach("package:dplyr", unload = TRUE)
count(longmums$agedeath)
longmums <- subset(longmums, longmums$agedeath <= 60 | is.na(longmums$agedeath))

### Add a column to identify cases with reproductive overlap ###
library(dplyr)

#Create column called "overlap" which returns TRUE for cases that were born <=2 years
#of other cases in the same household, but which had different mothers
longmums <- mutate(group_by(longmums, v000, v001, v002),
                   overlap = mapply(function(x,y) {any(abs(x-yob)<=2 & y!=v003)}, yob, v003))

#Change TRUE/FALSE to 1/0
longmums <- longmums %>% mutate(overlap = as.numeric(overlap))

#Edit "twin" variable to reflect whether birth was part of a multiple birth (0 no, 1 yes)
longmums$twin <- ifelse(longmums$twin > 0, 1, 0)

### Mothers and Daughters ###

#Create column called "mdoverlap" which returns TRUE for cases that were born <=2 years
#of other cases in the same household, but which had different mothers (who were mothers or daughters)
longmums <- mutate(group_by(longmums, v000, v001, v002),
                   mdoverlap = mapply(function(x,y,z) {any(abs(x-yob)<=2 & y!=v003 & 
                                                             (v150 == "head" | v150 == "wife" | v150=="daughter") & 
                                                             (z == "head" | z == "wife" | z=="daughter"))}, yob, v003, v150))

#Change TRUE/FALSE to 1/0
longmums <- longmums %>% mutate(mdoverlap = as.numeric(mdoverlap))

#Subset only mothers and daughters
longmumsdaughts <- subset(longmums, v150 == "head" | v150 == "wife" | v150 == "daughter")

#Remove previous overlap column which came from longmums
names(longmumsdaughts)
longmumsdaughts <- subset(longmumsdaughts, select = -c(38))

#Sort by survey, cluster, household, respline
longmumsdaughts <- longmumsdaughts[
  with(longmumsdaughts, order(v000, v001, v002, v003)),
  ]

##### 4. Analysis #####

### Model Analyses ###

#Fit logistic regression models

#Change reference level
#use the relevel() function, eg:
#longmums$sex <- relevel(longmums$sex, ref = "2")

#install.packages("survey")
library(survey) 

#Calculate survey weights
longmums$sampleweights <- longmums$v005 / 1000000

#Set survey design
dhsdesign <- svydesign(id= ~v021 + v002, strata = ~v025, weights = ~sampleweights, data = longmums,
                       nest = TRUE)

#Bivariate glm for mortality and overlap
bimodel <- svyglm(mortality ~ overlap, 
                  family = quasibinomial(link = logit), dhsdesign)

#View model output
summary(bimodel)

#Calculate standard odds ratios and 95% CIs
#exp(coef(model)) for a glm fit with "family = quasibinomial" will give odds ratios
exp(cbind(OR = coef(bimodel), confint(bimodel)))

#Fit preregistered model
#(see model defined in preregistration- https://osf.io/5r9fm/register/5771ca429ad5a1020de2872e)
preregmodel <- svyglm(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob, 
                      family = quasibinomial(link = logit), dhsdesign)

#View model output
summary(preregmodel)

#Calculate standard odds ratios and 95% CIs
exp(cbind(OR = coef(preregmodel), confint(preregmodel)))

#Fit model to provide relative risk ratios
#exp(coef(model)) for a glm fit with family = quasipoisson will give relative risk ratios
rrpreregmodel <- svyglm(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob, 
                        family = quasipoisson, dhsdesign)

#Realtive risk ratios
exp(cbind(OR = coef(rrpreregmodel), confint(rrpreregmodel)))

### Exploratory Models ###
#Fit 1st exploratory model: include birth order as control
expmodel1 <- svyglm(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + 
                      order, 
                    family = quasibinomial(link = logit), dhsdesign)

#View model output
summary(expmodel1)

#Calculate standard odds ratios and 95% CIs
exp(cbind(OR = coef(expmodel1), confint(expmodel1)))

### Fit 2nd exploratory model with main exposure as v138 instead of overlap ###
expmodel2 <- svyglm(mortality ~ v138 + sex + matage + twin + pbi + sbi + v136 + v149 + yob, 
                    family = quasibinomial(link = logit), dhsdesign)

#View model output
summary(expmodel2)

#Calculate standard odds ratios and 95% CIs
exp(cbind(OR = coef(expmodel2), confint(expmodel2)))

#Fit model to provide RRs for expmodel2
rrexpmodel2 <- svyglm(mortality ~ v138 + sex + matage + twin + pbi + sbi + v136 + v149 + yob, 
                      family = quasipoisson, dhsdesign)

#RRs for expmodel2
exp(cbind(OR = coef(rrexpmodel2), confint(rrexpmodel2)))

### Fit 3rd model including order, v203, v138, v025 and v190 ###
expmodel3 <- svyglm(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + 
                      order + v203 + v138 + v025 + v190, 
                    family = quasibinomial(link = logit), dhsdesign)

#View model output
summary(expmodel3)

#Calculate standard odds ratios and 95% CIs
exp(cbind(OR = coef(expmodel3), confint(expmodel3)))

#Fit model to provide relative risk ratios for expmodel3
#exp(coef(model)) for a glm fit with family = quasipoisson will give relative risk ratios
rrexpmodel3 <- svyglm(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + 
                        order + v203 + v138 + v025 + v190, 
                      family = quasipoisson, dhsdesign)

#Realtive risk ratios
exp(cbind(OR = coef(rrexpmodel3), confint(rrexpmodel3)))

#Fit "null model" including only control variables
noverlapmodel <- svyglm(mortality ~ sex + matage + twin + pbi + sbi + v136 + v149 + v138 + yob, 
                        family = quasibinomial(link = logit), dhsdesign)

#View model output
summary(noverlapmodel)

#Calculate standard odds ratios and 95% CIs
exp(cbind(OR = coef(noverlapmodel), confint(noverlapmodel)))

##### Fit a logistic regression model with cases limited to mothers and daughters #####
#Set survey weights
longmumsdaughts$sampleweights <- longmumsdaughts$v005 / 1000000

#Set survey design
mddesign <- svydesign(id= ~v021 + v002, strata = ~v025, weights = ~sampleweights, data = longmumsdaughts,
                      nest = TRUE)

#Fit second preregistered model
mdmodel <- svyglm(mortality ~ mdoverlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob, 
                  family = quasibinomial(link = logit), mddesign)

#View model output
summary(mdmodel)

#Calculate standard odds ratios and 95% CIs
exp(cbind(OR = coef(mdmodel), confint(mdmodel)))

#Fit model to provide relative risk ratios
rrmdmodel <- svyglm(mortality ~ mdoverlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob, 
                    family = quasipoisson, mddesign)

#Realtive risk ratios
exp(cbind(OR = coef(rrmdmodel), confint(rrmdmodel)))

#Best fitting exploratory model applied to mother/daughter data
mdexpmodel3 <- svyglm(mortality ~ mdoverlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + 
                        order + v203 + v138 + v025 + v190, 
                      family = quasibinomial(link = logit), mddesign)

#View model output
summary(mdexpmodel3)

#Calculate standard odds ratios and 95% CIs
exp(cbind(OR = coef(mdexpmodel3), confint(mdexpmodel3)))

### Compare model fits ###
AIC(preregmodel, noverlapmodel)
AIC(preregmodel, expmodel1, expmodel2, expmodel3)
AIC(preregmodel, mdmodel)

### Plot coefficients of models (log odds) ###
#install.packages("jtools")
library(jtools)

#Plot all coefficients in each model
plot_coefs(preregmodel)
plot_coefs(mdmodel)

#Plot only certain coefficients and name parameters
#plot_coefs(preregmodel, coefs= c("overlap" = "overlap", "sex" = "sex", "maternal age" = "matage",
#                             "twin" = "twin"))

#Plot log odds coefficients in multiple models
plot_coefs(preregmodel, mdmodel, coefs = c("overlap" = "mdoverlap", "overlap" = "overlap",
                                           "sex" = "sex", "maternal age" = "matage", "twin" = "twin",
                                           "pbi" = "pbi", "sbi" = "sbi", "household size" = "v136",
                                           "maternal education" = "v149", "birth year" = "yob"))

#Compare Model Coefficients as Odds Ratios
#plot_coefs(preregmodel, expmodel3, exp = TRUE)

#Compare Model Coefficients as Risk Ratios
plot_coefs(rrpreregmodel, rrexpmodel3, exp = TRUE, 
           coefs = c("overlap" = "overlap",
                     "sex" = "sex", "maternal age" = "matage", "twin" = "twin",
                     "pbi" = "pbi", "sbi" = "sbi", "household size" = "v136",
                     "maternal education" = "v149", "birth year" = "yob",
                     "order" = "order", "daughters in HH" = "v203", 
                     "women in HH" = "v138", "residence" = "v025",
                     "SES" = "v190"))

#View interaction effects between overlap and sex 
#interact_plot(preregmodel, pred = overlap, modx = sex, interval = TRUE)

### Sensitivity Analyses ###
#install.packages("konfound")
library(konfound)

#Bangladesh
pkonfound(est_eff = 1.61, 
          std_err = 0.07, 
          n_obs = 147112, 
          n_covariates = 8)

#India
pkonfound(est_eff = 2.41, 
          std_err = 0.2, 
          n_obs = 1572399, 
          n_covariates = 8)

#Nepal
pkonfound(est_eff = 2.31, 
          std_err = 0.45, 
          n_obs = 107068, 
          n_covariates = 8)

#Pakistan
pkonfound(est_eff = 2.09, 
          std_err = 0.43, 
          n_obs = 88752, 
          n_covariates = 8)


##### Descriptives #####
library(survey) 
library(jtools)

#View unique number of PSUs
length(unique(longmums$v001))

#Proportion of mortality 
svyciprop(~mortality, dhsdesign)

#Proportion of overlap 
svyciprop(~overlap, dhsdesign)

#Frequency of overlap
svytable(~overlap, dhsdesign)

#Proportion of mortality - interaction
#by sex
svymean(~interaction(mortality, sex), dhsdesign)
#by urban/rural residence
svymean(~interaction(mortality, v025), dhsdesign)

### Survey Means ###
#matage
cbind(mean = svymean( ~matage, dhsdesign), svysd( ~matage, dhsdesign))

#v136
cbind(mean = svymean( ~v136, dhsdesign), svysd( ~v136, dhsdesign))

#yob
cbind(mean = svymean( ~yob, dhsdesign), svysd( ~yob, dhsdesign))

#Calculate ratio of son:daughter deaths
sexmortratio <- svyratio( 
  numerator = ~v206, 
  denominator = ~v207, 
  dhsdesign 
)

cbind(sexmortratio, confint(sexmortratio))

#Create bar plot for child mortality prevalence
library(ggplot2)
library(scales)
qplot(as.factor(longmums$mortality)) + 
  geom_bar() + 
  labs(x="Child Mortality Status", y="Count") +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("Alive", "Dead")) +
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})

#Create bar plot for overlap prevalence
#NB breaks/labels not working- try hacking x axis name to provde labels
ggplot(data = longmums) + 
  geom_bar(mapping = aes(x = overlap, y = ..prop..), stat = "count") +
  scale_x_discrete(name = "No Overlap               Overlap",
                   breaks=c("0","1"),
                   labels=c("No Overlap", "Overlap")) +
  scale_y_continuous(labels = scales::percent_format(), breaks = scales::pretty_breaks(n = 5),
                     limits = c(0,1))

#Create bar plot for mortality prevalence by overlap
longmums$status <- ifelse(longmums$mortality == 0, "Alive", "Dead")
ggplot(data = longmums, mapping = aes(x = status, fill = factor(overlap))) +
  geom_bar() +
  scale_fill_discrete(name = "Overlap")

#Plot mortality by other vars
#Education level
ggplot(data = longmums, mapping = aes(x = mortality, fill = factor(v149))) +
  geom_bar() +
  scale_fill_discrete(name = "Level of Education") +
  scale_x_discrete(name = "Alive           Dead")

#Plot overlap by mother's relationship to HoH
ggplot(data = longmums, mapping = aes(x = mortality, fill = factor(v150))) +
  geom_bar() +
  scale_fill_discrete(name = "Relationship to HoH") +
  scale_x_discrete(name = "No overlap  Overlap")

#Create bar plot showing frequencies of rel to HoH, ordered by descending freq
library(plyr)
dframe <- count(mums$v150)
names(dframe)[1] <- "r2hoh"

hohplot <- ggplot(dframe, aes(x=reorder(r2hoh, freq), y=freq)) + 
  geom_bar(stat="identity") +
  theme_bw() + labs(x = "Relationshp to head of household", y = "Frequency") +
  coord_flip()

#Show plot
hohplot

#Covariance between parameters in preregmodel
#install.packages("corrplot")
library(corrplot)

#Parameters in prereg model:
# overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob
names(longmums)
correlations <- cor(longmums[, c(14, 16, 30:32, 35:38)], use = "complete.obs")
corrplot(correlations)

#Parameters in expmodel3: 29, 20, 15, 12
# + order + v203 + v138 + v025 + v190
correlations2 <- cor(longmums[, c(12, 14:16, 20, 26, 29:32, 35:38)], use = "complete.obs")

colnames(correlations2) <- c("residence", "household size", "women in HH","maternal education",
                             "daughters in HH", "SES", "order", "twin", "birth year",
                             "sex", "pbi",  "sbi", "maternal age", "overlap")

rownames(correlations2) <- c("residence", "household size", "women in HH","maternal education",
                             "daughters in HH", "SES", "order", "twin", "birth year",
                             "sex", "pbi",  "sbi", "maternal age", "overlap")

corrplot(correlations2)

#Describe missing data
#install.packages("Amelia", "mlbench")
library(Amelia)
library(mlbench)

missmap(longmums,
        col = c("black", "grey"),
        main = "",
        y.labels = NULL,
        y.at = NULL,
        x.cex = 0.65,
        legend = FALSE)