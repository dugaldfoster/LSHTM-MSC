# Add column to show whether mother is the only mother in a household 
# to have given birth within <=10 & >=5 years before survey date 
# (to account for any effects of complex household structure beyond simply n of women in household at time of survey (v138))

#Create dataframe with cases restricted to those born <= 10 years before survey date to minimise risk of changes in household composition since survey
longmums10yr <- subset(longmums, longmums$yob >= longmums$v007 - 10)

#Check that only cases born <=10 & >=5 years before survey date appear
with(longmums10yr, table(v007, yob))

# Add column "multimum" which returns 
# 0: household with one mother with a birth <=10 & >=5 years before survey date 
# 1: household with >1 mothers with a birth <=10 & >=5 years before survey date 
longmums10yr <- mutate(group_by(longmums10yr, v000, v001, v002),
                   multimum = mapply(function(x) {any(x!=v003)}, v003))

#Change TRUE/FALSE to 1/0
longmums10yr <- longmums10yr %>% mutate(multimum = as.numeric(multimum))

#Check effect of overlap with data subsetted to those from complex households
complexonly10_glmer <- glmer(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + (1 | v002/caseid), data = subset(longmums10yr, longmums10yr$multimum == 1), family = poisson)

#View model output
summary(complexonly10_glmer)

plot_model(complexonly10_glmer,
           show.values = TRUE, 
           value.offset = .3,
           axis.lim = c(0.5,10),
           axis.title = (x = "Relative Risk"), 
           title = "Births <=10 & >=5 ComplexOnly")

# Effect of overlap is significantly reduced...

#Check co-occurrence of overlap and multimum
with(longmums10yr, table(overlap, multimum))

##### 8 yr #####

# Add column "multimum" which returns 
# 0: household with one mother with a birth <=10 & >=5 years before survey date 
# 1: household with >1 mothers with a birth <=10 & >=5 years before survey date 
longmums8yr <- mutate(group_by(longmums8yr, v000, v001, v002),
                      multimum = mapply(function(x) {any(x!=v003)}, v003))

#Change TRUE/FALSE to 1/0
longmums8yr <- longmums8yr %>% mutate(multimum = as.numeric(multimum))

#Check effect of overlap with data subsetted to those from complex households
complexonly8_glmer <- glmer(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + (1 | v002/caseid), data = subset(longmums8yr, longmums8yr$multimum == 1), family = poisson)

#View model output
summary(complexonly8_glmer)

plot_model(complexonly8_glmer,
           show.values = TRUE, 
           value.offset = .3,
           axis.lim = c(0.5,10),
           axis.title = (x = "Relative Risk"), 
           title = "Births <=8 & >=5 ComplexOnly")

#Check co-occurrence of overlap and multimum
with(longmums8yr, table(overlap, multimum))

###### 6yr #####
# Add column "multimum" which returns 
# 0: household with one mother with a birth <=10 & >=5 years before survey date 
# 1: household with >1 mothers with a birth <=10 & >=5 years before survey date 
longmums6yr <- mutate(group_by(longmums6yr, v000, v001, v002),
                      multimum = mapply(function(x) {any(x!=v003)}, v003))

#Change TRUE/FALSE to 1/0
longmums6yr <- longmums6yr %>% mutate(multimum = as.numeric(multimum))

#Add multimum to model
#Check effect of overlap with data subsetted to those from complex households
complexonly6_glmer <- glmer(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + (1 | v002/caseid), data = subset(longmums6yr, longmums6yr$multimum == 1), family = poisson)

#View model output
summary(complexonly6_glmer)

plot_model(complexonly6_glmer,
           show.values = TRUE, 
           value.offset = .3,
           axis.lim = c(0.5,10),
           axis.title = (x = "Relative Risk"), 
           title = "Births <=6 & >=5 ComplexOnly")

#Check co-occurrence of overlap and multimum
with(longmums6yr, table(overlap, multimum))

##### NB often multimum and overlap are both present - need to separate out their effects #####
# Run model for 
# A) households with >1 mothers with a birth and no overlap
# B) households with >1 mothers with a birth and overlap

###TRY THIS: add multimum variable to longmums, not to 10/8/6yr. Then check co-ocurrence of overlap and multimum... NO: we're only interested in measures of household complexity from within the analysis period e.g. <=10 & >=5

#Check correlations between variables 
#10yr dataset: overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + multimum
names(longmums10yr)
correlations <- cor(longmums10yr[, c("overlap", "sex", "matage", "twin", "pbi", "sbi", "v136", "v149", "yob", "multimum")], use = "complete.obs")
corrplot(correlations)

#Also, check sample sizes for dataframes with multimum == 1
count(longmums10yr$multimum)
count(longmums8yr$multimum)
count(longmums6yr$multimum)

