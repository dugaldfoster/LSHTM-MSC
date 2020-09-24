#rerun models for subsample born <=10/8/6 & >=5 years before survey date
#Original model
rrpreregmodel_glmer <- glmer(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + (1 | v002/caseid), data = longmums, family = poisson)

tab_model(preregmodel_glmer)

plot_model(rrpreregmodel_glmer,
           show.values = TRUE, 
           value.offset = .3,
           axis.title = (x = "Relative Risk"), 
           title = "Births >= 5 years before survey date")

# *************** 10 YEAR  *************** #

#Create dataframe with cases restricted to those born <= 10 years before survey date to minimise risk of changes in household composition since survey
longmums10yr <- subset(longmums, longmums$yob >= longmums$v007 - 10)

#Fit model
longmums10yr_glmer <- glmer(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + (1 | v002/caseid), data = longmums10yr, family = poisson)

#View model output
summary(longmums10yr_glmer)

tab_model(longmums10yr_glmer)

plot_model(longmums10yr_glmer,
           show.values = TRUE, 
           value.offset = .3,
           axis.lim = c(0.5,10),
           axis.title = (x = "Relative Risk"), 
           title = "Births <=10 & >=5 years before survey date")

# *************** 8 YEAR  *************** #
#Create dataframe with cases restricted to those born <= 8 years before survey date to minimise risk of changes in household composition since survey
longmums8yr <- subset(longmums, longmums$yob >= longmums$v007 - 8)

#Fit model
longmums8yr_glmer <- glmer(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + (1 | v002/caseid), data = longmums8yr, family = poisson)

#View model output
summary(longmums8yr_glmer)

plot_model(longmums8yr_glmer,
           show.values = TRUE, 
           value.offset = .3,
           axis.lim = c(0.5,10),
           axis.title = (x = "Relative Risk"), 
           title = "Births <=8 & >=5 years before survey date")

# *************** 6 YEAR  *************** #
#Create dataframe with cases restricted to those born <= 6 years before survey date to minimise risk of changes in household composition since survey
longmums6yr <- subset(longmums, longmums$yob >= longmums$v007 - 6)

#Fit model
longmums6yr_glmer <- glmer(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + (1 | v002/caseid), data = longmums6yr, family = poisson)

#View model output
summary(longmums6yr_glmer)

tab_model(longmums6yr_glmer)

plot_model(longmums6yr_glmer,
           show.values = TRUE, 
           value.offset = .3,
           axis.lim = c(0,50),
           axis.title = (x = "Relative Risk"), 
           title = "Births <=6 & >=5 years before survey date")

##### Plot odds ratios for all models #####
# See https://strengejacke.github.io/sjPlot/reference/plot_models.html
library(sjPlot)
plot_model(longmums6yr_glmer, show.values = TRUE, value.offset = .3)

plot_model(rrlongmums6yr_glmer, show.values = TRUE, value.offset = .3)

all.models <- list()
all.models[[1]] <- preregmodel_glmer
all.models[[2]] <- longmums6yr_glmer
plot_models(all.models)

plot_models(preregmodel_glmer, longmums6yr_glmer)
#ERROR#

##

