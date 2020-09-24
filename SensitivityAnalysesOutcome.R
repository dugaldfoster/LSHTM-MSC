# Run the models with outcomes as deaths age <=1 and <=2 to see whether results hold 

### <= 2
#Remove cases with death over the age of 2
detach("package:dplyr", unload = TRUE)
count(longmums$agedeath)
MortalityBy2 <- subset(longmums, longmums$agedeath <= 24 | is.na(longmums$agedeath))

#Restrict cases to those born >= 2 years before survey date to account for censoring
MortalityBy2 <- subset(longmums, longmums$yob <= longmums$v007 - 2)

#Create column called "overlap" which returns TRUE for cases that were born <=2 years
#of other cases in the same household, but which had different mothers
MortalityBy2 <- mutate(group_by(MortalityBy2, v000, v001, v002),
                   overlap = mapply(function(x,y) {any(abs(x-yob)<=2 & y!=v003)}, yob, v003))

#Change TRUE/FALSE to 1/0
MortalityBy2 <- MortalityBy2 %>% mutate(overlap = as.numeric(overlap))

#Edit "twin" variable to reflect whether birth was part of a multiple birth (0 no, 1 yes)
MortalityBy2$twin <- ifelse(MortalityBy2$twin > 0, 1, 0)


### <= 1
#Remove cases with death over the age of 1
detach("package:dplyr", unload = TRUE)
count(longmums$agedeath)
MortalityBy1 <- subset(longmums, longmums$agedeath <= 12 | is.na(longmums$agedeath))

#Restrict cases to those born >= 1 year before survey date to account for censoring
MortalityBy1 <- subset(longmums, longmums$yob <= longmums$v007 - 1)

#Create column called "overlap" which returns TRUE for cases that were born <=2 years
#of other cases in the same household, but which had different mothers
MortalityBy1 <- mutate(group_by(MortalityBy1, v000, v001, v002),
                   overlap = mapply(function(x,y) {any(abs(x-yob)<=2 & y!=v003)}, yob, v003))

#Change TRUE/FALSE to 1/0
MortalityBy1 <- MortalityBy1 %>% mutate(overlap = as.numeric(overlap))

#Edit "twin" variable to reflect whether birth was part of a multiple birth (0 no, 1 yes)
MortalityBy1$twin <- ifelse(MortalityBy1$twin > 0, 1, 0)


### Run models

#Original model applied to MortalityBy2 data
By2_glmer <- glmer(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + (1 | v002/caseid), data = MortalityBy2, family = poisson)

plot_model(By2_glmer,
           show.values = TRUE, 
           value.offset = .3,
           axis.title = (x = "Relative Risk"), 
           title = "Births >= 2 years before survey date")

#Original model applied to MortalityBy1 data
By1_glmer <- glmer(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + (1 | v002/caseid), data = MortalityBy1, family = poisson)

plot_model(By1_glmer,
           show.values = TRUE, 
           value.offset = .3,
           axis.title = (x = "Relative Risk"), 
           title = "Births >= 1 years before survey date")

### Add data restrictions to minimise risk of changes in household composition since survey
MortalityBy2_5yr <- subset(MortalityBy2, MortalityBy2$yob >= MortalityBy2$v007 - 5)

By2_5yr_glmer <- glmer(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + (1 | v002/caseid), data = MortalityBy2_5yr, family = poisson)

plot_model(By2_5yr_glmer,
           show.values = TRUE, 
           value.offset = .3,
           axis.title = (x = "Relative Risk"), 
           title = "Births <=5 & >= 2 years before survey date")

MortalityBy1_5yr <- subset(MortalityBy1, MortalityBy1$yob >= MortalityBy1$v007 - 5)

By1_5yr_glmer <- glmer(mortality ~ overlap + sex + matage + twin + pbi + sbi + v136 + v149 + yob + (1 | v002/caseid), data = MortalityBy1_5yr, family = poisson)

plot_model(By1_5yr_glmer,
           show.values = TRUE, 
           value.offset = .3,
           axis.title = (x = "Relative Risk"), 
           title = "Births <=5 & >= 1 years before survey date")



