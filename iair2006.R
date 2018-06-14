### Extradata ###

setwd("~/Desktop/Rwd/Data")

#Install packages
install.packages("devtools")
devtools::install_github("OJWatson/rdhs")
library(rdhs)

#create credentials.txt in directory, showing:
#email="myemail"
#password="mypassword"
#project="projectname"

#set credentials
set_dhs_credentials(credentials="/Users/dougiefoster/Desktop/credentials.txt")

#download ***INDIA 2005-06*** survey IR data - IAIR52FL.ZIP
downloads <- get_datasets("IAIR52FL.ZIP")
str(downloads)

## "Zip" function not working...

#Put file in Rwd, then read in the dataset straight from computer
library(foreign)
iair2006 <- read.dta("/Users/dougiefoster/Desktop/Rwd/Data/IAIR52FL.dta")

#look at variable names
names(iair2006)


################# FIND AND RECODE VARIABLES FOR DHS RECODE V ##################
#Create labels for variables 
names(iair2006)[3] <- "cluster"
names(iair2006)[4] <- "household"
names(iair2006)[5] <- "respline"
names(iair2006)[14] <- "age"
names(iair2006)[458] <- "daughters at home"
names(iair2006)[73] <- "total living children"
names(iair2006)[69] <- "women in household"
names(iair2006)[467] <- "age at first birth"
names(iair2006)[74] <- "relationship to HoH"

#See DHS map to interpret rel to HoH codes: https://bit.ly/2Ig5bLe

#Create labels for child mortality variables
names(iair2006)[461] <- "sons died" 
names(iair2006)[462] <- "daughters died"

#Create labels for birth history variables (NB twomum data file max no. children = 9)
names(iair2006)[96] <- "1st birth"
names(iair2006)[176] <- "year of 1st birth"

names(iair2006)[97] <- "2nd birth"
names(iair2006)[177] <- "year of 2nd birth"

names(iair2006)[98] <- "3rd birth"
names(iair2006)[178] <- "year of 3rd birth"

names(iair2006)[99] <- "4th birth"
names(iair2006)[179] <- "year of 4th birth"

names(iair2006)[100] <- "5th birth"
names(iair2006)[180] <- "year of 5th birth"

names(iair2006)[101] <- "6th birth"
names(iair2006)[181] <- "year of 6th birth"

names(iair2006)[102] <- "7th birth"
names(iair2006)[182] <- "year of 7th birth"

names(iair2006)[103] <- "8th birth"
names(iair2006)[183] <- "year of 8th birth"

names(iair2006)[104] <- "9th birth"
names(iair2006)[184] <- "year of 9th birth"

#names(iair2006)[105] <- "10th birth"
#names(iair2006)[185] <- "year of 10th birth"

#Select variables
varselect <- c("caseid", "cluster", "household", "respline",  "age", "relationship to HoH", "daughters at home", 
               "women in household", "total living children", "sons died", "daughters died", 
               "age at first birth", "1st birth", "year of 1st birth", "2nd birth", 
               "year of 2nd birth", "3rd birth", "year of 3rd birth", "4th birth", 
               "year of 4th birth", "5th birth", "year of 5th birth", "6th birth", 
               "year of 6th birth", "7th birth", "year of 7th birth", "8th birth", 
               "year of 8th birth", "9th birth", "year of 9th birth")
hhold <- iair2006[varselect]

#Remove all whitespaces in caseid? Actually quite useful to see cluster household respline...
hhold$caseid <- gsub(" ", "", hhold$caseid)

#Select observations with >0 daughters at home
#dshome <- hhold[ which(hhold$"daughters at home">0 ) ,]
#Is >0 daughters at home best function to use?

#find duplicates in cluster&household columns (i.e. women in the same household)
twomum <- hhold[(duplicated(hhold[c("cluster","household")]) |
                   duplicated(hhold[c("cluster","household")], fromLast = TRUE)), ]

#Create column for total no. of children who died
twomum$"total child deaths" <- twomum$"sons died" + twomum$"daughters died"

#Change the order of the columns
twomum <- twomum[,c(1:11,31,12:30)]

#This gives us 124,380 cases...
#But fewer than half of these have daughters at home...

