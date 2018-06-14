### SUMMER PROJECT ###

setwd("~/Desktop/Rwd")

#Install packages
install.packages("devtools")
devtools::install_github("OJWatson/rdhs")
library(rdhs)

#create credentials.txt in directory, showing:
#email="myemail"
#password="mypassword"
#project="projectname"

#set credentials
set_dhs_credentials(credentials="~Desktop/Rwd/credentials.txt")
set_dhs_credentials(credentials="credentials.txt")

#download Nepal 2016 survey IR data - NPIR7HFL.ZIP
downloads <- get_datasets("NPIR7HFL.ZIP")
str(downloads)

#read in the dataset
npir2016 <- readRDS(downloads$NPIR7HFL)

#look at variable names
head(get_var_labels(npir2016))

#Create labels for variables
names(npir2016)[3] <- "cluster"
names(npir2016)[4] <- "household"
names(npir2016)[5] <- "respline"
names(npir2016)[15] <- "age"
names(npir2016)[549] <- "daughters at home"
names(npir2016)[564] <- "total living children"
names(npir2016)[73] <- "women in household"
names(npir2016)[558] <- "age at first birth"
names(npir2016)[78] <- "relationship to HoH"

#See DHS map to interpret rel to HoH codes: https://bit.ly/2Ig5bLe

#Create labels for child mortality variables
names(npir2016)[552] <- "sons died" 
names(npir2016)[553] <- "daughters died"

#Create labels for birth history variables (NB current data file max no. children = 10)
names(npir2016)[107] <- "1st birth"
names(npir2016)[187] <- "year of 1st birth"

names(npir2016)[108] <- "2nd birth"
names(npir2016)[188] <- "year of 2nd birth"

names(npir2016)[109] <- "3rd birth"
names(npir2016)[189] <- "year of 3rd birth"

names(npir2016)[110] <- "4th birth"
names(npir2016)[190] <- "year of 4th birth"

names(npir2016)[111] <- "5th birth"
names(npir2016)[191] <- "year of 5th birth"

names(npir2016)[112] <- "6th birth"
names(npir2016)[192] <- "year of 6th birth"

names(npir2016)[113] <- "7th birth"
names(npir2016)[193] <- "year of 7th birth"

names(npir2016)[114] <- "8th birth"
names(npir2016)[194] <- "year of 8th birth"

names(npir2016)[115] <- "9th birth"
names(npir2016)[195] <- "year of 9th birth"

names(npir2016)[116] <- "10th birth"
names(npir2016)[196] <- "year of 10th birth"

#Select variables
varselect <- c("caseid", "cluster", "household", "respline",  "age", "relationship to HoH", "daughters at home", 
               "women in household", "total living children", "sons died", "daughters died", 
               "age at first birth", "1st birth", "year of 1st birth", "2nd birth", 
               "year of 2nd birth", "3rd birth", "year of 3rd birth", "4th birth", 
               "year of 4th birth", "5th birth", "year of 5th birth", "6th birth", 
               "year of 6th birth", "7th birth", "year of 7th birth", "8th birth", 
               "year of 8th birth", "9th birth", "year of 9th birth", "10th birth", 
               "year of 10th birth")
hhold <- npir2016[varselect]

#Remove all whitespaces in caseid? Actually quite useful to see cluster household respline...
#hhold$caseid <- gsub(" ", "", hhold$caseid)

#Select observations with >0 daughters at home
dshome <- hhold[ which(hhold$"daughters at home">0 ) ,]
#Is >0 daughters at home best function to use? For now...

#find duplicates in cluster&household columns (i.e. women in the same household)
twomum <- dshome[(duplicated(dshome[c("cluster","household")]) |
                    duplicated(dshome[c("cluster","household")], fromLast = TRUE)), ]

#year of birth is in Nepali calendar format! How to convert?
#Nepali calendar year is roughly equivalent to Gregorian year + 57 (check this...)
twomum$"year of 1st birth" <- (twomum$"year of 1st birth")-57
twomum$"year of 2nd birth" <- (twomum$"year of 2nd birth")-57
twomum$"year of 3rd birth" <- (twomum$"year of 3rd birth")-57
twomum$"year of 4th birth" <- (twomum$"year of 4th birth")-57
twomum$"year of 5th birth" <- (twomum$"year of 5th birth")-57
twomum$"year of 6th birth" <- (twomum$"year of 6th birth")-57
twomum$"year of 7th birth" <- (twomum$"year of 7th birth")-57
twomum$"year of 8th birth" <- (twomum$"year of 8th birth")-57
twomum$"year of 9th birth" <- (twomum$"year of 9th birth")-57
twomum$"year of 10th birth" <- (twomum$"year of 10th birth")-57

#Create column for total no. of children who died
twomum$"total child deaths" <- twomum$"sons died" + twomum$"daughters died"

#Change the order of the columns
twomum <- twomum[,c(1:11,33,12:32)]

#Check order of columns
names(twomum)

#This gives us 12,716 cases... How to narrow data down further?
#Find cases within households that overlap in any birth by ≤ 2 years...
#Apply to other DHS datasets- check recode files for diff variable codes!

##### 
#Histogram plots/X2 test demos/linear regression

#plot histogram of total children ever born
hist(npir$v201)

#plot histogram of age at first birth
hist(npir$v212)

#plot histogram of number of eligible women in household
hist(npir$v138)

#plot scatterplot of Age at First Birth vs Children Ever Born
attach(npir)
plot(v212, v201, col = "blue", cex = 0.4, main="CEB vs AFB Scatterplot",
     xlab="AFB ", ylab="CEB ", pch=19) 
abline(lm(v212 ~ v201))

##Practice analyses in R##

#X2 test for difference between v212 and v201
chisq.test(npir$v212, npir$v201, correct=FALSE)

#Run linear regression on AFB and CEB
lm(v212 ~ v201)




