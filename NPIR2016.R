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
set_dhs_credentials(credentials="credentials.txt")

#download Nepal 2016 survey IR data - NPIR7HFL.ZIP - RECODE VII (NB VII manual not yet available,
#so use VI for time being...)
downloads <- get_datasets("NPIR7HFL.ZIP")
str(downloads)

#read in the dataset
npir2016 <- readRDS(downloads$NPIR7HFL)

#Create labels for variables
names(npir2016)[3] <- "cluster"
names(npir2016)[4] <- "household"
names(npir2016)[5] <- "respline"
names(npir2016)[15] <- "age"
names(npir2016)[547] <- "children ever born"
names(npir2016)[564] <- "total living children"
names(npir2016)[73] <- "women in household"
names(npir2016)[558] <- "age at first birth"
names(npir2016)[78] <- "relationship to HoH"

#Create labels for child mortality variables
names(npir2016)[552] <- "sons died" 
names(npir2016)[553] <- "daughters died"

#Create labels for birth history variables (NB current data file max no. children = 15)
names(npir2016)[107] <- "1st birth"
names(npir2016)[187] <- "year of 1st birth"
names(npir2016)[247] <- "1st child alive?"
  
names(npir2016)[108] <- "2nd birth"
names(npir2016)[188] <- "year of 2nd birth"
names(npir2016)[248] <- "2nd child alive?"
  
names(npir2016)[109] <- "3rd birth"
names(npir2016)[189] <- "year of 3rd birth"
names(npir2016)[249] <- "3rd child alive?"
  
names(npir2016)[110] <- "4th birth"
names(npir2016)[190] <- "year of 4th birth"
names(npir2016)[250] <- "4th child alive?"
  
names(npir2016)[111] <- "5th birth"
names(npir2016)[191] <- "year of 5th birth"
names(npir2016)[251] <- "5th child alive?"
  
names(npir2016)[112] <- "6th birth"
names(npir2016)[192] <- "year of 6th birth"
names(npir2016)[252] <- "6th child alive?"
  
names(npir2016)[113] <- "7th birth"
names(npir2016)[193] <- "year of 7th birth"
names(npir2016)[253] <- "7th child alive?"
  
names(npir2016)[114] <- "8th birth"
names(npir2016)[194] <- "year of 8th birth"
names(npir2016)[254] <- "8th child alive?"
  
names(npir2016)[115] <- "9th birth"
names(npir2016)[195] <- "year of 9th birth"
names(npir2016)[255] <- "9th child alive?"
  
names(npir2016)[116] <- "10th birth"
names(npir2016)[196] <- "year of 10th birth"
names(npir2016)[256] <- "10th child alive?"
  
names(npir2016)[117] <- "11th birth"
names(npir2016)[197] <- "year of 11th birth"
names(npir2016)[257] <- "11th child alive?"
  
names(npir2016)[118] <- "12th birth"
names(npir2016)[198] <- "year of 12th birth"
names(npir2016)[258] <- "12th child alive?"
  
names(npir2016)[119] <- "13th birth"
names(npir2016)[199] <- "year of 13th birth"
names(npir2016)[259] <- "13th child alive?"
  
names(npir2016)[120] <- "14th birth"
names(npir2016)[200] <- "year of 14th birth"
names(npir2016)[260] <- "14th child alive?"
  
names(npir2016)[121] <- "15th birth"
names(npir2016)[201] <- "year of 15th birth"
names(npir2016)[261] <- "15th child alive?"
  
#Select variables
varselect <- c("caseid", "cluster", "household", "respline",  "age", "relationship to HoH", 
               "women in household","children ever born", "total living children", "sons died", 
               "daughters died", "age at first birth", "1st birth", "year of 1st birth", "1st child alive?", 
               "2nd birth", "year of 2nd birth", "2nd child alive?", "3rd birth", "year of 3rd birth", 
               "3rd child alive?", "4th birth", "year of 4th birth", "4th child alive?", "5th birth", 
               "year of 5th birth", "5th child alive?", "6th birth", "year of 6th birth", 
               "6th child alive?", "7th birth", "year of 7th birth", "7th child alive?",  "8th birth", 
               "year of 8th birth", "8th child alive?", "9th birth", "year of 9th birth", 
               "9th child alive?", "10th birth", "year of 10th birth", "10th child alive?", 
               "11th birth", "year of 11th birth", "11th child alive?", "12th birth", 
               "year of 12th birth", "12th child alive?", "13th birth", "year of 13th birth", 
               "13th child alive?", "14th birth", "year of 14th birth", "14th child alive?", 
               "15th birth", "year of 15th birth", "15th child alive?")
hhold <- npir2016[varselect]

#Remove all whitespaces in caseid? Actually quite useful to see cluster household respline...
#hhold$caseid <- gsub(" ", "", hhold$caseid)

#Select observations with >0 children ever born
mums <- subset(hhold, hhold$"1st birth" > 0)

#find duplicates in cluster & household columns (i.e. households with multiple mothers)
twomum <- mums[(duplicated(mums[c("cluster","household")]) |
                     duplicated(mums[c("cluster","household")], fromLast = TRUE)), ]

#Max no. children ever born is now 11, so drop vars for births 12-15
names(twomum)
twomum <- subset(twomum, select = -c(46:57))

#year of birth is in Nepali calendar format! How to convert?
#Nepali calendar year is roughly equivalent to Gregorian year - 57 (check this...)
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
twomum$"year of 11th birth" <- (twomum$"year of 11th birth")-57

#Create column for total no. of children who died
twomum$"total child deaths" <- twomum$"sons died" + twomum$"daughters died"

#Change the order of the columns
twomum <- twomum[,c(1:11,46,12:45)]

#Check order of columns
names(twomum)

#See DHS VI Recode Map to interpret relationship to HoH codes: https://bit.ly/2Ig5bLe
# (NB VII NOT YET AVAILABLE)

#Relabel HoH codes to:
# 1 Head
# 2 Wife or husband
# 3 Son/daughter
# 4 Son/daughter-in-law
# 5 Grandchild
# 6 Parent
# 7 Parent-in-law
# 8 Brother/sister
# 9 Co-spouse
# 10 Other relative
# 11 Adopted/foster child
# 12 Not related
# 13 Niece/nephew by blood
# 14 Niece/nephew by marriage
# 98 Don't know 
# 99 Missing

twomum$`relationship to HoH` <- factor(twomum$`relationship to HoH`,
                                       levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                                                  13, 14, 15),
                                       labels = c("head", "wife", "daughter", "daughter-in-law",
                                                  "granddaughter", "mother", "mother-in-law", 
                                                  "sister", "co-wife", "other relative", 
                                                  "adopted daughter", "not related", 
                                                  "niece by (blood)", "niece (by marriage)",
                                                  "missing"))

#Save twomum as csv file
#write.csv(twomum, "npir2016.csv")