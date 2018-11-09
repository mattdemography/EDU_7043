#Bring in Data
dat<-read.csv(file = "https://raw.githubusercontent.com/mattdemography/EDU_7043/master/Data/GSS_2016_AA.csv", stringsAsFactors=F)
names(dat)
str(dat)

#Let's Make some changes in the following variables
summary(dat$PARTYID) #PartyID
#What Are 9's and 8's? Looking at codebook, I should eliminate these from the analysis as leavign them in would skew my findings
#and give a false representation of people's party identification.
dat$PARTYID<-ifelse(dat$PARTYID==9 | dat$PARTYID==8, NA, dat$PARTYID)
summary(dat$PARTYID)

#Let me See how Party Identification Varies by Race, Sex, and Education.
dat$dem<-ifelse(dat$PARTYID==0 | dat$PARTYID==1, 1, 0)
dat$ind<-ifelse(dat$PARTYID==2 | dat$PARTYID==3 | dat$PARTYID==4 | dat$PARTYID==7, 1, 0)
dat$rep<-ifelse(dat$PARTYID==5 | dat$PARTYID==6, 1, 0)

#Check the Data
table(dat$dem)
table(dat$ind)
table(dat$rep)

dat$newpoly<-ifelse(dat$dem==1, 1, ifelse(dat$ind, 2, ifelse(dat$rep, 3, 0)))
table(dat$newpoly)

summary(dat$EDUC) #Education
#What are 97-99?
dat$EDUC<-ifelse(dat$EDUC==99 | dat$EDUC==98 | dat$EDUC==97, NA, dat$EDUC)
summary(dat$EDUC) 

#Make Education a Categorical Variable
dat$nohs<-ifelse(dat$EDUC<=11, 1, 0)
table(dat$nohs)
head(dat[5:10,c(33,47)]) #Check Data

dat$high<-ifelse(dat$EDUC==12, 1, 0)
table(dat$high)

dat$scol<-ifelse(dat$EDUC>=13 & dat$EDUC<=15, 1, 0)
table(dat$scol)
head(dat[5:10,c(33,47:49)]) #Check Data

dat$col<-ifelse(dat$EDUC>=16, 1, 0)
table(dat$col)
head(dat[5:10,c(33,47:50)]) #Check Data

#Look at Sex variable
summary(dat$SEX)

#Create Contingency Table
table(dat$SEX, dat$newpoly)
ftable(dat$SEX, dat$newpoly)

#Cross Tabulation
library(gmodels)
CrossTable(dat$SEX, dat$newpoly, prop.chisq =F)
CrossTable(dat$SEX, dat$newpoly, expected=T)

chisq.test(df$SEX, df$newpoly)
chisq.test(df$RACE, df$newpoly)
