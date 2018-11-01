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

summary(dat$AGE) #Age
  #What are 99's and 98's?
  dat$AGE<-ifelse(dat$AGE==99 | dat$AGE==98, NA, dat$AGE)
  summary(dat$AGE)

summary(dat$EDUC) #Education
  #What are 97-99?
  dat$EDUC<-ifelse(dat$EDUC==99 | dat$EDUC==98 | dat$EDUC==97, NA, dat$EDUC)
  summary(dat$EDUC) 

summary(dat$SEX)

#Break Up Data To Analyze By Race, Sex
  wh<-subset(dat, dat$RACE==1)
  nw<-subset(dat, dat$RACE==2 | dat$RACE==3)
  b<-subset(dat, dat$RACE==2)
  o<-subset(dat, dat$RACE==3)
  m<-subset(dat, dat$SEX==1)
  f<-subset(dat, dat$SEX==2)

#Difference in means of education?
  summary(m$EDUC)
  summary(f$EDUC)
  boxplot(dat$EDUC ~dat$SEX) #Boxplot by Group
  
  #Does there seem to be a difference? How can we know for statistically sure?
  t.test(m$EDUC, f$EDUC)

  #What about for Race - white/non-white?
  summary(wh$EDUC)
  summary(nw$EDUC)  
  boxplot(dat$EDUC ~dat$RACE)  
  
  t.test(wh$EDUC, nw$EDUC)
  t.test(dat$EDUC ~dat$SEX)
  #Use t.test(wh$EDUC, nw$EDUC, var.equal=T) to assume equal variance.
  
#Does education meet the assumptions of a T-Test?
  hist(wh$EDUC)
  qqnorm(wh$EDUC); qqline(wh$EDUC)  
  
  hist(nw$EDUC)
  qqnorm(nw$EDUC); qqline(nw$EDUC)  
  
  #Maybe a non-parametric test is best to estimate difference in means
  wilcox.test(wh$EDUC, nw$EDUC)
  
  #For Sex you could also do this code for the Wilcox test because we have them in the same dataset
  wilcox.test(dat$EDUC ~dat$SEX)
  wilcox.test(m$EDUC, f$EDUC)
  
#Let's take a look at ANOVA. Given that our original race variable has three categories and
  #education is continuous, this works for ANOVA
  fit<-aov(dat$EDUC ~ as.factor(dat$RACE)) #Education is our dependent variable
  summary(fit)  #Let's look at the results
  
  #Where are these differences?
  TukeyHSD(fit)
  

  