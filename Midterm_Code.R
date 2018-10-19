summary(mid$PARTYID) #PartyID
  #What Are 9's and 8's? Looking at codebook, I should eliminate these from the analysis as leavign them in would skew my findings
  #and give a false representation of people's party identification.
  mid$PARTYID<-ifelse(mid$PARTYID==9 | mid$PARTYID==8, NA, mid$PARTYID)
  summary(mid$PARTYID)

#Everyone in Sample
summary(mid$AGE) #Age
  #What are 99's and 98's?
  mid$AGE<-ifelse(mid$AGE==99 | mid$AGE==98, NA, mid$AGE)
  summary(mid$AGE)

summary(mid$EDUC) #Education
  #What are 97-99?
  mid$EDUC<-ifelse(mid$EDUC==99 | mid$EDUC==98 | mid$EDUC==97, NA, mid$EDUC)
  summary(mid$EDUC)  

#Break Up Data To Analyze By Race, Sex
  wh<-subset(mid, mid$RACE==1)
  b<-subset(mid, mid$RACE==2)
  o<-subset(mid, mid$RACE==3)
  m<-subset(mid, mid$SEX==1)
  f<-subset(mid, mid$SEX==2)

#For Whites
  summary(wh$AGE)
  summary(wh$EDUC)
#For Blacks
  summary(b$AGE)
  summary(b$EDUC)
#For Other Race
  summary(o$AGE)
  summary(o$EDUC)
#For Males
  summary(m$AGE)
  summary(m$EDUC)  
#For Females
  summary(f$AGE)
  summary(f$EDUC)  
  
