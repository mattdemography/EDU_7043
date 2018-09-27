#Bring In Data
prb<-read.csv("https://raw.githubusercontent.com/coreysparks/data/master/PRB2008_All.csv", stringsAsFactors = F)

#Look at my data
names(prb)
str(prb)

#I'm interested in Per Capita HIV/AIDS for 15-49 year olds in 2001 and 2007. This is a long variable name so I want to change it.
library(plyr)
prb<-rename(prb, c(PercPop1549HIVAIDS2001="hiv01", PercPop1549HIVAIDS2007="hiv07"))

#See if changes came through
names(prb)

#Let's look more at these variables. Are there any na's? #How many?
table(is.na(prb$hiv01))
table(is.na(prb$hiv07))

#Why are there so many missing values?
#What is the central tendency of these variables?
summary(prb$hiv01)
summary(prb$hiv07)

#Are these variables normally distributed? What tools do we have to begin to answer this question?

#I'm Making a Fake Data Set that appoximates normality
ex<-rnorm(10000, mean=0, sd=1)
summary(ex)
h<-hist(ex)
xfit<-seq(min(ex), max(ex), length=40)
yfit<-dnorm(xfit, mean=mean(ex), sd=sd(ex))
yfit<-yfit*diff(h$mids[1:2])*length(ex)
lines(xfit, yfit)

#Let's Look at our two variables. I'm creating a function for the steps above. You don't have to do this, but it can be helpful.
Dist_lines<-function(x){
  h<-hist(x)
  xfit<-seq(min(x), max(x), length=40)
  yfit<-dnorm(xfit, mean=mean(x), sd=sd(x))
  yfit<-yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit)
  
}

#Test the Function
Dist_lines(ex)

#Function that I created does not like NAs, so let's remove them and create a new object
hiv01<-subset(prb, !is.na(prb$hiv01))
Dist_lines(hiv01$hiv01)

hiv07<-subset(prb, !is.na(prb$hiv07))
Dist_lines(hiv07$hiv07)

#Another way to think about normality
#Q-Q Plots - Let's try our fake data first
qqnorm(ex); qqline(ex)

#Now let's try our real data
qqnorm(hiv01$hiv01); qqline(hiv01$hiv01)

