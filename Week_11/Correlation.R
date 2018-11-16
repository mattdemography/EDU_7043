#Bring in Data
dat<-read.csv(file = "https://raw.githubusercontent.com/mattdemography/EDU_7043/master/Data/GSS_2016_AA.csv", stringsAsFactors=F)
names(dat)
str(dat)

#Clean age, childs, and divorce
summary(dat$AGE)
dat$AGE<-ifelse(dat$AGE==99 | dat$AGE==98, NA, dat$AGE)
summary(dat$AGE)

summary(dat$CHILDS)
dat$CHILDS<-ifelse(dat$CHILDS==9, NA, dat$CHILDS)
summary(dat$CHILDS)

summary(dat$DIVORCE)
dat$DIVORCE<-ifelse(dat$DIVORCE>7 | dat$DIVORCE==0, NA, dat$DIVORCE)
summary(dat$DIVORCE)

dat$DIVORCE<-ifelse(dat$DIVORCE==2, 0, dat$DIVORCE)
summary(dat$DIVORCE)

dat2<-subset(dat, !is.na(dat$AGE) & !is.na(dat$CHILDS)) #Take out NA's

cor(dat2$CHILDS, dat2$AGE, method="pearson")
cor.test(dat2$CHILDS, dat2$AGE)

cor(dat2$CHILDS, dat2$AGE, method="spearman")

names(dat2)
cor(dat2[3:9])
