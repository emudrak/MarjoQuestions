
library(foreign)
library(lme4)
library(lsmeans)

annuals <- read.spss("SPSS BothSitesAllYears Data File.sav",to.data.frame=TRUE)

str(annuals)
annuals$Year=as.factor(annuals$Year)

#Way to get p-values
# http://www.inside-r.org/packages/cran/lme4/docs/pvalues
#Help on restriction matrices
install.packages("pbkrtest")
library(pbkrtest)

source("Transform.R")

#Use this line to change subsets (i.e. switch to Sonoran)
subdata=subset(annuals, (Desert=='Mojave ' & TranDir=='N' & Year!=2011)) 
subdata=droplevels(subdata)
table(subdata$Erci)  #891 zeros
hist(subdata$Erci)  #891 zeros
hist(logw0(subdata$Erci))
table(subdata$Erci, subdata$Year)
boxplot(logw0(Erci)~Year*MH, data=subdata, main="Year x MH") #Could be signif
#Only 10 non-zero observations for Erci in 2010.  Do 2013 only? 

subdata=subset(annuals, (Desert=='Mojave ' & TranDir=='N' & Year==2013)) 
subdata=droplevels(subdata)
hist(subdata$Erci)  #229 zeros
hist(logw0(subdata$Erci))

### Exploratory -----
par(mfrow=c(2,3))
boxplot(logw0(Erci)~FireTrt, data=subdata, main="FireTrt")
boxplot(logw0(Erci)~RainTrt, data=subdata, main="RainTrt")
boxplot(logw0(Erci)~SeedTrt, data=subdata, main="SeedTrt")
boxplot(logw0(Erci)~TurbTrt, data=subdata, main="TurbTrt")
#boxplot(logw0(Erci)~Year, data=subdata, main="Year")
boxplot(logw0(Erci)~MH, data=subdata, main="MH")
par(mfrow=c(1,1))

#Looking for interactions- I checked all pairwise interactins, but only these seemed like a possibility of interaction---------
boxplot(logw0(Erci)~RainTrt*MH, data=subdata, main="RainTrt x MH")  #Could be signif

#Count modeling---------
#Just put in one interaction
counts2.glmer=glmer(Erci~FireTrt+RainTrt+SeedTrt+TurbTrt+MH + RainTrt:MH+(1|ShrubID)+(1|ShrubID:Plot), family="poisson", data=subdata, control=glmerControl(optimizer="bobyqa")  )
summary(counts2.glmer)
anova(counts2.glmer)
plot(counts2.glmer)

#Take out Turb and Seed
counts3.glmer=glmer(Erci~FireTrt+RainTrt+ MH + RainTrt:MH+(1|ShrubID)+(1|ShrubID:Plot), family="poisson", data=subdata, control=glmerControl(optimizer="bobyqa")  )

summary(counts3.glmer)
anova(counts3.glmer)
plot(counts3.glmer)


#Zero inflation still a problem----------------

subdata$ErciPres=subdata$Erci>0
table(subdata$ErciPres)


# Exploratory presence/absense crosstabs ------
#Looking for interactions
par(mfrow=c(2,3))
barplot(xtabs(~ErciPres+Year, data=subdata), main="Year")
barplot(xtabs(~ErciPres+RainTrt, data=subdata), main="RainTrt")
barplot(xtabs(~ErciPres+FireTrt, data=subdata), main="FireTrt")
barplot(xtabs(~ErciPres+MHcode, data=subdata), main="Microhabitat")
barplot(xtabs(~ErciPres+SeedTrt, data=subdata), main="SeedTrt")
barplot(xtabs(~ErciPres+TurbTrt, data=subdata), legend.text=c("Absent", "Present"), main="TurbTrt")
par(mfrow=c(1,1))
plot(xtabs(~MHcode+RainTrt+ErciPres, data=subdata))  #Looks signif
plot(xtabs(~MHcode+FireTrt+ErciPres, data=subdata))
plot(xtabs(~MHcode+SeedTrt+ErciPres, data=subdata))
plot(xtabs(~MHcode+TurbTrt+ErciPres, data=subdata))
#plot(xtabs(~Year+MHcode+ErciPres, data=subdata))  
plot(xtabs(~RainTrt+FireTrt+ErciPres, data=subdata))
plot(xtabs(~RainTrt+SeedTrt+ErciPres, data=subdata))
plot(xtabs(~RainTrt+TurbTrt+ErciPres, data=subdata))
#plot(xtabs(~Year+RainTrt+ErciPres, data=subdata))  #looks Signif

plot(xtabs(~FireTrt+SeedTrt+ErciPres, data=subdata))
plot(xtabs(~FireTrt+TurbTrt+ErciPres, data=subdata))
plot(xtabs(~Year+FireTrt+ErciPres, data=subdata))  

plot(xtabs(~SeedTrt+TurbTrt+ErciPres, data=subdata))
plot(xtabs(~Year+SeedTrt+ErciPres, data=subdata))  
plot(xtabs(~Year+TurbTrt+ErciPres, data=subdata))  

# Logistic modeling -----
#Note y~(X1+X2+X3+X4)^4 is shorthand for full factorial up to 4-way interactions

library(lme4)
#Note: To allow convergence, must change optimizer to bobyqa according to Ben Bolker: http://stackoverflow.com/questions/21344555/convergence-error-for-development-version-of-lme4

#All two-way effects
hurdle2.lme=glmer(ErciPres~(RainTrt+MH+TurbTrt+FireTrt+SeedTrt)^2+ (1|ShrubID)+(1|ShrubID:Plot),  data=subdata, family=binomial, control=glmerControl(optimizer="bobyqa") )
summary(hurdle2.lme)

#Only signif two ways 
hurdle3.lme=glmer(ErciPres~RainTrt+MH+TurbTrt+FireTrt+SeedTrt+MH:FireTrt+ MH:SeedTrt + (1|ShrubID)+(1|ShrubID:Plot),  data=subdata, family=binomial, control=glmerControl(optimizer="bobyqa") )
summary(hurdle3.lme)
plot(hurdle3.lme)

#Take out TurbTrt
hurdle4.lme=glmer(ErciPres~RainTrt+MH+FireTrt+SeedTrt+MH:FireTrt+ MH:SeedTrt + (1|ShrubID)+(1|ShrubID:Plot),  data=subdata, family=binomial, control=glmerControl(optimizer="bobyqa") )
summary(hurdle4.lme)
anova(hurdle4.lme)


###################

# Now model Counts of non-zero quads------------
hist(subdata[subdata$Erci>0,]$Erci)
hist(log(subdata[subdata$Erci>0,]$Erci))
par(mfrow=c(2,3))
boxplot(Erci~FireTrt, data=subdata[subdata$Erci>0,], main="FireTrt")
boxplot(Erci~RainTrt, data=subdata[subdata$Erci>0,], main="RainTrt")
boxplot(Erci~SeedTrt, data=subdata[subdata$Erci>0,], main="SeedTrt")
boxplot(Erci~TurbTrt, data=subdata[subdata$Erci>0,], main="TurbTrt")
#boxplot(Erci~Year, data=subdata[subdata$Erci>0,], main="Year")
boxplot(Erci~MH, data=subdata[subdata$Erci>0,], main="MH")
par(mfrow=c(1,1))

#Looking for interactions- I checked all pairwise interactins, but only these seemed like a possibility of interaction
par(mfrow=c(2,2))
par(mfrow=c(1,1))
boxplot(Erci~MHcode+RainTrt, data=subdata[subdata$Erci>0,])  #Looks 
boxplot(Erci~RainTrt+TurbTrt, data=subdata[subdata$Erci>0,])#Looks signif


#Fit all main effects, and two-way interactions that seem signif above
poscount.glmer=glmer(Erci~FireTrt+RainTrt+SeedTrt+TurbTrt+MH + MH:RainTrt + RainTrt:TurbTrt +(1|ShrubID)+(1|ShrubID:Plot), family="poisson",control=glmerControl(optimizer="bobyqa"), data=subdata[subdata$Erci>0,] )

summary(poscount.glmer)
anova(poscount.glmer)

#RainTrt:MH not signif
poscount2.glmer=glmer(Erci~FireTrt+RainTrt+SeedTrt+TurbTrt+MH + RainTrt:TurbTrt +(1|ShrubID)+(1|ShrubID:Plot), family="poisson",control=glmerControl(optimizer="bobyqa"), data=subdata[subdata$Erci>0,] )
summary(poscount2.glmer)
anova(poscount2.glmer)

#Take out SeedTrt
poscount3.glmer=glmer(Erci~FireTrt+RainTrt+TurbTrt+MH + RainTrt:TurbTrt +(1|ShrubID)+(1|ShrubID:Plot), family="poisson",control=glmerControl(optimizer="bobyqa"), data=subdata[subdata$Erci>0,] )
summary(poscount3.glmer)
anova(poscount3.glmer)
plot(poscount3.glmer)

lsmeans(poscount3.glmer,  ~ FireTrt+RainTrt+TurbTrt+MH)  

lsmip(poscount3.glmer, RainTrt~TurbTrt)


