
library(foreign)  # To get SPSS file
library(lme4)     # For random effects
library(lsmeans)  # To summarized expected values by group
library(pbkrtest) # Way to get p-values
                  # http://www.inside-r.org/packages/cran/lme4/docs/pvalues
                  # What is restriction matrix? http://people.su.se/~palme/Rmatr.pdf 

source("Transform.R")

annuals <- read.spss("SPSS BothSitesAllYears Data File.sav",to.data.frame=TRUE)
str(annuals)
annuals$Year=as.factor(annuals$Year)

#Model options- Don't put in Precipitation Amounts (WintRain), but include Year as a fixed effect.  This will allow for other weather variation rather than just precip. 

#Reponse variables:  Biomass, Counts of target species, Cover of target species.
   
####  Cover -----

#Use this line to change subsets (i.e. switch to Sonoran)
subdata=subset(annuals, (Desert=='Mojave ' & TranDir=='N' & Year==2013)) 
subdata=droplevels(subdata)


## Explain why we're doing specieal logw0 Transformations...
hist(subdata$EroCover)
hist(logit.transform(subdata$EroCover/100))
#Still zero inflated
table(subdata$EstBiomass) #755 zeros
table(subdata$EroCover>0, subdata$Erci>0)
#only one instance of 0%cover for nonzero Erci count

#- Stick to same hurdle model


############ Now model cover of non-zero quads
hist(subdata[subdata$EroCover>0,]$EroCover)
hist(logit.transform(subdata[subdata$EroCover>0,]$EroCover/100))
#Make logit transformed variable
subdata$LogitEroCov=logit.transform(subdata$EroCover/100)
table(subdata$EroCover>0, subdata$Erci>0, subdata$RainTrt)

par(mfrow=c(2,3))
boxplot(LogitEroCov~FireTrt, data=subdata[subdata$EroCover>0,], main="FireTrt")
boxplot(LogitEroCov~RainTrt, data=subdata[subdata$EroCover>0,], main="RainTrt")
boxplot(LogitEroCov~SeedTrt, data=subdata[subdata$EroCover>0,], main="SeedTrt")
boxplot(LogitEroCov~TurbTrt, data=subdata[subdata$EroCover>0,], main="TurbTrt")
#boxplot(EroCover~Year, data=subdata[subdata$EroCover>0,], main="Year")
boxplot(LogitEroCov~MH, data=subdata[subdata$EroCover>0,], main="MH")
par(mfrow=c(1,1))

#Looking for interactions- I checked all pairwise interactins, but none seemed like a possibility of interaction
par(mfrow=c(2,2))
par(mfrow=c(1,1))
boxplot(LogitEroCov~FireTrt+RainTrt,data=subdata[subdata$EroCover>0,])  
boxplot(LogitEroCov~FireTrt+SeedTrt,data=subdata[subdata$EroCover>0,]) 
boxplot(LogitEroCov~FireTrt+TurbTrt,data=subdata[subdata$EroCover>0,]) 
boxplot(LogitEroCov~FireTrt+MH, data=subdata[subdata$EroCover>0,])   
boxplot(LogitEroCov~RainTrt+SeedTrt,data=subdata[subdata$EroCover>0,]) 
boxplot(LogitEroCov~RainTrt+TurbTrt,data=subdata[subdata$EroCover>0,])  
boxplot(LogitEroCov~RainTrt+MH, data=subdata[subdata$EroCover>0,])  
boxplot(LogitEroCov~SeedTrt+TurbTrt,data=subdata[subdata$EroCover>0,])   
boxplot(LogitEroCov~SeedTrt+MH, data=subdata[subdata$EroCover>0,])  
boxplot(LogitEroCov~TurbTrt+MH, data=subdata[subdata$EroCover>0,])  


#Fit all main effects only - include term (1|ShrubID:Plot) only if there are two or more years
# poscov.lmer=lmer(LogitEroCov~FireTrt+RainTrt+SeedTrt+TurbTrt+MH +(1|ShrubID)+(1|ShrubID:Plot),control=lmerControl(optimizer="bobyqa"), data=subdata[subdata$EroCover>0,] )


#Fit all main effects only
poscov.lmer=lmer(LogitEroCov~FireTrt+RainTrt+SeedTrt+TurbTrt+MH +(1|ShrubID),control=lmerControl(optimizer="bobyqa"), data=subdata[subdata$EroCover>0,] )



summary(poscov.lmer)
anova(poscov.lmer)


lsmeans(poscount3.glmer,  ~ FireTrt+RainTrt+TurbTrt+MH)  

lsmip(poscount3.glmer, RainTrt~TurbTrt)

