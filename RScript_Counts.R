
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

#Use this line to change subsets (i.e. switch to Sonoran)
subdata=subset(annuals, (Desert=='Mojave ' & TranDir=='N' & Year!=2011)) 
table(subdata$Erci)  #891 zeros
hist(log(subdata$Erci))
hist(logw0(subdata$Erci))
table(logw0(subdata$Erci))
biomass.glmer=glmer(Erci~(FireTrt+RainTrt+SeedTrt+TurbTrt+Year+MH)^2+(1|ShrubID)+(1|ShrubID:Plot), family="poisson", data=subdata )

summary(biomass.glmer)
anova(biomass.glmer)



lsmeans(biomass.lmer6,  ~ FireTrt:RainTrt:SeedTrt:MH|TurbTrt+Year)  #Just put in highest order interactions

lsmip(biomass.lmer6, RainTrt~MH|Year)
lsmip(biomass.lmer6, RainTrt~MH|Year+FireTrt)
lsmip(biomass.lmer6, RainTrt~MH|Year+SeedTrt)
lsmip(biomass.lmer6, RainTrt~MH|Year+TurbTrt)


lsmip(biomass.lmer6, RainTrt~MH|Year+FireTrt+SeedTrt+TurbTrt)

#This is crazy complicated... try simplifying.... 