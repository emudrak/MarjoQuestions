
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
table(subdata$Erci)  #891 zeros
hist(subdata$Erci)  #891 zeros
hist(logw0(subdata$Erci))

# Now model biomass of non-zero quads
par(mfrow=c(2,3))
boxplot(logw0(Erci)~FireTrt, data=subdata, main="FireTrt")
boxplot(logw0(Erci)~RainTrt, data=subdata, main="RainTrt")
boxplot(logw0(Erci)~SeedTrt, data=subdata, main="SeedTrt")
boxplot(logw0(Erci)~TurbTrt, data=subdata, main="TurbTrt")
boxplot(logw0(Erci)~Year, data=subdata, main="Year")
boxplot(logw0(Erci)~MH, data=subdata, main="MH")
par(mfrow=c(1,1))

#Looking for interactions- I checked all pairwise interactins, but only these seemed like a possibility of interaction
par(mfrow=c(2,2))
boxplot(logw0(Erci)~RainTrt*Year, data=subdata, main="RainTrt x Yr") #Looks signif
boxplot(logw0(Erci)~RainTrt*MH, data=subdata, main="RainTrt x MH")  #Could be signif
boxplot(logw0(Erci)~Year*MH, data=subdata, main="Year x MH") #Could be signif

counts.glmer=glmer(Erci~(FireTrt+RainTrt+SeedTrt+TurbTrt+Year+MH)+(1|ShrubID)+(1|ShrubID:Plot), family="poisson", data=subdata, control=glmerControl(optimizer="bobyqa")  )

summary(counts.glmer)
anova(counts.glmer)
plot(counts.glmer)


lsmeans(biomass.lmer6,  ~ FireTrt:RainTrt:SeedTrt:MH|TurbTrt+Year)  #Just put in highest order interactions

lsmip(biomass.lmer6, RainTrt~MH|Year)
lsmip(biomass.lmer6, RainTrt~MH|Year+FireTrt)
lsmip(biomass.lmer6, RainTrt~MH|Year+SeedTrt)
lsmip(biomass.lmer6, RainTrt~MH|Year+TurbTrt)


lsmip(biomass.lmer6, RainTrt~MH|Year+FireTrt+SeedTrt+TurbTrt)

#This is crazy complicated... try simplifying.... 