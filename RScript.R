
library(foreign)  # To get SPSS file
library(lme4)     # For random effects
library(lsmeans)  # To summarized expected values by group
library(pbkrtest) # Way to get p-values
                  # http://www.inside-r.org/packages/cran/lme4/docs/pvalues
                  # What is restriction matrix? http://people.su.se/~palme/Rmatr.pdf 


annuals <- read.spss("SPSS BothSitesAllYears Data File.sav",to.data.frame=TRUE)
str(annuals)
annuals$Year=as.factor(annuals$Year)

#Model options- Don't put in Precipitation Amounts (WintRain), but include Year as a fixed effect.  This will allow for other weather variation rather than just precip. 

#Reponse variables:  Biomass, Counts of target species, Cover of target species.

   
####  Biomass -----

#Use this line to change subsets (i.e. switch to Sonoran)
subdata=subset(annuals, (Desert=='Mojave ' & TranDir=='N' & Year!=2011)) 

hist(subdata$EstBiomass)
table(subdata$EstBiomass) #755 zeros
min(subdata$EstBiomass[subdata$EstBiomass>0], na.rm=T)


hist(log(subdata$EstBiomass+1))  #Logging this is silly
plot(subdata$EstBiomass, log(subdata$EstBiomass+1))
table(log(subdata$EstBiomass+1)) 

hist(logw0(subdata$EstBiomass))
plot(subdata$EstBiomass, logw0(subdata$EstBiomass))
table(logw0(subdata$EstBiomass))

hist(log(subdata$EstBiomass))
hist(logw0(subdata$Erci))
table(logw0(subdata$Erci))

#Reason for special log transform
log(0.001)
log(0.01)
log(1.001)
log(1.01)
tx=seq(0,10,by=0.001)
plot(tx)
plot(tx, log(tx))
plot(tx, log(tx+1))
plot(tx, logw0(tx))
pairs(cbind(log=log(tx), logp1=log(tx+1), logw0=logw0(tx), orig=tx))


plot(subdata$EstBiomass, log(subdata$EstBiomass))
plot(subdata$EstBiomass, log(subdata$EstBiomass+1))








#Full factorial model
biomass.lmer=lmer(EstBiomass~FireTrt*RainTrt*SeedTrt*TurbTrt*Year*MH+(1|ShrubID)+(1|ShrubID:Plot),  data=subdata )
#Gives warning message. Maybe false positive?  see Ben Bolkers comment here: 
# http://comments.gmane.org/gmane.comp.lang.r.lme4.devel/11674
summary(biomass.lmer)
anova(biomass.lmer)
dim(summary(biomass.lmer)$coefficients)
RM=Diagonal(192)
#a slow function
pvals<-apply(RM, 1, function(x)KRmodcomp(biomass.lmer, t(x))$stats$p.value)
cbind(summary(biomass.lmer)$coefficients,pvals)
#No six-way interxns signif (phew!)

biomass.lmer2=lmer(EstBiomass~(FireTrt+RainTrt+SeedTrt+TurbTrt+Year+MH)^5+(1|ShrubID)+(1|ShrubID:Plot),  data=subset(annuals, (Desert=='Mojave ' & TranDir=='N' & Year!=2011)), control=lmerControl(optimizer="bobyqa") )
summary(biomass.lmer2)
dim(summary(biomass.lmer2)$coefficients)
RM2=Diagonal(186)
#a slow function
pvals2<-apply(RM2, 1, function(x)KRmodcomp(biomass.lmer2, t(x))$stats$p.value)
cbind(summary(biomass.lmer2)$coefficients,pvals2)


biomass.lmer3=lmer(EstBiomass~(FireTrt+RainTrt+SeedTrt+TurbTrt+Year+MH)^4 +(1|ShrubID)+(1|ShrubID:Plot),  data=subset(annuals, (Desert=='Mojave ' & TranDir=='N' & Year!=2011)), control=lmerControl(optimizer="bobyqa") )
summary(biomass.lmer3)
dim(summary(biomass.lmer3)$coefficients)
RM3=Diagonal(157)
#a slow function
pvals3<-apply(RM3, 1, function(x)KRmodcomp(biomass.lmer3, t(x))$stats$p.value)
cbind(summary(biomass.lmer3)$coefficients,round(pvals3,4))
#Some signif p-vals here: FireTrtUB:RainTrtE:SeedTrtS:MHON 



biomass.lmer4=lmer(EstBiomass~(FireTrt+RainTrt+SeedTrt+TurbTrt+Year+MH)^3 + FireTrt:RainTrt:SeedTrt:MH  +(1|ShrubID)+(1|ShrubID:Plot),  data=subset(annuals, (Desert=='Mojave ' & TranDir=='N' & Year!=2011)), control=lmerControl(optimizer="bobyqa") )
summary(biomass.lmer4)
dim(summary(biomass.lmer4)$coefficients)
RM4=Diagonal(106)
#a slow function
pvals4<-apply(RM4, 1, function(x)KRmodcomp(biomass.lmer4, t(x))$stats$p.value)
cbind(summary(biomass.lmer4)$coefficients,round(pvals4,4))
#Some signif p-vals here: FireTrtUB:RainTrtE:SeedTrtS:MHON (before) RainTrt:Year:MH FireTrt:SeedTrt:MH FireTrt:RainTrt:MH FireTrt:RainTrt:TurbTrt  FireTrt:RainTrt:SeedTrt



biomass.lmer5=lmer(EstBiomass~(FireTrt+RainTrt+SeedTrt+TurbTrt+Year+MH)^2 + FireTrt:RainTrt:SeedTrt:MH + RainTrt:SeedTrt:MH  + FireTrt:SeedTrt:MH +FireTrt:RainTrt:MH + FireTrt:RainTrt:SeedTrt + RainTrt:Year:MH + FireTrt:RainTrt:TurbTrt + (1|ShrubID)+(1|ShrubID:Plot),  data=subset(annuals, (Desert=='Mojave ' & TranDir=='N' & Year!=2011)), control=lmerControl(optimizer="bobyqa") ) 
summary(biomass.lmer5)
dim(summary(biomass.lmer5)$coefficients)
RM5=Diagonal(73)
#a slow function
pvals5<-apply(RM5, 1, function(x)KRmodcomp(biomass.lmer5, t(x))$stats$p.value)
cbind(summary(biomass.lmer5)$coefficients,round(pvals5,4))
#Some signif p-vals here


biomass.lmer6=lmer(EstBiomass~(FireTrt+RainTrt+SeedTrt+TurbTrt+Year+MH)^2 + FireTrt:RainTrt:SeedTrt:MH + RainTrt:SeedTrt:MH  + FireTrt:SeedTrt:MH +FireTrt:RainTrt:MH + FireTrt:RainTrt:SeedTrt + RainTrt:Year:MH + FireTrt:RainTrt:TurbTrt - Year:TurbTrt - FireTrt:Year+ (1|ShrubID)+(1|ShrubID:Plot),  data=subset(annuals, (Desert=='Mojave ' & TranDir=='N' & Year!=2011)), control=lmerControl(optimizer="bobyqa") ) 
summary(biomass.lmer6)
dim(summary(biomass.lmer6)$coefficients)
RM6=Diagonal(71)
#a slow function
pvals6<-apply(RM6, 1, function(x)KRmodcomp(biomass.lmer6, t(x))$stats$p.value)
cbind(summary(biomass.lmer6)$coefficients,round(pvals6,4))
#Final model?
plot(biomass.lmer6)
qqnorm(biomass.lmer6$resid)

lsmeans(biomass.lmer6,  ~ FireTrt:RainTrt:SeedTrt:MH|TurbTrt+Year)  #Just put in highest order interactions

lsmip(biomass.lmer6, RainTrt~MH|Year)
lsmip(biomass.lmer6, RainTrt~MH|Year+FireTrt)
lsmip(biomass.lmer6, RainTrt~MH|Year+SeedTrt)
lsmip(biomass.lmer6, RainTrt~MH|Year+TurbTrt)


lsmip(biomass.lmer6, RainTrt~MH|Year+FireTrt+SeedTrt+TurbTrt)

#This is crazy complicated... try simplifying.... 