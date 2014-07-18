
library(foreign)  # To get SPSS file
library(lme4)     # For random effects
library(lsmeans)  # To summarized expected values by group
library(pbkrtest) # Way to get p-values
                  # http://www.inside-r.org/packages/cran/lme4/docs/pvalues
                  # What is restriction matrix? http://people.su.se/~palme/Rmatr.pdf 

<<<<<<< HEAD
source("Transform.R")
=======
>>>>>>> c84c8d2c7722682d7688aa2e66fc93216ae506cb

annuals <- read.spss("SPSS BothSitesAllYears Data File.sav",to.data.frame=TRUE)
str(annuals)
annuals$Year=as.factor(annuals$Year)

#Model options- Don't put in Precipitation Amounts (WintRain), but include Year as a fixed effect.  This will allow for other weather variation rather than just precip. 

#Reponse variables:  Biomass, Counts of target species, Cover of target species.

   
####  Biomass -----

#Use this line to change subsets (i.e. switch to Sonoran)
subdata=subset(annuals, (Desert=='Mojave ' & TranDir=='N' & Year!=2011)) 
subdata=droplevels(subdata)


## Explain why we're doing specieal logw0 Transformations...
hist(subdata$EstBiomass)
table(subdata$EstBiomass) #755 zeros
min(subdata$EstBiomass[subdata$EstBiomass>0], na.rm=T)

hist(subdata$EstBiomass[subdata$EstBiomass>0])
hist(sqrt(subdata$EstBiomass)[subdata$EstBiomass>0])
hist(sqrt(subdata$EstBiomass))

#Reason for special log transform------------
tx=seq(0,5,by=0.001)

plot(tx, log(tx))
plot(tx, log(tx+1))
plot(tx, logw0(tx))
pairs(cbind(orig=tx, log=log(tx), logp1=log(tx+1), logw0=logw0(tx) ), lower.panel=NULL, row1attop = FALSE)

# Note logp1 changes overall shape of transform- the log relationship is lost. 
#############


#Try a two-stage model? First logistic regression on presence of biomass, then if present, how much?
subdata$BiomassPres=as.numeric(subdata$EstBiomass>0)
table(subdata$BiomassPres)

# Exploratory presence/absense crosstabs ------
#Looking for interactions
par(mfrow=c(2,3))
barplot(xtabs(~BiomassPres+Year, data=subdata), main="Year")
barplot(xtabs(~BiomassPres+RainTrt, data=subdata), main="RainTrt")
barplot(xtabs(~BiomassPres+FireTrt, data=subdata), main="FireTrt")
barplot(xtabs(~BiomassPres+MHcode, data=subdata), main="Microhabitat")
barplot(xtabs(~BiomassPres+SeedTrt, data=subdata), main="SeedTrt")
barplot(xtabs(~BiomassPres+TurbTrt, data=subdata), legend.text=c("Absent", "Present"), main="TurbTrt")
par(mfrow=c(1,1))
plot(xtabs(~MHcode+RainTrt+BiomassPres, data=subdata))  #Looks signif
plot(xtabs(~MHcode+FireTrt+BiomassPres, data=subdata))
plot(xtabs(~MHcode+SeedTrt+BiomassPres, data=subdata))
plot(xtabs(~MHcode+TurbTrt+BiomassPres, data=subdata))
plot(xtabs(~Year+MHcode+BiomassPres, data=subdata))  #looks Signif

plot(xtabs(~RainTrt+FireTrt+BiomassPres, data=subdata))
plot(xtabs(~RainTrt+SeedTrt+BiomassPres, data=subdata))
plot(xtabs(~RainTrt+TurbTrt+BiomassPres, data=subdata))
plot(xtabs(~Year+RainTrt+BiomassPres, data=subdata))  #looks Signif

plot(xtabs(~FireTrt+SeedTrt+BiomassPres, data=subdata))
plot(xtabs(~FireTrt+TurbTrt+BiomassPres, data=subdata))
plot(xtabs(~Year+FireTrt+BiomassPres, data=subdata))  

plot(xtabs(~SeedTrt+TurbTrt+BiomassPres, data=subdata))
plot(xtabs(~Year+SeedTrt+BiomassPres, data=subdata))  
plot(xtabs(~Year+TurbTrt+BiomassPres, data=subdata))  

# Logistic modeling -----
#Note y~(X1+X2+X3+X4)^4 is shorthand for full factorial up to 4-way interactions

library(lme4)
#Note: To allow convergence, must change optimizer to bobyqa according to Ben Bolker: http://stackoverflow.com/questions/21344555/convergence-error-for-development-version-of-lme4

#Only main affects
hurdle1.lme=glmer(BiomassPres~RainTrt+MH+TurbTrt+FireTrt+SeedTrt+Year  +(1|ShrubID)+(1|ShrubID:Plot),  data=subdata, family=binomial, control=glmerControl(optimizer="bobyqa") )
summary(hurdle1.lme)

#All two-way effects
hurdle2.lme=glmer(BiomassPres~(RainTrt+MH+TurbTrt+FireTrt+SeedTrt+Year)^2+ (1|ShrubID)+(1|ShrubID:Plot),  data=subdata, family=binomial, control=glmerControl(optimizer="bobyqa") )
summary(hurdle2.lme)

# Add only two way effects that look like they interact from cross tabs EDA: +MH:RainTrt + MH:Year + RainTrt:Year
hurdle3.lme=glmer(BiomassPres~RainTrt+MH+TurbTrt+FireTrt+SeedTrt+Year+MH:RainTrt + MH:Year + RainTrt:Year+ (1|ShrubID)+(1|ShrubID:Plot),  data=subdata, family=binomial, control=glmerControl(optimizer="bobyqa") )
summary(hurdle3.lme)
anova(hurdle3.lme)

# It actually doesn't look like RainTrt:Year is significant
hurdle4.lme=glmer(BiomassPres~RainTrt+MH+TurbTrt+FireTrt+SeedTrt+Year+MH:RainTrt + MH:Year + (1|ShrubID)+(1|ShrubID:Plot),  data=subdata, family=binomial, control=glmerControl(optimizer="bobyqa") )
summary(hurdle4.lme)
anova(hurdle4.lme)

# Or RainTrt:MH
hurdle5.lme=glmer(BiomassPres~RainTrt+MH+TurbTrt+FireTrt+SeedTrt+Year+ MH:Year + (1|ShrubID)+(1|ShrubID:Plot),  data=subdata, family=binomial, control=glmerControl(optimizer="bobyqa") )
summary(hurdle5.lme)
anova(hurdle5.lme)

# Or Turb or Seed Trt Main Effects...
hurdle6.lme=glmer(BiomassPres~RainTrt+MH+FireTrt+Year+ MH:Year + (1|ShrubID)+(1|ShrubID:Plot),  data=subdata, family=binomial, control=glmerControl(optimizer="bobyqa") )
summary(hurdle6.lme)
anova(hurdle6.lme)



hurdle7.lme=glmer(BiomassPres~RainTrt+MH+Year+ MH:Year + (1|ShrubID)+(1|ShrubID:Plot),  data=subdata, family=binomial, control=glmerControl(optimizer="bobyqa") )
summary(hurdle7.lme)
anova(hurdle7.lme)

# So it appears that the presence of biomass depends on Rain, MicroHabitat, Year, and there is an interaction of Microhabitat and year. 

lsmeans(hurdle7.lme, ~RainTrt+MH+Year, data=subdata)
lsmip(hurdle7.lme, RainTrt~MH|Year)

###########
# Now model biomass of non-zero quads
par(mfrow=c(2,3))
boxplot(EstBiomass~FireTrt, data=subdata[subdata$EstBiomass>0,], main="FireTrt")
boxplot(EstBiomass~RainTrt, data=subdata[subdata$EstBiomass>0,], main="RainTrt")
boxplot(EstBiomass~SeedTrt, data=subdata[subdata$EstBiomass>0,], main="SeedTrt")
boxplot(EstBiomass~TurbTrt, data=subdata[subdata$EstBiomass>0,], main="TurbTrt")
boxplot(EstBiomass~Year, data=subdata[subdata$EstBiomass>0,], main="Year")
boxplot(EstBiomass~MH, data=subdata[subdata$EstBiomass>0,], main="MH")
par(mfrow=c(1,1))

#Looking for interactions- I checked all pairwise interactins, but only these seemed like a possibility of interaction
par(mfrow=c(2,2))
boxplot(EstBiomass~RainTrt*Year, data=subdata[subdata$EstBiomass>0,], main="RainTrt x Yr") #Looks signif
boxplot(EstBiomass~RainTrt*MH, data=subdata[subdata$EstBiomass>0,], main="RainTrt x MH")  #Could be signif
boxplot(EstBiomass~Year*MH, data=subdata[subdata$EstBiomass>0,], main="Year x MH") #Could be signif
par(mfrow=c(1,1))



#Fit all main effects, and all two-way interactions
biomass.lmer=lmer(EstBiomass~(FireTrt+RainTrt+SeedTrt+TurbTrt+Year+MH)^2+(1|ShrubID)+(1|ShrubID:Plot),  data=subdata[subdata$EstBiomass>0,] )

summary(biomass.lmer)
anova(biomass.lmer)
dim(summary(biomass.lmer)$coefficients)
RM=Diagonal(dim(summary(biomass.lmer)$coefficients)[1])
#a slow function
pvals<-round(apply(RM, 1, function(x)KRmodcomp(biomass.lmer, t(x))$stats$p.value),4)
cbind(summary(biomass.lmer)$coefficients,pvals)
# The interaction terms look like they match our exploratory figures, so just add those interaction terms...

biomass2.lmer=lmer(EstBiomass~FireTrt+RainTrt+SeedTrt+TurbTrt+Year+MH + RainTrt:Year + RainTrt:MH + Year:MH+(1|ShrubID)+(1|ShrubID:Plot),  data=subdata[subdata$EstBiomass>0,] )

summary(biomass2.lmer)
anova(biomass2.lmer)
dim(summary(biomass2.lmer)$coefficients)
RM=Diagonal(dim(summary(biomass2.lmer)$coefficients)[1])
#a slow function
pvals<-round(apply(RM, 1, function(x)KRmodcomp(biomass2.lmer, t(x))$stats$p.value),4)
cbind(summary(biomass2.lmer)$coefficients,pvals)
#Main Effects of Fire, Seed and Turb NS.  Remove

biomass3.lmer=lmer(EstBiomass~RainTrt+Year+MH+ RainTrt:Year + RainTrt:MH + Year:MH+(1|ShrubID)+(1|ShrubID:Plot),  data=subdata[subdata$EstBiomass>0,] )

summary(biomass3.lmer)
anova(biomass3.lmer)
dim(summary(biomass3.lmer)$coefficients)
RM=Diagonal(dim(summary(biomass3.lmer)$coefficients)[1])
#a slow function
pvals<-round(apply(RM, 1, function(x)KRmodcomp(biomass3.lmer, t(x))$stats$p.value),4)
cbind(summary(biomass3.lmer)$coefficients,pvals)
# So stick with this model- Rain, Year, MH, and all interactions here 


#Get least squares means for each group of Rain*Year*MH
lsmeans(biomass3.lmer,  ~ RainTrt:Year:MH)  #Just put in highest order interactions
#Add pairwise comparisons
lsmeans(biomass3.lmer, pairwise ~ RainTrt:Year:MH)  
#Interactin Plot
lsmip(biomass3.lmer, RainTrt~MH|Year)


=======

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
>>>>>>> c84c8d2c7722682d7688aa2e66fc93216ae506cb
