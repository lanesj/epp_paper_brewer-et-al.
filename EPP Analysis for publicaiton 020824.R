

library(Rmisc) 
library(tidyverse)
library(car)
library(lme4)
library(lmerTest)
library(lubridate)
library(nlme)
library(glmm)
library(predictmeans)
library(lsmeans)
library(gridExtra)
library(effects)
library(emmeans)
library(broom)
library(modelr)
library(ggeffects) #ggpredict
library(ggpubr) #scatter plots
library(MASS) #negative binomial 
library(performance)

#################################################################################################
##########-----------------Presence of EPY--------------#########################################
#################################################################################################

#Read in data
epp.pc<-read.csv("extra-pair paternity data.csv")

#filter just EPP data
epp.pc = epp.pc %>% filter(Cuckolding.identified.by.any.method.95.!="NA")

#add total number of nestlings to the data set
epp.pc$total.number.nestlings=epp.pc$Number.of.SOSP.nestlings+epp.pc$Number.of.BHCO.Nestlings

#use lubridate to add day to data 
epp.pc$day=yday(epp.pc$date)

#Factor the numbers
your.dat <- mutate(epp.pc,
                 Cuckolding.identified.by.any.method.95.=factor(Cuckolding.identified.by.any.method.95.))

##################################################################################################################################
##################################################################################################################################
####################################----------Parental care analysis--------################################################
##################################################################################################################################
##################################################################################################################################

#################################################################################################
##########-----------------Presence of EPY--------------#########################################
#################################################################################################


#Full mixed model looking at the main effect of EPP and sex on parental care
pc<-lmer(visist.per.id~cuck.95*sex+day+total.number.nestlings+SOSP.Age+year+(1|nest.id), data = epp.pc)

#Check out them residuals
hist(residuals(pc))

#Summary plots for the model
check_model(pc)

#Look at summary of model
summary(pc)

#Model dropping age
pc<-lmer(visist.per.id~cuck.95*sex+day+total.number.nestlings+year+(1|nest.id), data = epp.pc)

#Check out them residuals
hist(residuals(pc))

#Summary plots for the model
check_model(pc)

#Look at summary of model
summary(pc)

#Model dropping year
pc<-lmer(visist.per.id~cuck.95*sex+day+total.number.nestlings+(1|nest.id), data = epp.pc)

#Check out them residuals
hist(residuals(pc))

#Summary plots for the model
check_model(pc)

#Look at summary of model
summary(pc)

#Post hoc analysis
emm.pc<-emmeans(pc, pairwise ~ cuck.95*sex)

emm.pc

###################-----------------Figure for number of offspring--------------################################

#Change variable titles for paper 
epp.pc$sex<- factor(epp.pc$sex, levels=c("female","male"))

levels(epp.pc$sex) <- c("Female","Male")

epp.pc$cuck.95 <- factor(epp.pc$cuck.95, levels=c("no","yes"))

levels(epp.pc$cuck.95) <- c("No","Yes")

#Summarize
EPY.table = epp.pc %>% 
  group_by(band.number,sex,nest.id,cuck.95,X..of.offspring.sampled.that.are.epp) %>% 
  summarise(mean.visists=mean(visist.per.id,na.rm=TRUE))

#Summarize by sex and presence absence of EPY
epp.pc.sum<-summarySE(epp.pc, measurevar="visist.per.id", groupvars=c("sex","cuck.95"), na.rm = TRUE)

epp.pc.full<-ggplot(EPY.table, aes(x=cuck.95, y=mean.visists, color=sex))+ 
  geom_jitter(aes(color=sex),position=position_jitter(w=0.1, h=0.1), shape = 16, size=2, alpha = 0.75)+
  geom_errorbar(data = epp.pc.sum, mapping = aes(x = cuck.95, y = visist.per.id, 
                                                 col=as.factor(cuck.95),
                                                 ymin = visist.per.id - se, ymax = visist.per.id + se), size=1, width=.3) +
  geom_point(data = epp.pc.sum, mapping = aes(x =cuck.95, y= visist.per.id, 
                                              col=as.factor(cuck.95)), size=3,  shape=16) +
  facet_grid(.~sex, scales ="free_x", switch = "x")+
  theme_classic()+
  theme(text =element_text(size =15))+
  theme(panel.grid.major =element_blank(), panel.grid.minor =element_blank())+
  ylab("Average nest visits (visits/hour)") +
  xlab("Presence of extra-pair young") +
  scale_color_manual(values = c("No" = "gray1", "Yes" = "gray60"))

epp.pc.full + theme(legend.position="none")

#################################################################################################
##########-----------------Number of EPY--------------#########################################
#################################################################################################

#Full mixed model looking at the main effect of number of EPY and sex on parental care
pc.num<-lmer(visist.per.id~X..of.epp.offspring.in.nest*sex+day+total.number.nestlings+SOSP.Age+year+(1|nest.id), data = epp.pc)

#Check out them residuals
hist(residuals(pc.num))

#Check assumptions
check_model(pc.num)

#Diagnostic plot
plot(pc.num)

#Look at summary of model
summary(pc.num)

#Model Removing nestling age
pc.num<-lmer(visist.per.id~X..of.epp.offspring.in.nest*sex+day+total.number.nestlings+year+(1|nest.id), data = epp.pc)

#Check out them residuals
hist(residuals(pc.num))

#Check assumptions
check_model(pc.num)

#Diagnostic plot
plot(pc.num)

#Look at summary of model
summary(pc.num)

#Model Removing Year sampled
pc.num<-lmer(visist.per.id~X..of.epp.offspring.in.nest*sex+day+total.number.nestlings+(1|nest.id), data = epp.pc)

#Check out them residuals
hist(residuals(pc.num))

#Check assumptions
check_model(pc.num)

#Diagnostic plot
plot(pc.num)

#Look at summary of model
summary(pc.num)

#Pos-hoc analysis
emt <- emtrends(pc.num, "sex", var = "X..of.epp.offspring.in.nest")

emt         

pairs(emt)   

#################################################################################################
##########-----------------Proportion of EPY--------------#########################################
#################################################################################################

#Full mixed model looking at the main effect of proportion of EPY and sex on parental care
pc.por<-lmer(visist.per.id~X..of.offspring.sampled.that.are.epp*sex+day+total.number.nestlings+SOSP.Age+year+(1|nest.id), data = epp.pc)

#Check out them residuals
hist(residuals(pc.por))

#Check assumptions
check_model(pc.por)

#Diagnostic plot
plot(pc.por)

#Look at summary of model
summary(pc.por)

#Model Dropping Nestling age
pc.por<-lmer(visist.per.id~X..of.offspring.sampled.that.are.epp*sex+day+total.number.nestlings+year+(1|nest.id), data = epp.pc)

#Check out them residuals
hist(residuals(pc.por))

#Check assumptions
check_model(pc.por)

#Diagnostic plot
plot(pc.por)

#Look at summary of model
summary(pc.por)

#Moel dropping year
pc.por<-lmer(visist.per.id~X..of.offspring.sampled.that.are.epp*sex+day+total.number.nestlings+(1|nest.id), data = epp.pc)

#Check out them residuals
hist(residuals(pc.por))

#Check assumptions
check_model(pc.por)

#Diagnostic plot
plot(pc.por)

#Look at summary of model
summary(pc.por)

#Post hoc analysis
emt <- emtrends(pc.por, "sex", var = "X..of.offspring.sampled.that.are.epp")

emt         

pairs(emt) 
