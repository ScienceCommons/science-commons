#############MANY LABS 3 ELABORATION LIKELIHOOD MODEL##################
#######Charlie Ebersole, Begin January 21, 2014###########

#setwd("/Users/Charlie/Desktop/ML3 Final Data")
#ML3<-read.csv(file="ML3AllSitesandmTurk.csv",header=TRUE,stringsAsFactors=FALSE)
ML3<-read.csv(file="https://osf.io/pftdg/?action=download&version=1",header=TRUE,stringsAsFactors=FALSE)
head(ML3)

#Required Packages
require(car)
require(doBy)
require(ggplot2)
require(effects)
require(MBESS)
require(lmSupport)

str(ML3$NFCcenter)
str(ML3$ArgumentQuality)
str(ML3$ELMCond)
ML3$ELMCond<-as.factor(ML3$ELMCond)
Cond<-subset(ML3,ML3$ELMCond!="NA")
NFC<-subset(Cond,Cond$NFCcenter!="NA")
DV<-length(unlist(NFC$ArgumentQuality!="NA"))

###Replicating Previous Effect###
#The five items will be averaged as an index of argument quality.  In the original study, participants who scored in the upper or lower third of need for cognition were recruited from the available sample for the study and were labeled as being high or low on the trait for analysis.  We will use all participants and treat need for cognition as a continuous measure.  As such the key analysis will be a general linear model with condition (strong = 1 vs. weak = -1), need for cognition (mean centered), and their interaction predicting argument quality.  The key test is the interaction term to show that the effect of the manipulation is moderated by need for cognition.

ELM.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=ML3)
Anova(ELM.lm,type="II")
summary(ELM.lm)
modelEffectSizes(ELM.lm)
plot(allEffects(ELM.lm))
ci.pvaf(F.value= 79.9251,df.1=1,df.2=2361,N=2365,conf.level=.95)
ci.pvaf(F.value= 0.1289,df.1=1,df.2=2361,N=2365,conf.level=.95)
ci.pvaf(F.value= 2.3859,df.1=1,df.2=2361,N=2365,conf.level=.95)

ci.pvaf(F.value= 79.9251,df.1=1,df.2=2361,N=2365,conf.level=.99)
ci.pvaf(F.value= 0.1289,df.1=1,df.2=2361,N=2365,conf.level=.99)

###DESCRIPTIVES###
summaryBy(ArgumentQuality~ELMCond,data=ML3,FUN=list(mean,max,min,median,sd),na.rm=TRUE)
ggplot(ML3,aes(x=ArgumentQuality,fill=ELMCond))+geom_histogram(binwidth=1,alpha=.5,position="identity")


##############################Supplementary Analyses#######################################
AttentionPass<-subset(ML3,ML3$AttentionCheck=="Pass")
length(AttentionPass$session_id)

#Primary Replication
ELM2.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=AttentionPass)
Anova(ELM2.lm,type="II")
plot(allEffects(ELM2.lm))

###This Effect First###
Weak1st<-subset(ML3,ML3$elmweak_order==2)
list(Weak1st$elmweak_order)
length(Weak1st$session_id)
Strong1st<-subset(ML3,ML3$elmstrong_order==2)
list(Strong1st$elmstrong_order)
length(Strong1st$session_id)
ELMfirst<-rbind(Weak1st,Strong1st)

ELM1.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=ELMfirst)
Anova(ELM1.lm,type="II")
plot(allEffects(ELM1.lm))


##############################Exploratory Analyses#######################################
str(ML3$DaysInComp)
Pool<-subset(ML3,ML3$Site!="mTurk")
length(unique(Pool$session_id))
str(Pool$ELMCond)
Pool$ELMCond<-as.factor(Pool$ELMCond)


ELM.Time.lm<-lm(ArgumentQuality~NFCcenter*ELMCond*DaysInComp,data=Pool)
Anova(ELM.Time.lm,type="II")
plot(allEffects(ELM.Time.lm))

ELMStrong<-subset(Pool,Pool$ELMCond==1)
list(ELMStrong$elmweak_order)
list(ELMStrong$elmstrong_order)
list(ELMStrong$ELMCond)
ELM.Time.Strong.lm<-lm(ArgumentQuality~DaysInComp*NFCcenter,data=ELMStrong)
Anova(ELM.Time.Strong.lm,type="II")
plot(allEffects(ELM.Time.Strong.lm))

ELMWeak<-subset(Pool,Pool$ELMCond==-1)
list(ELMWeak$elmstrong_order)
list(ELMWeak$elmweak_order)
list(ELMWeak$ELMCond)
ELM.Time.Weak.lm<-lm(ArgumentQuality~NFCcenter*DaysInComp,data=ELMWeak)
Anova(ELM.Time.Weak.lm,type="II")
plot(allEffects(ELM.Time.Weak.lm))

FirstFifth<-subset(Pool,Pool$DaysInComp<.2)
ELM.Fifth.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=FirstFifth)
Anova(ELM.Fifth.lm,type="II")
plot(allEffects(ELM.Fifth.lm))

LastFifth<-subset(Pool,Pool$DaysInComp>.8)
ELM.LastFifth.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=LastFifth)
Anova(ELM.LastFifth.lm,type="II")
plot(allEffects(ELM.LastFifth.lm))


###############################Time of Semester Analyses#################################
###80 vs. 20 Analyses
Pool<-subset(ML3,ML3$Site!="mTurk")
First80<-subset(Pool,DaysInComp<.8)
Last20<-subset(Pool,DaysInComp>=.8)
range(First80$DaysInComp)
range(Last20$DaysInComp)

#First 80
ELM80.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=First80)
Anova(ELM80.lm,type="II")
plot(allEffects(ELM80.lm))
ci.pvaf(F.value= 57.8695,df.1=1,df.2=1789,N=1793,conf.level=.95)
ci.pvaf(F.value= 0.2822,df.1=1,df.2=1789,N=1793,conf.level=.95)

#Last 20
ELM20.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Last20)
Anova(ELM20.lm,type="II")
plot(allEffects(ELM20.lm))
ci.pvaf(F.value= 22.1004,df.1=1,df.2=568,N=572,conf.level=.95)
ci.pvaf(F.value= 2.4380,df.1=1,df.2=568,N=572,conf.level=.95)


#####Mixed Models####
str(Pool$DaysInComp)
str(Pool$ArgumentQuality)
str(Pool$NFCcenter)
str(Pool$ELMCond)

#Unconditional Model
ArgumentQuality.Uncond<-lmer(ArgumentQuality~1+(1|Site),data=Pool)
summary(ArgumentQuality.Uncond)
0.02417/(0.02417+ 2.21580)

#Full Model
ArgumentQuality.MEmodel<-lmer(ArgumentQuality~ELMCond*NFCcenter*DaysInComp+(1|Site),data=Pool)
summary(ArgumentQuality.MEmodel)

#Model Comparison
ArgumentQuality.MEmodel.null<-lmer(ArgumentQuality~ELMCond*NFCcenter+(1|Site),data=Pool,REML=FALSE)
ArgumentQuality.MEmodel.test<-lmer(ArgumentQuality~ELMCond*NFCcenter*DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(ArgumentQuality.MEmodel.null,ArgumentQuality.MEmodel.test)
coef(ArgumentQuality.MEmodel.test)

#Added Main Effect
#Full Model
ArgumentQuality.MEmodel<-lmer(ArgumentQuality~ELMCond*DaysInComp+(1|Site),data=Pool)
summary(ArgumentQuality.MEmodel)

#Model Comparison
ArgumentQuality.MEmodel.null<-lmer(ArgumentQuality~ELMCond+(1|Site),data=Pool,REML=FALSE)
ArgumentQuality.MEmodel.test<-lmer(ArgumentQuality~ELMCond*DaysInComp+(1|Site),data=Pool,REML=FALSE)
anova(ArgumentQuality.MEmodel.null,ArgumentQuality.MEmodel.test)
coef(ArgumentQuality.MEmodel.test)

###############Moderator and Order Analyses#################
require(lmSupport)
###Moderators for Main Effect
str(Pool$AttentionCheck)
str(Pool$ELMCond)
Pool$AttentionCheck<-as.factor(Pool$AttentionCheck)
AttentionCheck<-lm(ArgumentQuality~ELMCond*AttentionCheck,data=Pool)
Anova(AttentionCheck,type="II")
ci.pvaf(F.value= 1.8005,df.1=1,df.2=2340,N=2344,conf.level=.95)

ML3$AttentionCheck<-as.factor(ML3$AttentionCheck)
AttentionCheck<-lm(ArgumentQuality~ELMCond*AttentionCheck,data=ML3)
Anova(AttentionCheck,type="II")
modelEffectSizes(AttentionCheck)
ci.pvaf(F.value= 0.5783,df.1=1,df.2=2340,N=2344,conf.level=.95)
ci.pvaf(F.value= 0.0293,df.1=1,df.2=2340,N=2344,conf.level=.95)


str(Pool$ReportedAttention)
ReportedAttention<-lm(ArgumentQuality~ELMCond*ReportedAttention,data=Pool)
Anova(ReportedAttention,type="II")
ci.pvaf(F.value= 0.4014,df.1=1,df.2=2348,N=2352,conf.level=.95)

str(Pool$ReportedEffort)
ReportedEffort<-lm(ArgumentQuality~ELMCond*ReportedEffort,data=Pool)
Anova(ReportedEffort,type="II")
ci.pvaf(F.value= 0.2521,df.1=1,df.2=2347,N=2351,conf.level=.95)

str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)
Gender<-lm(ArgumentQuality~ELMCond*Genderfactor,data=Pool)
Anova(Gender,type="II")
ci.pvaf(F.value= 4.3394,df.1=1,df.2=2349,N=2353,conf.level=.95)
plot(allEffects(Gender))
modelEffectSizes(Gender)
Weak<-subset(Pool,ELMCond=="-1")
Strong<-subset(Pool,ELMCond=="1")
t.test(ArgumentQuality~Genderfactor,data=Weak)
summaryBy(ArgumentQuality~Genderfactor,data=Weak,FUN=list(mean,sd),na.rm=TRUE)
t.test(ArgumentQuality~Genderfactor,data=Strong)

str(Pool$Conscientiousness)
Conscientiousness<-lm(ArgumentQuality~ELMCond*Conscientiousness,data=Pool)
Anova(Conscientiousness,type="II")
ci.pvaf(F.value= 0.8295,df.1=1,df.2=2345,N=2349,conf.level=.95)

str(Pool$Mood)
Mood<-lm(ArgumentQuality~ELMCond*Mood,data=Pool)
Anova(Mood,type="II")
ci.pvaf(F.value= 0.2272,df.1=1,df.2=2352,N=2356,conf.level=.95)

str(Pool$Stress)
Stress<-lm(ArgumentQuality~ELMCond*Stress,data=Pool)
Anova(Stress,type="II")
ci.pvaf(F.value= 0.0297,df.1=1,df.2=2341,N=2345,conf.level=.95)


###Moderators for Primary Effect
str(Pool$AttentionCheck)
Pool$AttentionCheck<-as.factor(Pool$AttentionCheck)
AttentionCheck<-lm(ArgumentQuality~ELMCond*NFCcenter*AttentionCheck,data=Pool)
Anova(AttentionCheck,type="II")
ci.pvaf(F.value= 1.8005,df.1=1,df.2=2340,N=2344,conf.level=.95)

str(Pool$ReportedAttention)
ReportedAttention<-lm(ArgumentQuality~ELMCond*NFCcenter*ReportedAttention,data=Pool)
Anova(ReportedAttention,type="II")
ci.pvaf(F.value= 0.4014,df.1=1,df.2=2348,N=2352,conf.level=.95)

str(Pool$ReportedEffort)
ReportedEffort<-lm(ArgumentQuality~ELMCond*NFCcenter*ReportedEffort,data=Pool)
Anova(ReportedEffort,type="II")
ci.pvaf(F.value= 0.2521,df.1=1,df.2=2347,N=2351,conf.level=.95)

str(Pool$Genderfactor)
Pool$Genderfactor<-as.factor(Pool$Genderfactor)
Gender<-lm(ArgumentQuality~ELMCond*NFCcenter*Genderfactor,data=Pool)
Anova(Gender,type="II")
ci.pvaf(F.value= 4.3394,df.1=1,df.2=2349,N=2353,conf.level=.95)
plot(allEffects(Gender))

Men<-subset(Pool,Genderfactor=="Male")
Women<-subset(Pool,Genderfactor=="Female")
MaleELM<-lm(ArgumentQuality~ELMCond*NFCcenter,data=Men)
Anova(MaleELM,type="II")
FemaleELM<-lm(ArgumentQuality~ELMCond*NFCcenter,data=Women)
Anova(FemaleELM,type="II")
plot(allEffects(FemaleELM))


str(Pool$Conscientiousness)
Conscientiousness<-lm(ArgumentQuality~ELMCond*NFCcenter*Conscientiousness,data=Pool)
Anova(Conscientiousness,type="II")
ci.pvaf(F.value= 0.8295,df.1=1,df.2=2345,N=2349,conf.level=.95)

str(Pool$Mood)
Mood<-lm(ArgumentQuality~ELMCond*NFCcenter*Mood,data=Pool)
Anova(Mood,type="II")
ci.pvaf(F.value= 0.2272,df.1=1,df.2=2352,N=2356,conf.level=.95)

str(Pool$Stress)
Stress<-lm(ArgumentQuality~ELMCond*NFCcenter*Stress,data=Pool)
Anova(Stress,type="II")
ci.pvaf(F.value= 0.0297,df.1=1,df.2=2341,N=2345,conf.level=.95)




###Exploratory
HighNFC<-subset(Female,NFCcenter>.75)
HighNFC$NFCtype<-"High"
LowNFC<-subset(Female,NFCcenter< -.75)
LowNFC$NFCtype<-"Low"
ExtremeNFC<-rbind(HighNFC,LowNFC)
str(ExtremeNFC$NFCtype)
ExtremeNFC$NFCtype<-as.factor(ExtremeNFC$NFCtype)
ExtremeELM<-lm(ArgumentQuality~ELMCond*NFCtype,data=ExtremeNFC)
Anova(ExtremeELM,type="II")
plot(allEffects(ExtremeELM))



#Summary Data
str(ML3$ELMCond)
ELMSum<-summarise(group_by(ML3,Site),WeakN=sum(ELMCond=="-1",na.rm=TRUE),StrongN=sum(ELMCond=="1",na.rm=TRUE),WeakMean=mean(ArgumentQuality[ELMCond=="-1"],na.rm=TRUE),StrongMean=mean(ArgumentQuality[ELMCond=="1"],na.rm=TRUE),WeakSD=sd(ArgumentQuality[ELMCond=="-1"],na.rm=TRUE),StrongSD=sd(ArgumentQuality[ELMCond=="1"],na.rm=TRUE))
ELMSum
#setwd("/Users/Charlie/Desktop")
write.csv(ELMSum,file="ELMSum.csv",row.names=FALSE)

####Effect by Site

Ashland<-subset(ML3,ML3$Site=="AshlandUniversity")
length(Ashland$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Ashland)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.5849,df.1=1,df.2=81,N=85,conf.level=.95)

Bradley<-subset(ML3,ML3$Site=="BradleyUniversity")
length(Bradley$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Bradley)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.3980,df.1=1,df.2=108,N=112,conf.level=.95)

Carleton<-subset(ML3,ML3$Site=="CarletonUniversity")
length(Carleton$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Carleton)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.0383,df.1=1,df.2=38,N=42,conf.level=.95)

Ithaca<-subset(ML3,ML3$Site=="IthacaCollege")
length(Ithaca$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Ithaca)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.0518,df.1=1,df.2=82,N=86,conf.level=.95)

Miami<-subset(ML3,ML3$Site=="MiamiUniversity")
length(Miami$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Miami)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.8656,df.1=1,df.2=86,N=90,conf.level=.95)

MichSt<-subset(ML3,ML3$Site=="MichiganStateUniversity")
length(MichSt$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=MichSt)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.6674,df.1=1,df.2=106,N=110,conf.level=.95)

Montana<-subset(ML3,ML3$Site=="MontanaStateUniversity")
length(Montana$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Montana)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 1.1271,df.1=1,df.2=120,N=124,conf.level=.95)

Nova<-subset(ML3,ML3$Site=="NovaSoutheasternUniversity")
length(Nova$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Nova)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.1474,df.1=1,df.2=126,N=130,conf.level=.95)

OSU<-subset(ML3,ML3$Site=="OSUNewark")
length(OSU$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=OSU)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.0009,df.1=1,df.2=147,N=151,conf.level=.95)

PLU<-subset(ML3,ML3$Site=="PacificLutheranUniversity")
length(PLU$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=PLU)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.1980,df.1=1,df.2=98,N=102,conf.level=.95)

Penn<-subset(ML3,ML3$Site=="PennStateAbington")
length(Penn$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Penn)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.2597,df.1=1,df.2=104,N=108,conf.level=.95)

SDSU<-subset(ML3,ML3$Site=="SanDiegoStateUniversity")
length(SDSU$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=SDSU)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.0120,df.1=1,df.2=132,N=136,conf.level=.95)

Texas<-subset(ML3,ML3$Site=="TexasAandM")
length(Texas$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Texas)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.3556,df.1=1,df.2=173,N=177,conf.level=.95)

Davis<-subset(ML3,ML3$Site=="UCDavis")
length(Davis$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Davis)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.1202,df.1=1,df.2=112,N=116,conf.level=.95)

Riverside<-subset(ML3,ML3$Site=="UCRiverside")
length(Riverside$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Riverside)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 1.2132,df.1=1,df.2=224,N=228,conf.level=.95)

Florida<-subset(ML3,ML3$Site=="UniversityOfFlorida")
length(Florida$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Florida)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.2023,df.1=1,df.2=131,N=135,conf.level=.95)

Mississippi<-subset(ML3,ML3$Site=="UniversityOfSouthernMississippi")
length(Mississippi$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Mississippi)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.0015,df.1=1,df.2=85,N=89,conf.level=.95)

Toronto<-subset(ML3,ML3$Site=="UniversityOfToronto")
length(Toronto$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Toronto)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.1005,df.1=1,df.2=73,N=77,conf.level=.95)

Virginia<-subset(ML3,ML3$Site=="UniversityOfVirginia")
length(Virginia$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=Virginia)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 0.8508,df.1=1,df.2=171,N=175,conf.level=.95)

VCU<-subset(ML3,ML3$Site=="VirginiaCommonwealthUniversity")
length(VCU$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=VCU)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
ci.pvaf(F.value= 2.1826,df.1=1,df.2=88,N=92,conf.level=.95)

mTurk<-subset(ML3,ML3$Site=="mTurk") #weird error somehow, can't figure out@#$@#$@##$ so we'll just leave out sample for now
length(mTurk$session_id)
ELM.inter.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=mTurk)
Anova(ELM.inter.lm,type="II")
summary(ELM.inter.lm)
#partial-eta squared 95% CI calculations
modelEffectSizes(ELM.inter.lm)
#ci.pvaf(F.value= 0.3980,df.1=1,df.2=108,N=112,conf.level=.95)




#####Task Order Effects####
head(ML3)
str(ML3$elmques_order)
Order.lm<-lm(ArgumentQuality~NFCcenter*ELMCond* elmques_order,data=ML3)
Anova(Order.lm,type="II")
ci.pvaf(F.value= 2.7608,df.1=1,df.2=2357,N=2365,conf.level=.95)
ci.pvaf(F.value= 0.4437,df.1=1,df.2=2357,N=2365,conf.level=.95)

ML3$elmques_ordersquare<-ML3$elmques_order^2
OrderQuad.lm<-lm(ArgumentQuality~NFCcenter*ELMCond* elmques_ordersquare,data=ML3)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 3.4855,df.1=1,df.2=2357,N=2365,conf.level=.95)
ci.pvaf(F.value= 0.0435,df.1=1,df.2=2357,N=2365,conf.level=.95)

Order2<-read.csv(file="ML3Order10.csv",header=TRUE)
head(Order2)
ML3<-merge(ML3,Order2,by="session_id",all=TRUE)
head(ML3)

str(ML3$ELMOrder10)
Order.lm<-lm(ArgumentQuality~NFCcenter*ELMCond* ELMOrder10,data=ML3)
Anova(Order.lm,type="II")
ci.pvaf(F.value= 0.3482,df.1=1,df.2=2357,N=2365,conf.level=.95)
ci.pvaf(F.value= 2.0423,df.1=1,df.2=2357,N=2365,conf.level=.95)

ML3$ELMOrder10square<-ML3$ELMOrder10^2
OrderQuad.lm<-lm(ArgumentQuality~NFCcenter*ELMCond* ELMOrder10square,data=ML3)
Anova(OrderQuad.lm,type="II")
ci.pvaf(F.value= 0.0420,df.1=1,df.2=2357,N=2365,conf.level=.95)
ci.pvaf(F.value= 2.0806,df.1=1,df.2=2357,N=2365,conf.level=.95)

First<-subset(ML3,ELMOrder10==1)
ELM.lm<-lm(ArgumentQuality~NFCcenter*ELMCond,data=First)
Anova(ELM.lm,type="II")
ci.pvaf(F.value= 1.3016,df.1=1,df.2=260,N=264,conf.level=.95)
ci.pvaf(F.value= 19.0228,df.1=1,df.2=260,N=264,conf.level=.95)


###PLU
PLU<-subset(ML3,Site=="PacificLutheranUniversity")
head(PLU)
PLU.lm<-lm(ArgumentQuality~NFCcenter*ELMCond*DaysInComp,data=PLU)
Anova(PLU.lm,type="II")


###Revision analyses###
ML3$NFCcenter<-as.numeric(ML3$NFCceneter)

cor.test(ML3$NFCcenter,ML3$ArgumentQuality,method="pearson")
cor(ML3$NFCceneter,ML3$ArgumentQuality)
?cor.test

?quantile
quantile(ML3$NFCcenter,probs=seq(0,1,.33),na.rm=TRUE)
lwr3<-subset(ML3,NFCcenter< -0.221523)
range(lwr3$NFCcenter)
upr3<-subset(ML3,NFCcenter>0.278477)
range(upr3$NFCcenter)
lwr3$split<--1
upr3$split<-1
Splits<-rbind(lwr3,upr3)

Splits$split<-as.factor(Splits$split)
str(Splits$ELMCond)
Splits$ELMCond<-as.factor(Splits$ELMCond)
Thirds.lm<-lm(ArgumentQuality~split*ELMCond,data=Splits)
Anova(Thirds.lm,type="II")
Anova(Thirds.lm,type="III")
require(lmSupport)
modelEffectSizes(Thirds.lm)

median(ML3$NFCcenter,na.rm=TRUE)
lwrMed<-subset(ML3,NFCcenter< -0.0548563)
lwrMed$MedSplit<- -1

uprMed<-subset(ML3,NFCcenter> -0.0548563)
uprMed$MedSplit<- 1

Med<-rbind(lwrMed,uprMed)

Med$MedSplit<-as.factor(Med$MedSplit)
Med$ELMCond<-as.factor(Med$ELMCond)
Med.lm<-lm(ArgumentQuality~MedSplit*ELMCond,data=Med)
Anova(Med.lm,type="II")
Anova(Med.lm,type="III")
require(lmSupport)
modelEffectSizes(Med.lm)

