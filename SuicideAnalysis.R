setwd("C:/Users/Tobi/Documents/R")
LifeExp=read.table("R tutorials_LifeExpRegion.txt")
colnames(LifeExp)= c("Country", 'Age', 'Region')
Suicide=read.csv("Worldwide suicides 2004.csv",header=T)
SuicideReg=merge(Suicide,LifeExp, by="Country")
attach(SuicideReg)

SuicideR=split(SuicideReg,Region)
SAs=SuicideReg[Region=="SAs",][,2]
EuCA=SuicideReg[Region=="EuCA",][,2]
EAP=SuicideReg[Region=="EAP",][,2]
summary(SAs, digits=7)
summary(EuCA)
summary(EAP)

boxplot(MENA,SAs, range=1, col=2:3, main='Suicides Worldwide in 2004', names=c('Middle East', 'South Asia'), ylab="Total Deaths")
boxplot(Suicide.Deaths~Region, range=0, horizontal=T, col=rainbow(6), main='Suicides Worldwide in 2004', xlab="Total Deaths")
boxplot(Suicide.Deaths~Region, horizontal=T, col=rainbow(6), main='Suicides Worldwide in 2004', xlab="Total Deaths")
legend("topright", col=rainbow(6), pch=15, c('Americas','East Asia & Pacific','Europe & Central Asia','Middle East & North Africa','South Asia', 'Sub-Saharan Africa'), cex=.7)

hist(EAP, breaks=12, freq=F, main="Histogram of East Asia Pacific with Probability Density Curve", xlab="Number of Suicides", ylim=c(0,14E-5))
lines(density(EAP), col=2)
hist(SAs, breaks=1, freq=F, main="Histogram of South Asia with Probability Density Curve", xlab="Number of Suicides")
lines(density(SAs), col=2)
hist(EuCA, breaks=5, freq=F, ylim=c(0,11E-04), main="Histogram of Middle East with Probability Density Curve", xlab="Number of Suicides")
lines(density(EuCA), col=2)

plot(Suicide.Deaths, xlab="Country", ylab="Number of Deaths", main="Plot of Suicides Worldwide")
summary(Suicide.Deaths)
Mean=aggregate(Suicide.Deaths~Region, data=SuicideReg, mean)
MeanAge=aggregate(Age~Region, data=SuicideReg, mean)
Summary=aggregate(Suicide.Deaths~Region, data=SuicideReg, summary, digits=6)
MeanM=data.matrix(Mean[[2]])
t(MeanM)
plot(Mean)

barplot(MeanM, beside=T)
par(font.lab=2)
barplot(Mean$Suicide.Deaths, names=c('Amer', 'EAP', 'EuCA', 'MENA', 'SAs', 'SSA'), col=2:7, main='Average Suicides', xlab='Region', ylab='Number of Deaths' )
legend("topleft", col=2:7, pch=15, c('Americas','East Asia & Pacific','Europe & Central Asia','Middle East & North Africa','South Asia', 'Sub-Saharan Africa'), cex=.8)
head(SuicideOutRm,5)

#Outliers (rows 61 India, 30 China, 115 Rusia) and NAs (row 94, 119) removed
SuicideOutRm=SuicideReg[Suicide.Deaths!=188524 or 222211] #why doesn't this work?!?
SuicideOutRm=subset(SuicideReg,Suicide.Deaths!=188524& Suicide.Deaths!=222211 & Suicide.Deaths!=52841)
nrow(SuicideOutRm)
length(SuicideOutRm[[2]])
SummaryOut=aggregate(SuicideOutRm[[2]]~SuicideOutRm[[4]], data=SuicideReg, summary, digits=5)

boxplot(SuicideOutRm[[2]]~SuicideOutRm[[4]], horizontal=T, col=rainbow(6), main='Suicides Worldwide in 2004', xlab="Total Deaths")#no outliers
legend("topright", col=rainbow(6), pch=15, c('Americas','East Asia & Pacific','Europe & Central Asia','Middle East & North Africa','South Asia', 'Sub-Saharan Africa'),cex=.8)
plot(SuicideOutRm[[2]], xlab="Country", ylab="Number of Deaths", main="Plot of Suicides Worldwide")
MeanOut=aggregate(SuicideOutRm[[2]]~SuicideOutRm[[4]], data=SuicideOutRm, mean)
barplot(MeanOut[[2]], names=c('Amer', 'EAP', 'EuCA', 'MENA', 'SAs', 'SSA'), col=2:7, main='Average Suicides', xlab='Region', ylab='Number of Deaths' )
legend("topleft", col=2:7, pch=15, c('Americas','East Asia & Pacific','Europe & Central Asia','Middle East & North Africa','South Asia', 'Sub-Saharan Africa'), cex=.8)
MeanLifeExp=MeanAge[[2]]
MeanSuicide=Mean[[2]]
cor.test(MeanLifeExp, MeanSuicide) #relationship between life expectancy and number of suicides?

