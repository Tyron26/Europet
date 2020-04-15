getwd()
setwd("C:/Users/user/Desktop/Europet R")
DT <- read.csv("EuroPet.csv") 
head(DT) 
dim(DT) 
#Q1
summary(DT)
#aggregate(Sales~FuelVolume, data = DT, mean)
reg1 = lm(Sales~FuelVolume, data = DT) 
summary(reg1)
#SE_FuelVolume = sd(DT$FuelVolume)/sqrt(length(DT$FuelVolume))
#Upper = mean(DT$FuelVolume) + 1.96 * SE_FuelVolume
#Lower = mean(DT$FuelVolume) - 1.96 * SE_FuelVolume
#CI = c(Lower, Upper)
#CI
#Uppermin = min(DT$FuelVolume) + 1.96 * SE_FuelVolume
#Lowermin = min(DT$FuelVolume) - 1.96 * SE_FuelVolume
#CI = c(Lowermin, Uppermin)
#CI
#Uppermax = max(DT$FuelVolume) + 1.96 * SE_FuelVolume
#Lowermax = max(DT$FuelVolume) - 1.96 * SE_FuelVolume
#CI = c(Lowermax, Uppermax)
#CI
#conf.level <-0.95
#z<-qt((1+conf.level)/2,df=length(DT$FuelVolume-1))
#SE_FuelVolume = sd(DT$FuelVolume)/sqrt(length(DT$FuelVolume))
#Q2
## Obtain the min, mean, and max of FuelVolume
minFuelVolume = min(DT$FuelVolume)
meanFuelVolume = mean(DT$FuelVolume)
maxFuelVolume = max(DT$FuelVolume)
## Regression
reg1 = lm(Sales ~ FuelVolume, data = DT)
## Set the input value
input = data.frame(FuelVolume = c(minFuelVolume,meanFuelVolume, maxFuelVolume))
## Use predict function
predict(reg1, input, interval = 'confidence')
#Q3
reg2 = lm(Sales~TV+Radio, data = DT)
summary(reg2)
coef(reg2)["(Intercept)"] + coef(reg2)["TV"]*40 + coef(reg2)["Radio"]*80
meanTV = mean(DT$TV)
meanRadio = mean(DT$Radio)
input = data.frame(TV=40,Radio=80)
predict(reg2,input,interval = 'confidence')
#Q4
reg3 = lm(Sales~TV+Radio+Temp, data = DT)
summary(reg3)
reg4 = lm(Sales~TV+Temp, data = DT)
summary(reg4)
#Q5
reg5 = lm(Sales~TV+Radio+FuelVolume+FuelPrice+Temp+Prec+Holiday+Visits, data = DT)
summary(reg5)
reg6 = lm(Sales~TV+FuelVolume+FuelPrice+Temp+Prec+Holiday+Visits, data = DT)
summary(reg6)
TV1=mean(DT$TV)+5
meanFuelVolume = mean(DT$FuelVolume)
meanFuelPrice = mean(DT$FuelPrice)
maenTemp=mean(DT$Temp)
meanPrec=mean(DT$Prec)
meanHoliday=mean(DT$Holiday)
meanVisits=mean(DT$Visits)
input = data.frame(TV=TV1,FuelVolume=meanFuelVolume,FuelPrice=meanFuelPrice,Temp=maenTemp,Prec=meanPrec,Holiday=meanHoliday,Visits=meanVisits)
predict(reg6,input,interval = 'confidence')
#Just check if TV is mean
meanTV=mean(DT$TV)
input = data.frame(TV=meanTV,FuelVolume=meanFuelVolume,FuelPrice=meanFuelPrice,Temp=maenTemp,Prec=meanPrec,Holiday=meanHoliday,Visits=meanVisits)
predict(reg6,input,interval = 'confidence')
#Q6
Weeks7 = cbind(ifelse(DT$Week=='7',1,0))
Weeks21 = cbind(ifelse(DT$Week=='21',1,0))            
Weeks49 = cbind(ifelse(DT$Week=='49',1,0))
reg7 = lm(Sales~TV+Radio+FuelVolume+FuelPrice+Temp+Prec+Holiday+Visits+Weeks7+Weeks21+Weeks49, data = DT)
summary(reg7)
reg8 = lm(Sales~TV+FuelVolume+FuelPrice+Temp+Prec+Holiday+Visits+Weeks7+Weeks21+Weeks49, data = DT)
summary(reg8)
#Q7
#summary(lm(Sales~TV+Radio+FuelVolume+FuelPrice+Temp+Prec+Holiday+Visits, data = DT[DT$Holiday==0,]))
#summary(lm(Sales~TV+Radio+FuelVolume+FuelPrice+Temp+Prec+Holiday+Visits, data = DT[DT$Holiday==1,]))
summary(lm(Sales~TV+Radio+FuelVolume+FuelPrice+Temp+Prec+Holiday+Visits + TV*Holiday +Radio*Holiday, data = DT))
#Q8
#reg9 = lm(Sales~TV+FuelVolume+FuelPrice+Temp+Prec+Visits, data = DT[DT$Holiday==0,])
#summary(reg9)
#reg10 = lm(Sales~FuelVolume+FuelPrice+Temp+Prec+Visits, data = DT[DT$Holiday==1,])
#summary(reg10)
#coef(reg9)["(Intercept)"] + coef(reg9)["TV"]*50 + coef(reg9)["FuelVolume"]*63000+coef(reg9)["FuelPrice"]*1.2+coef(reg9)["Temp"]*20+coef(reg9)["Prec"]*1+coef(reg9)["Visits"]*6
#coef(reg10)["(Intercept)"] + coef(reg10)["FuelVolume"]*63000+coef(reg10)["FuelPrice"]*1.2+coef(reg10)["Temp"]*20+coef(reg10)["Prec"]*1+coef(reg10)["Visits"]*6
#reg11 = lm(Sales~FuelVolume+FuelPrice+Prec+Visits, data = DT[DT$Holiday==1,])
#summary(reg11)
#coef(reg11)["(Intercept)"] + coef(reg11)["FuelVolume"]*63000+coef(reg11)["FuelPrice"]*1.2+coef(reg11)["Prec"]*1+coef(reg11)["Visits"]*6
coef(reg5)["(Intercept)"] + coef(reg5)["TV"]*50 + coef(reg5)["Radio"]*40+ coef(reg5)["FuelVolume"]*63000+coef(reg5)["FuelPrice"]*1.2+coef(reg5)["Temp"]*20+coef(reg5)["Prec"]*1+coef(reg5)["Visits"]*6+coef(reg5)["Holiday"]*0
coef(reg5)["(Intercept)"] + coef(reg5)["TV"]*50 + coef(reg5)["Radio"]*40+ coef(reg5)["FuelVolume"]*63000+coef(reg5)["FuelPrice"]*1.2+coef(reg5)["Temp"]*20+coef(reg5)["Prec"]*1+coef(reg5)["Visits"]*6+coef(reg5)["Holiday"]*1
reg12= lm(Sales~TV+Radio+FuelVolume+FuelPrice+Temp+Prec+Holiday+Visits+Holiday*Temp, data = DT)
summary (reg12)
reg13= lm(Sales~TV+FuelVolume+FuelPrice+Temp+Prec+Holiday+Visits+Holiday*Temp, data = DT)
summary (reg13)
coef(reg13)["(Intercept)"] + coef(reg13)["TV"]*50 + coef(reg13)["FuelVolume"]*63000+coef(reg13)["FuelPrice"]*120+coef(reg13)["Temp"]*20+coef(reg13)["Prec"]*1+coef(reg13)["Visits"]*6+coef(reg13)["Holiday"]*1 + (-76.8*20)
coef(reg13)["(Intercept)"] + coef(reg13)["TV"]*50 + coef(reg13)["FuelVolume"]*63000+coef(reg13)["FuelPrice"]*120+coef(reg13)["Temp"]*20+coef(reg13)["Prec"]*1+coef(reg13)["Visits"]*6

