library(readxl)
library(neuralnet)
Project_Data <- read_excel("Documents/MBA/Semester 3/Data Mining/Project/Project Data.xlsx")
source ('http://blogs.fiveelementanalytics.com/RCode/min_max_normalization.R')
Votes<-Project_Data
Votes<-Votes[complete.cases(Votes),]
Votes<-Votes[,-c(1:3)]
Votes<-Votes[,-c(3:4)]
Votes$votes16_Hillary_Clinton<-NULL
Votes<-Votes[,-c(5:9)]
Male<-Votes$Men/Votes$TotalPop
Female<-Votes$Women/Votes$TotalPop
Votes$Men<-Male
Votes$Women<-Female

Age.Income<-Votes[,c(15:19)]
Age.Income = as.data.frame(apply(Age.Income, 2, FUN =min_max_normal))
Votes$VotingAgeCitizen<-Age.Income$VotingAgeCitizen
Votes$Income<-Age.Income$Income
Votes$IncomeErr<-Age.Income$IncomeErr
Votes$IncomePerCap<-Age.Income$IncomePerCap
Votes$IncomePerCapErr<-Age.Income$IncomePerCapErr
PCT<-Votes[,c(20:33)]
PCT=PCT/100
Votes[,c(20:33)]<-PCT
EMP<-Votes$Employed/Votes$TotalPop
Votes$Employed<-EMP


PCT2<-Votes[,c(34:38)]
PCT2<-PCT2/100

Votes[,c(34:38)]<-PCT2
Ethnicity<-Votes[,c(10:15)]
Ethnicity<-Ethnicity/100
Votes[,c(10:15)]<-Ethnicity
Votes$Unemployment<-Votes$Unemployment/100
#Votes for 2016 & 2020
Votes16<-Votes
Votes16<-Votes16[,-c(3:7)]

Votes20<-Votes
Votes20<-Votes20[,-c(1:2)]

Cases=Votes20$cases/Votes20$TotalPop
Votes20$cases<-Cases
Deaths=Votes20$deaths/Votes20$TotalPop
Votes20$deaths<-Deaths
Votes20$TotalPop<-NULL

#Neural Network for Votes 16
Trump_win<-Votes$percentage16_Donald_Trump>Votes16$percentage16_Hillary_Clinton
Trump_win<-as.numeric(Trump_win)
Votes16$Trump_win<-Trump_win
Votes16<-Votes16[,-c(1:2)]
#formula
n = colnames(Votes16)   
frm = as.formula(paste("Trump_win ~", paste(n[!n %in% "Trump_win"], collapse = " + ")))

nn = neuralnet(frm,data=Votes16,hidden=c(10,5),linear.output=F)
plot(nn)

Election16_prediction = compute(nn, covariate = Votes16[,-c(33)])$net.result
Election16_prediction = as.matrix(cbind(Election16_prediction, Votes16[,33]))
Election16_prediction=round(Election16_prediction,1)

Election16_prediction = as.data.frame(Election16_prediction)

#Error
Error<-(Election16_prediction$Election16_prediction==Election16_prediction$Trump_win)
Election16_prediction$Error<-as.numeric(Error)
1-mean(Election16_prediction$Error)

#Neural Network for Votes 2020
Trump_lose<-Votes20$percentage20_Donald_Trump<Votes20$percentage20_Joe_Biden
Trump_lose<-as.numeric(Trump_lose)
Votes20$Trump_lose<-Trump_lose

Votes20<-Votes20[,-c(1:2)]

#formula
n = colnames(Votes20)   
frm = as.formula(paste("Trump_lose ~", paste(n[!n %in% "Trump_lose"], collapse = " + ")))

nn = neuralnet(frm,data=Votes20,hidden=c(10,5),linear.output=F)
plot(nn)

Election20_prediction = compute(nn, covariate = Votes20[,-c(35)])$net.result
Election20_prediction = as.matrix(cbind(Election20_prediction, Votes20[,35]))
Election20_prediction=round(Election20_prediction,1)

Election20_prediction = as.data.frame(Election20_prediction)

#Error
Error<-(Election20_prediction$Election20_prediction==Election20_prediction$Trump_lose)
Election20_prediction$Error<-as.numeric(Error)
1-mean(Election16_prediction$Error)







