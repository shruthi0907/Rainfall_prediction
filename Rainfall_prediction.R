data<-read.csv("C:\\Users\\bhara\\Downloads\\archive\\district wise rainfall normal.csv")
print(data)
state<-data$STATE
annual<-data$ANNUAL
jan<-data$JAN
feb<-data$FEB
mar<-data$MAR
apr<-data$APR
may<-data$MAY
jun<-data$JUN
jul<-data$JUL
aug<-data$AUG
sep<-data$SEP
oct<-data$OCT
nov<-data$NOV
dec<-data$DEC
jf<-data$Jan.Feb
mm<-data$Mar.May
js<-data$Jun.Sep
od<-data$Oct.Dec
name=readline(prompt="Enter the state name:")    
name1<-toString(name)
print(name1)
val<-c()
val1<-c()
for(i in 1:nrow(data)){
  if(state[i]==name){
    val=append(val,jan[i])
    val1=append(val1,annual[i])
  } 
  
}
relation=lm(val1~val)
rain=as.numeric(readline(prompt="enter rain fall rate for jan month:"))
a=data.frame(val=rain)
result=predict(relation,a)
print(result)
data<-read.csv("E:\\shruthi\\R programs\\sample.csv")
print(data)
library(ggplot2)
library(AICcmodavg)
#Importing Data
c.dt <- read.csv("E:\\shruthi\\R programs\\sample.csv",header = TRUE, colClasses = c("factor","numeric","numeric","numeric","numeric"))
summary(c.dt)

#One-way ANOVA
on.way <- aov(YEAR2016~YEAR2017, data = c.dt)
summary(on.way)
on.way <- aov(YEAR2017~YEAR2018, data = c.dt)
summary(on.way)
on.way <- aov(YEAR2018~YEAR2019, data = c.dt)
summary(on.way)

