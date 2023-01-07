rm(list=ls)
library(reshape)
library(tidyverse)
setwd("~/Documents/Research/dissertation/12 sites analyses/12sitesdisease")


sites<-read.csv("12sitesseriesdensity.csv")
str(sites)
sites

sites2<-read.csv("12sitesserieslong2.csv")
str(sites2)
distmat<-read.csv('distmat.csv')
ncol(distmat)
distmat<-distmat[,2:21]

###Custom index - hilltop connectivity
## Calculate Hanski CI for each hilltop from larval patches

CIpatch<-matrix(nrow=12,ncol=15)
CIpatch

CIhills<-matrix(nrow=8,ncol=15)
CIhills
length(CIhills)

alpha<-1
b<-1
t<-1
for(t in 1:15){
  for(i in 1:8){
    ##Loop to calculate connectivity for each hilltop
    
    exps<-rep(0,12)
    for(j in 1:12){
      ##Each patch
      dist<-distmat[12+i,j]
      area<-sites[j,4+t] ##area is population density
      exps[j]<-exp(-alpha*dist)*area
      
    }
    CIhills[i,t]<-sum(exps)
  }
}

CIhills
colnames(CIhills)<-sites2$Year
rownames(CIhills)<-colnames(distmat)[13:20]
CIhills<-log(CIhills+7.020758e-229 )## Logged to make the next step more reasonable.  Smallest non-zero value added to allow logging
mean(CIhills)
CIhills

CIhills<-(CIhills-mean(CIhills))/sd(CIhills)+2
hist(CIhills)

CIhills

###Now calculate CI for patches, based on hills

for(t in 1:15){
  for(i in 1:12){
    ##Loop to calculate connectivity for each patch
    
    exps<-rep(0,8)
    for(j in 1:8){
      ##Using each hilltop
      dist<-distmat[i,12+j]
      area<-CIhills[j,t]
      exps[j]<-exp(-alpha*dist)*area
      
    }
    CIpatch[i,t]<-sum(exps)
  }
}
CIpatch

colnames(CIpatch)<-sites2$Year
rownames(CIpatch)<-colnames(distmat)[1:12]
hist(CIpatch)
CIpatch
mat10k<-CIpatch*10e100
mat10k
matplot(log(mat10k),type='l')
min(CIpatch)
CIpatch<-log(CIpatch)
matplot(lCIpatch,type='l')

CIpatch<-(CIpatch-mean(CIpatch))/sd(CIpatch)
CIpatch
write.csv(CIpatch, file='connectivity2.csv')

write.csv(CIhills, file='connectivityhills2.csv')
