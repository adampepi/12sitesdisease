

rm(list=ls())
library(biwavelet)
library(cluster)
library(reshape)
library(ggplot2)
library(cowplot)
setwd("~/Documents/Research/dissertation/12 sites analyses/12sitesanalyses")


sites<-read.csv("12sitesseriesdensity2.csv")
str(sites)
sites

sites2<-read.csv("12sitesserieslong3.csv")
str(sites2)
sites2
sitesdata2<-sites2[,2:14]*15+1
sitesdata<-log(sites2[,2:14]*15+1)
sitesdata
siteswet<-rowMeans(sitesdata[,c(9:13)],na.rm=T)
sitesdry<-rowMeans(sitesdata[,c(1:8)])

siteswet2<-rowMeans(sitesdata2[,c(9:12)])
sitesdry2<-rowMeans(sitesdata2[,c(1:8)])

t1<-cbind(sites2$Year,sitesdata[,1])
t2<-cbind(sites2$Year,sitesdata[,2])
t3<-cbind(sites2$Year,sitesdata[,3])
t4<-cbind(sites2$Year,sitesdata[,4])
t5<-cbind(sites2$Year,sitesdata[,5])
t6<-cbind(sites2$Year,sitesdata[,6])
t7<-cbind(sites2$Year,sitesdata[,7])
t8<-cbind(sites2$Year,sitesdata[,8])
t9<-cbind(sites2$Year,sitesdata[,9])
t10<-cbind(sites2$Year,sitesdata[,10])
t11<-cbind(sites2$Year,sitesdata[,11])
t12<-cbind(sites2$Year,sitesdata[,12])
twet<-cbind(sites2$Year,siteswet)
tdry<-cbind(sites2$Year,sitesdry)
sitesdata

plot(t1,type='b',main="NEC")
plot(t2,type='b',main="DRW")
plot(t3,type='b',main="TOH")
plot(t4,type='b',main="ART")
plot(t5,type='b',main="JND")
plot(t6,type='b',main="MUP")
plot(t7,type='b',main="SCU")
plot(t8,type='b',main="RAB")
plot(t9,type='b',main="AFL")
plot(t10,type='b',main="HEM")
plot(t11,type='b',main="DRM")
plot(t12,type='b',main="BAY")

plot(twet,type='b',main="Wet")
plot(tdry,type='b',main="Dry")
nrands<-100000
wt1<-wt(t1)
plot(wt1, type = "power.corr.norm", main = "AFL",ylab="Period",xlab=NA)
wt2<-wt(t2)
plot(wt2, type = "power.corr.norm", main = "HEM",ylab="Period",xlab=NA)
wt3<-wt(t3)
plot(wt3, type = "power.corr.norm", main = "TOH",ylab="Period",xlab=NA)
wt4<-wt(t4)
plot(wt4, type = "power.corr.norm", main = "ART",ylab="Period",xlab=NA)
wt5<-wt(t5)
plot(wt5, type = "power.corr.norm", main = "JND",ylab="Period",xlab=NA)
wt6<-wt(t6)
plot(wt6, type = "power.corr.norm", main = "MUP",ylab="Period",xlab=NA)
wt7<-wt(t7)
plot(wt7, type = "power.corr.norm", main = "NEC",ylab="Period",xlab=NA)
wt8<-wt(t8)
plot(wt8, type = "power.corr.norm", main = "DRW",ylab="Period",xlab=NA)
wt9<-wt(t9)
plot(wt9, type = "power.corr.norm", main = "SCU",ylab="Period",xlab=NA)
wt10<-wt(t10)
plot(wt10, type = "power.corr.norm", main = "RAB",ylab="Period",xlab=NA)
wt11<-wt(t11)
plot(wt11, type = "power.corr.norm", main = "DRM",ylab="Period",xlab=NA)
wt12<-wt(t12)
plot(wt12, type = "power.corr.norm", main = "BAY",ylab="Period",xlab=NA)

wt13<-wt(twet)
plot(wt13, type = "power.corr.norm", main = "Wet",ylab="Period",xlab=NA)
wt14<-wt(tdry)
plot(wt14, type = "power.corr.norm", main = "Dry",ylab="Period",xlab=NA)



wt1$gws <- apply(wt1$power, 1, mean)
wt2$gws <- apply(wt2$power, 1, mean)
wt3$gws <- apply(wt3$power, 1, mean)
wt4$gws <- apply(wt4$power, 1, mean)
wt5$gws <- apply(wt5$power, 1, mean)
wt6$gws <- apply(wt6$power, 1, mean)
wt7$gws <- apply(wt7$power, 1, mean)
wt8$gws <- apply(wt8$power, 1, mean)
wt9$gws <- apply(wt9$power, 1, mean)
wt10$gws <- apply(wt10$power, 1, mean)
wt11$gws <- apply(wt11$power, 1, mean)
wt12$gws <- apply(wt12$power, 1, mean)
wt13$gws <- apply(wt13$power, 1, mean)
wt14$gws <- apply(wt14$power, 1, mean)

spectra<-data.frame(Dry=rowMeans(cbind(wt1$gws,wt2$gws,wt3$gws,wt4$gws,wt5$gws,wt6$gws,wt8$gws,wt7$gws)),Wet=rowMeans(cbind(wt9$gws,wt10$gws,wt11$gws,wt12$gws)),Period=wt1$period)

str(spectra)
spectra<-melt(spectra,id.vars='Period')
str(spectra)
spectra$Habitat<-spectra$variable
spectra$Power<-spectra$value

spectra2<-data.frame(Dry=wt14$gws,Wet=wt13$gws,Period=wt1$period)

str(spectra2)
spectra2<-melt(spectra2,id.vars='Period')
str(spectra2)
spectra2$Habitat<-spectra2$variable
spectra2$Power<-spectra2$value


wd<-data.frame(Dry=sitesdry2,Wet=siteswet2,Year=sites2$Year)
wd<-melt(wd,id.vars='Year')
str(wd)
wd$Habitat<-wd$variable
wd$Density<-wd$value/15

p1<-ggplot(wd,aes(y=Density,x=Year,color=Habitat))+geom_line()+scale_y_log10()+theme_classic()+geom_point()+ylab("Caterpillars/Lupine")+scale_x_continuous(n.breaks=10)

p2<-ggplot(spectra2,aes(y=Power,x=Period,color=Habitat))+geom_line()+scale_x_log10(n.breaks=8)+theme_classic()

pg1<-plot_grid(p1+ theme(legend.position="none"),p2+ theme(legend.position="none"),nrow=2,labels='AUTO')

legend_p1 <- get_legend(p1)
plot_grid(pg1,legend_p1,rel_widths = c(1,0.2))

par(mfrow=c(1,1))
plot(rowMeans(cbind(wt1$gws,wt2$gws,wt3$gws,wt4$gws,wt5$gws,wt6$gws,wt7$gws,wt8$gws)), wt7$period, type="l",
     xlab="Global Wavelet Spectrum", ylab=NA,
     log="y", ylim=rev(range(wt7$period)), xlim=range(c(0,4.2)),col="red")

lines(rowMeans(cbind(wt9$gws,wt10$gws,wt11$gws,wt12$gws)), wt1$period, lty=1, col='blue') 


par(mfrow=c(1,1))
plot(wt14$gws, wt7$period, type="l",
     xlab="Global Wavelet Spectrum", ylab=NA,
     log="y", ylim=rev(range(wt7$period)), xlim=range(c(0,4.2)),col="red")

lines(wt13$gws, wt1$period, lty=1, col='blue') 


