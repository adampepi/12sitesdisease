rm(list=ls())
library(tidyverse)
library(performance)
library(sjstats)
library(glmmTMB)
library(sjPlot)
library(ordinal)
library(brms)
library(emmeans)
library(tidybayes)
library(ggplot2)
library(AICcmodavg)
library(MASS)
library(loo)
library(modelr)
library(cowplot)
library(bayesplot)
library(boot)
library(Hmisc)
setwd("~/Documents/Research/dissertation/12 sites analyses/12sitesdisease")

##With both years
d3<-read.csv('merged.data.2021.csv')
str(d3)
d3$habitat<-as.factor(d3$habitat)
d3$site<-as.factor(d3$site)
d3$observer<-as.factor(d3$observer)
d3$dissection.order.scale<-scale(d3$dissection.order)
d3$dissection.order.scale2<-d3$dissection.order.scale^2
d3$infection_severity.ord<-factor(d3$infection_severity,levels=c("uninfected","l","m","h","sh"),ordered=T)
d3$log.last.year.scale<-scale(log(d3$last_year+1/15))
d3$log.this.year.scale<-scale(log(d3$this_year+1/15))
str(d3)


brm2.1<-brm(infection~habitat*log.last.year.scale+log.this.year.scale+dissection.order.scale+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.1)
get_prior(brm2.1)
prior_summary(brm2.1)
brms::pp_check(brm2.1,nsamples=100)

brm2.2<-brm(infection~habitat+log.last.year.scale+log.this.year.scale+dissection.order.scale+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.2)
brms::pp_check(brm2.2,nsamples=100)

brm2.3<-brm(infection~log.last.year.scale+log.this.year.scale+dissection.order.scale+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.3)
brms::pp_check(brm2.3,nsamples=100)


prior_summary(brm2.3)

brms::pp_check(brm2.3,nsamples=100)


brm2.1<-add_criterion(brm2.1,"waic")
brm2.2<-add_criterion(brm2.2,"waic")
brm2.3<-add_criterion(brm2.3,"waic")

brmcomp2<-loo_compare(brm2.1,brm2.2,brm2.3,criterion = "waic")

print(brmcomp2,simplify = F)
write.csv(brmcomp2,'infectioncomp.csv')

summary(brm2.3)
posterior_interval(brm2.3, prob=0.9)
bayes_R2(brm2.3)


ddplot5data<-d3 %>%
  data_grid(log.last.year.scale = seq_range(log.last.year.scale, n = 101),log.this.year.scale=mean(log.this.year.scale),dissection.order.scale=0) %>%
  add_fitted_draws(brm2.3, n = 1000,re_formula = NA,scale='response') 

p1<-ggplot(ddplot5data,aes(x = log.last.year.scale, y =infection)) + stat_lineribbon(aes(y = .value))+geom_count(data=d3)+xlab(expression(X[t-1]))+ylab('Infection')+theme_classic()+scale_fill_brewer()+theme(legend.position = 'none')
p1

ddplot5.1data<-d3 %>%
  data_grid(log.last.year.scale = mean(log.last.year.scale),log.this.year.scale=seq_range(log.this.year.scale,n=101),dissection.order.scale=0) %>%
  add_fitted_draws(brm2.3, n = 1000,re_formula = NA,scale='response') 


p2<-ggplot(ddplot5.1data,aes(x = log.this.year.scale, y =infection)) + stat_lineribbon(aes(y = .value))+geom_count(data=d3)+xlab(expression(X[t]))+ylab('Infection')+theme_classic()+scale_fill_brewer()+ theme(legend.position = "none")

inf.wet<-sum(d3$infection[d3$habitat=='Wet'])
n.wet<-length(d3$infection[d3$habitat=='Wet'])

inf.dry<-sum(d3$infection[d3$habitat=="Dry"])
n.dry<-length(d3$infection[d3$habitat=='Dry'])

wet.conf<-binconf(inf.wet,n.wet,alpha=0.05)
dry.conf<-binconf(inf.dry,n.dry,alpha=0.05)
str(dry.conf)

inf.int<-data.frame(habitat=c("Wet","Dry"),mean=c(wet.conf[1],dry.conf[1]),upper=c(wet.conf[3],dry.conf[3]),lower=c(wet.conf[2],dry.conf[2]))
inf.int

p3<-inf.int%>% ggplot(aes(x=habitat,y=mean,fill=habitat))+geom_bar(stat="identity")+geom_errorbar(aes(ymin=lower,ymax=upper))+xlab("Habitat")+ylab('Infection')+theme_classic()+ theme(legend.position = "none")+ylim(0,1)

p3


legend_p2 <- get_legend(p2 + theme(legend.position="left"))

plot_grid(p1,p3,p2,legend_p2,labels=c("A","B","C"))

