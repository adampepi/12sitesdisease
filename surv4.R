library(glmmTMB)
library(lme4)
library(emmeans)
library(ggplot2)
library(brms)
library(bayesplot)
library(bayestestR)
library(tidybayes)
library(tidyverse)
library(cowplot)
library(reshape2)
setwd("~/Documents/Research/dissertation/12 sites analyses/12sitesdisease")

s2<-read.csv('surv2.csv')
str(s2)
s2$date<-as.factor(s2$date)
s2$block<-as.factor(s2$block)
s2$total<-(s2$survived+s2$died)
s2brm<-s2[s2$total>0,]

s2$watering<-as.factor(s2$watering)
s2$warming<-as.factor(s2$warming)



brm1<-brm(survived|trials(total)~watering*warming+(1|date)+(1|block),data=s2brm,family=binomial,iter=4000)
summary(brm1)


brm2<-brm(survived|trials(total)~watering+warming+(1|date)+(1|block),data=s2brm,family=binomial,iter=10000)
summary(brm2)
prior_summary(brm2)



brms::pp_check(brm1,nsamples=50)
brms::pp_check(brm2,nsamples=50)

brm1<-add_criterion(brm1,"waic")
brm2<-add_criterion(brm2,"waic")

brmcomp<-loo_compare(brm1,brm2, criterion = "waic")

print(brmcomp,simplify = F)


posterior<-as.matrix(brm2)
str(posterior)

p2<-mcmc_areas(posterior,pars = c("b_Intercept",'b_wateringwatered','b_warmingwarmed'),
           prob = 0.9, prob_outer = 0.99)+theme(plot.title = element_text(hjust = 0.5))+scale_y_discrete(labels=c(expression(beta[0]),expression(beta[watering]),expression(beta[warming])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=15))+xlab('Estimate')+ylab("Parameter")
p2
posterior_summary(brm2, probs = c(0.05, 0.95), robust = FALSE)
posterior_summary(brm1, probs = c(0.05, 0.95), robust = FALSE)

s2$survival<-s2$survived/s2$total
p1<-ggplot(s2,aes(y=survival,fill=watering,x=warming))+geom_bar(stat='summary',position = position_dodge(width=1))+geom_errorbar(stat='summary',position = position_dodge(width=1))+xlab("Warming")+ylab('Survival')+labs(fill="Watering")+theme_classic()

cowplot::plot_grid(p1,p2,labels=c('A','B'))

