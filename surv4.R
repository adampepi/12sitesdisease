library(glmmTMB)
library(lme4)
library(emmeans)
library(ggplot2)
library(brms)
library(bayesplot)
library(bayestestR)
library(equatiomatic)
library(tidybayes)
library(rethinking)
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



brm2<-brm(survived|trials(total)~watering*warming+(1|date)+(1|block),data=s2brm,family=binomial,iter=4000)
summary(brm2)


brm2.1<-brm(survived|trials(total)~watering+warming+(1|date)+(1|block),data=s2brm,family=binomial,iter=10000)
summary(brm2.1)
prior_summary(brm2.1)

brm2.2<-brm(survived|trials(total)~watering+(1|date)+(1|block),data=s2brm,family=binomial,iter=4000)
summary(brm2.2)
posterior_interval(brm2.1,prob=0.9)

brm2.3<-brm(survived|trials(total)~warming+(1|date)+(1|block),data=s2brm,family=binomial,iter=4000)
summary(brm2.3)

brm2.4<-brm(survived|trials(total)~(1|date)+(1|block),data=s2brm,family=binomial,iter=4000)
summary(brm2.4)

brms::pp_check(brm2,nsamples=50)
brms::pp_check(brm1,nsamples=50)
brms::pp_check(brm2.4,nsamples=50)

brm2<-add_criterion(brm2,"waic")
brm2.1<-add_criterion(brm2.1,"waic")
brm2.2<-add_criterion(brm2.2,"waic")
brm2.3<-add_criterion(brm2.3,"waic")
brm2.4<-add_criterion(brm2.4,"waic")

brmcomp<-loo_compare(brm2,brm2.1,brm2.2,brm2.3,brm2.4, criterion = "waic")

print(brmcomp,simplify = F)

extract_eq(brm2.4)


posterior<-as.matrix(brm2.1)
str(posterior)

p2<-mcmc_areas(posterior,pars = c("b_Intercept",'b_wateringwatered','b_warmingwarmed'),
           prob = 0.9, prob_outer = 0.99)+theme(plot.title = element_text(hjust = 0.5))+scale_y_discrete(labels=c(expression(beta[0]),expression(beta[watering]),expression(beta[warming])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=15))+xlab('Estimate')+ylab("Parameter")
p2
posterior_summary(brm2.1, probs = c(0.05, 0.95), robust = FALSE)
posterior_summary(brm1, probs = c(0.05, 0.95), robust = FALSE)


posterior<-as.matrix(brm2.1)
str(posterior)

str(brm2.1)

brm2.1.1<-recover_types(brm2.1,s2)
str(brm2.1.1)
spread_draws(brm2.1.1,b[watering],b[warming])
post1<-spread_draws(brm2.1.1,b_wateringwatered,b_warmingwarmed,b_Intercept)
head(post1)
post1$watered_warmed<-post1$b_Intercept+post1$b_wateringwatered+post1$b_warmingwarmed
post1$watered_control<-post1$b_Intercept+post1$b_wateringwatered
post1$control_warmed<-post1$b_Intercept+post1$b_warmingwarmed
post1$control_control<-post1$b_Intercept
str(post1)
post2<-post1[,-c(4:6)]
str(post2)
post3<-melt(post2,id.vars=c('.chain','.iteration','.draw'))
str(post3)
post3$value<-inv.logit(post3$value)
levels(post3$variable)
post3$variable<-factor(post3$variable,levels=c("control_control" , "control_warmed","watered_control","watered_warmed") )

ggplot(data=post3,aes(y = value, x = variable,fill=variable,color=variable)) +stat_eye()+scale_fill_manual(values=c("grey",'grey',"#C77CFF","#C77CFF"),labels=c("Control/Control",'Control/Warmed',"Watered/Control","Watered/Warmed"))+scale_color_manual(values=c("black","#F8766D",'black',"#F8766D"),labels=c("Control/Control",'Control/Warmed',"Watered/Control","Watered/Warmed"))+scale_x_discrete(labels=c("Control/Control",'Control/Warmed',"Watered/Control","Watered/Warmed"))+xlab('Treatment')+ylab('Survival')

str(s2)
s2$survival<-s2$survived/s2$total
p1<-ggplot(s2,aes(y=survival,fill=watering,x=warming))+geom_bar(stat='summary',position = position_dodge(width=1))+geom_errorbar(stat='summary',position = position_dodge(width=1))+xlab("Warming")+ylab('Survival')+labs(fill="Watering")+theme_classic()

cowplot::plot_grid(p1,p2,labels=c('A','B'))

