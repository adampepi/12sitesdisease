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

ggplot(d3,aes(x=dissection.order,y=infection,color=observer))+geom_smooth()+geom_count()
ggplot(d3,aes(x=dissection.order.scale,y=infection,color=observer))+geom_smooth()+geom_count()


brm2.2<-brm(infection~habitat*log.last.year.scale+log.this.year.scale+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.2)
get_prior(brm2.2)
prior_summary(brm2.2)
brms::pp_check(brm2.2,nsamples=100)

brm2.3<-brm(infection~habitat*log.last.year.scale+log.this.year.scale+observer+dissection.order.scale+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.3)
brms::pp_check(brm2.3,nsamples=100)

brm2.3.1<-brm(infection~habitat+log.last.year.scale+log.this.year.scale+observer+dissection.order.scale+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.3.1)
brms::pp_check(brm2.3.1,nsamples=100)

brm2.4<-brm(infection~habitat*log.last.year.scale+log.this.year.scale+dissection.order.scale+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.4)
brms::pp_check(brm2.4,nsamples=100)

brm2.5<-brm(infection~log.last.year.scale+log.this.year.scale+dissection.order.scale+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.5)
brms::pp_check(brm2.5,nsamples=100)

prior_summary(brm2.5)

brm2.6<-brm(infection~log.last.year.scale+log.this.year.scale+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.6)
brms::pp_check(brm2.6,nsamples=100)

brm2.7<-brm(infection~log.this.year.scale+dissection.order.scale+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.7)

brm2.8<-brm(infection~log.this.year.scale+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.8)
brms::pp_check(brm2.6,nsamples=100)

brm2.9<-brm(infection~log.last.year.scale+log.this.year.scale+dissection.order.scale+dissection.order.scale2+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.9)

brm2.10<-brm(infection~log.last.year.scale+log.this.year.scale+observer+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.10)

brm2.11<-brm(infection~log.last.year.scale+dissection.order.scale+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.11)


brm2.12<-brm(infection~habitat,family='bernoulli',data=d3,iter=3000)
summary(brm2.12)

brm2.13<-brm(infection~habitat+log.last.year.scale+dissection.order.scale,family='bernoulli',data=d3,iter=3000)
summary(brm2.13)

brm2.14<-brm(infection~habitat+log.last.year.scale+dissection.order.scale+(1|site),family='bernoulli',data=d3,iter=3000)
summary(brm2.14)


brms::pp_check(brm2.5,nsamples=100)

glm1<-glm(infection~habitat,family=binomial,data=d3)
summary(glm1)

inv.logit(0.3417)
inv.logit(0.3417)
inv.logit(0.8147)

glm2<-glm(infection~habitat+log.last.year.scale,family=binomial,data=d3)
summary(glm2)

glm3<-glm(infection~habitat*log.last.year.scale,family=binomial,data=d3)
summary(glm3)

brm2.2<-add_criterion(brm2.2,"waic")
brm2.3<-add_criterion(brm2.3,"waic")
brm2.3.1<-add_criterion(brm2.3.1,"waic")
brm2.4<-add_criterion(brm2.4,"waic")
brm2.5<-add_criterion(brm2.5,"waic")
brm2.6<-add_criterion(brm2.6,"waic")
brm2.7<-add_criterion(brm2.7,"waic")
brm2.8<-add_criterion(brm2.8,"waic")
brm2.9<-add_criterion(brm2.9,"waic")
brm2.10<-add_criterion(brm2.10,"waic")
brm2.11<-add_criterion(brm2.11,"waic")
brm2.14<-add_criterion(brm2.14,"waic")
brmcomp2<-loo_compare(brm2.2,brm2.3,brm2.3.1,brm2.4,brm2.5,brm2.6,brm2.7,brm2.8,brm2.9,brm2.10,brm2.11,brm2.14,criterion = "waic")

print(brmcomp2,simplify = F)
write.csv(brmcomp2,'infectioncomp.csv')

posterior<-as.matrix(brm2.3)
str(posterior)
mcmc_areas(posterior,pars = c("b_Intercept","b_habitatWet",'b_log.this.year.scale','b_log.last.year.scale','b_habitatWet:log.last.year.scale','b_observervincent','b_dissection.order.scale'),
           prob = 0.9, prob_outer = 0.999)+theme(plot.title = element_text(hjust = 0.5))+scale_y_discrete(labels=c(expression(alpha[0]),expression(beta[wet]),expression(alpha[1]),expression(alpha[2]),expression(beta[wet%*%alpha[2]]),expression(beta[vincent]),expression(beta[order])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=20))

posterior<-as.matrix(brm2.2)
str(posterior)
mcmc_areas(posterior,pars = c("b_Intercept","b_habitatWet",'b_log.this.year.scale','b_log.last.year.scale','b_habitatWet:log.last.year.scale'),
           prob = 0.9, prob_outer = 0.999)+theme(plot.title = element_text(hjust = 0.5))+scale_y_discrete(labels=c(expression(alpha[0]),expression(beta[wet]),expression(alpha[1]),expression(alpha[2]),expression(beta[wet%*%alpha[2]])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=20))

posterior<-as.matrix(brm2.4)
str(posterior)
mcmc_areas(posterior,pars = c("b_Intercept","b_habitatWet",'b_log.this.year.scale','b_log.last.year.scale','b_habitatWet:log.last.year.scale','b_dissection.order.scale'),
           prob = 0.9, prob_outer = 0.999)+theme(plot.title = element_text(hjust = 0.5))+scale_y_discrete(labels=c(expression(alpha[0]),expression(beta[wet]),expression(alpha[1]),expression(alpha[2]),expression(beta[wet%*%alpha[2]]),expression(beta[order])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=20))

posterior2.5<-as.matrix(brm2.5)
str(posterior2.5)
mcmc_areas(posterior,pars = c("b_Intercept",'b_log.this.year.scale','b_log.last.year.scale','b_dissection.order.scale'),
           prob = 0.9, prob_outer = 0.999)+theme(plot.title = element_text(hjust = 0.5))+scale_y_discrete(labels=c(expression(alpha[0]),expression(alpha[1]),expression(alpha[2]),expression(beta[order])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=20))


ggplot(d3[d3$observer=='vincent',],aes(x = log.last.year.scale, y =infection,color=habitat ))+geom_count() +stat_smooth(method="glm",method.args = list(family='binomial'))

ggplot(d3[d3$observer=='adam',],aes(x = log.last.year.scale, y =infection,color=habitat ))+geom_count() +stat_smooth(method="glm",method.args = list(family='binomial'))

ggplot(d3,aes(x = log.last.year.scale, y =infection,color=habitat ))+geom_count() +stat_smooth(method="glm",method.args = list(family='binomial'))


ddplot4data<-d3 %>%
  data_grid(log.last.year.scale = seq_range(log.last.year.scale, n = 101),log.this.year.scale=mean(log.this.year.scale),dissection.order.scale=0,habitat=c("Wet",'Dry')) %>%
  add_fitted_draws(brm2.4, n = 1000,re_formula = NA,scale='response') 

ggplot(ddplot4data,aes(x = log.last.year.scale, y =infection,color=habitat,fill=habitat)) + stat_lineribbon(aes(y = .value),.width = c(0, .5,0.8),alpha=0.3)+
  geom_count(data=d3)+xlab(expression(X[t-1]))+ylab('Infection')+theme_classic()+ guides(fill=guide_legend(title="Habitat"),color=guide_legend(title="Habitat"))


ddplot5data<-d3 %>%
  data_grid(log.last.year.scale = seq_range(log.last.year.scale, n = 101),log.this.year.scale=mean(log.this.year.scale),dissection.order.scale=0) %>%
  add_fitted_draws(brm2.5, n = 1000,re_formula = NA,scale='response') 

p1<-ggplot(ddplot5data,aes(x = log.last.year.scale, y =infection)) + stat_lineribbon(aes(y = .value))+geom_count(data=d3)+xlab(expression(X[t-1]))+ylab('Infection')+theme_classic()+scale_fill_brewer()+theme(legend.position = 'none')
p1

ddplot5.1data<-d3 %>%
  data_grid(log.last.year.scale = mean(log.last.year.scale),log.this.year.scale=seq_range(log.this.year.scale,n=101),dissection.order.scale=0) %>%
  add_fitted_draws(brm2.5, n = 1000,re_formula = NA,scale='response') 


p2<-ggplot(ddplot5.1data,aes(x = log.this.year.scale, y =infection)) + stat_lineribbon(aes(y = .value))+geom_count(data=d3)+xlab(expression(X[t]))+ylab('Infection')+theme_classic()+scale_fill_brewer()+ theme(legend.position = "none")

p3<-d3 %>% ggplot(aes(x=habitat,y=infection,fill=habitat))+geom_bar(stat="summary")+geom_errorbar(stat="summary")+xlab("Habitat")+ylab('Infection')+theme_classic()+ theme(legend.position = "none")+ylim(0,1)
p3

legend_p2 <- get_legend(p2 + theme(legend.position="left"))

plot_grid(p1,p3,p2,legend_p2,labels=c("A","B","C"))

