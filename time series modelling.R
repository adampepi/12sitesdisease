rm(list=ls())
library (rstan)
library(loo)
library(tidyverse)
library(tidybayes)
library(bayesplot)
library(cowplot)
library(loo)
library(rstantools)
setwd("~/Documents/Research/dissertation/12 sites analyses/12sitesdisease")

##Data processing
connectivity<-read.csv('connectivity2.csv')
CImat1<-connectivity
CImat<-CImat1[,2:16]
rownames(CImat)<-CImat1[,1]
connectivity2<-read.csv('hanskiconnectivity.csv')
CImat2<-connectivity2
CImat2
HCImat<-CImat2[,2:16]
rownames(HCImat)<-CImat2[,1]
HCImat
str(HCImat)
HCImat<-log(as.matrix(HCImat)+7.015732e-321)##add small constant and log
str(HCImat)
HCImat
connectivity3<-read.csv('connectivityelevation.csv')
CImat3<-connectivity3
ECImat<-CImat3[,2:16]
rownames(ECImat)<-CImat3[,1]
HCImat<-as.matrix(HCImat)
CImat<-as.matrix(CImat)
ECImat<-as.matrix(ECImat)
HCImatscaled<-(HCImat-mean(HCImat))/sd(HCImat)
ECImatscaled<-(ECImat-mean(ECImat))/sd(ECImat)
CImatscaled<-CImat
CImat<-as.matrix(CImatscaled)
CImat<-t(CImat)
CImat<-t(CImat)
HCImat<-as.matrix(HCImatscaled)
ECImat<-as.matrix(ECImatscaled)
CImat
wetdry2<-read.csv('wetdry.csv')
wetdry1<-wetdry2[,2]
weather<-read.csv('weather.csv')
str(weather)
tempscaled<-as.vector(scale(weather$avgtm1))
precipscaled<-as.vector(scale(weather$preciptm1))
datamat<-read.csv("sitesdatamat.csv")
y<-as.matrix(datamat[,2:16])
y
Z.model4<-seq(1,12,by=1)
states<-Z.model4
obsVariances = rep(1, nrow(y))
N = ncol(y)
M = nrow(y)
y<-t(y)
y<-t(y)
N
M
row_indx_pos = matrix((rep(1:M, N)), M, N)[which(!is.na(y))]
col_indx_pos = matrix(sort(rep(1:N, M)), M, N)[which(!is.na(y))]
n_pos = length(row_indx_pos)
y = y[which(!is.na(y))]
y
W=2
wetdry<-wetdry1[1:12]
length(wetdry)

##Load text models

str(y)
fit1<-rstan::stan(file='models/full interaction model.stan',
                  data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                              "states"=states, "S" = max(states), "W"=W,
                              "wetdry"=wetdry,
                              "obsVariances"=obsVariances,
                              "n_obsvar" = max(obsVariances),  
                              "n_pos" = n_pos,
                              "col_indx_pos" = col_indx_pos,
                              "row_indx_pos" = row_indx_pos,
                              "y_int"=round(y)),
                  pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                  iter = 20000, thin = 3, cores=3)
fit2<-rstan::stan(file='models/CI same.stan',
                  data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                              "states"=states, "S" = max(states), "W"=W,
                              "wetdry"=wetdry,
                              "obsVariances"=obsVariances,
                              "n_obsvar" = max(obsVariances),  
                              "n_pos" = n_pos,
                              "col_indx_pos" = col_indx_pos,
                              "row_indx_pos" = row_indx_pos,
                              "y_int"=round(y)),
                  pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                  iter = 20000, thin = 3, cores=3)
fit3<-rstan::stan(file='models/CI temp same.stan',
                  data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                              "states"=states, "S" = max(states), "W"=W,
                              "wetdry"=wetdry,
                              "obsVariances"=obsVariances,
                              "n_obsvar" = max(obsVariances),  
                              "n_pos" = n_pos,
                              "col_indx_pos" = col_indx_pos,
                              "row_indx_pos" = row_indx_pos,
                              "y_int"=round(y)),
                  pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                  iter = 20000, thin = 3, cores=3)
fit4<-rstan::stan(file='models/CI temp precip same.stan',
                  data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                              "states"=states, "S" = max(states), "W"=W,
                              "wetdry"=wetdry,
                              "obsVariances"=obsVariances,
                              "n_obsvar" = max(obsVariances),  
                              "n_pos" = n_pos,
                              "col_indx_pos" = col_indx_pos,
                              "row_indx_pos" = row_indx_pos,
                              "y_int"=round(y)),
                  pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                  iter = 20000, thin = 3, cores=3)
fit5<-rstan::stan(file='models/CI temp precip dd2 same.stan',
                  data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                              "states"=states, "S" = max(states), "W"=W,
                              "wetdry"=wetdry,
                              "obsVariances"=obsVariances,
                              "n_obsvar" = max(obsVariances),  
                              "n_pos" = n_pos,
                              "col_indx_pos" = col_indx_pos,
                              "row_indx_pos" = row_indx_pos,
                              "y_int"=round(y)),
                  pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                  iter = 20000, thin = 3, cores=3)
fit6<-rstan::stan(file='models/CI temp dd2 same.stan',
                  data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                              "states"=states, "S" = max(states), "W"=W,
                              "wetdry"=wetdry,
                              "obsVariances"=obsVariances,
                              "n_obsvar" = max(obsVariances),  
                              "n_pos" = n_pos,
                              "col_indx_pos" = col_indx_pos,
                              "row_indx_pos" = row_indx_pos,
                              "y_int"=round(y)),
                  pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                  iter = 20000, thin = 3, cores=3)
fit7<-rstan::stan(file='models/dd1 precip vary.stan',
                  data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                              "states"=states, "S" = max(states), "W"=W,
                              "wetdry"=wetdry,
                              "obsVariances"=obsVariances,
                              "n_obsvar" = max(obsVariances),  
                              "n_pos" = n_pos,
                              "col_indx_pos" = col_indx_pos,
                              "row_indx_pos" = row_indx_pos,
                              "y_int"=round(y)),
                  pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                  iter = 20000, thin = 3, cores=3)
fit8<-rstan::stan(file='models/dd2 precip vary.stan',
                  data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                              "states"=states, "S" = max(states), "W"=W,
                              "wetdry"=wetdry,
                              "obsVariances"=obsVariances,
                              "n_obsvar" = max(obsVariances),  
                              "n_pos" = n_pos,
                              "col_indx_pos" = col_indx_pos,
                              "row_indx_pos" = row_indx_pos,
                              "y_int"=round(y)),
                  pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                  iter = 20000, thin = 3, cores=3)
print(fit8)
fit9<-rstan::stan(file='models/dd2 vary.stan',
                  data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                              "states"=states, "S" = max(states), "W"=W,
                              "wetdry"=wetdry,
                              "obsVariances"=obsVariances,
                              "n_obsvar" = max(obsVariances),  
                              "n_pos" = n_pos,
                              "col_indx_pos" = col_indx_pos,
                              "row_indx_pos" = row_indx_pos,
                              "y_int"=round(y)),
                  pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt",'sigma_obs'), chains = 3,
                  iter = 20000, thin = 3, cores=3)
options(max.print=10000)
print(fit9)
summary(fit9)
fit10<-rstan::stan(file='models/dd1 vary.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit11<-rstan::stan(file='models/intercept vary.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
summary(fit11)
fit12<-rstan::stan(file='models/intercept precip vary.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit13<-rstan::stan(file='models/all same.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt","sigma_obs"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
print(fit13)

fit14<-rstan::stan(file='models/dd1 precip CI vary.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)

fit15<-rstan::stan(file='models/dd2 intercept vary.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt",'sigma_obs'), chains = 3,
                   iter = 20000, thin = 3, cores=3)
summary(fit15)

fit15.1<-rstan::stan(file='models/dd2 intercept precip vary.stan',
                     data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                                 "states"=states, "S" = max(states), "W"=W,
                                 "wetdry"=wetdry,
                                 "obsVariances"=obsVariances,
                                 "n_obsvar" = max(obsVariances),  
                                 "n_pos" = n_pos,
                                 "col_indx_pos" = col_indx_pos,
                                 "row_indx_pos" = row_indx_pos,
                                 "y_int"=round(y)),
                     pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                     iter = 20000, thin = 3, cores=3)

fit15.2<-rstan::stan(file='models/dd2 vary CI precip interact.stan',
                     data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                                 "states"=states, "S" = max(states), "W"=W,
                                 "wetdry"=wetdry,
                                 "obsVariances"=obsVariances,
                                 "n_obsvar" = max(obsVariances),  
                                 "n_pos" = n_pos,
                                 "col_indx_pos" = col_indx_pos,
                                 "row_indx_pos" = row_indx_pos,
                                 "y_int"=round(y)),
                     pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt","beta_CIp"), chains = 3,
                     iter = 20000, thin = 3, cores=3)
print(fit15.2)
fit1H<-rstan::stan(file='models/full interaction model.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit2H<-rstan::stan(file='models/CI same.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit3H<-rstan::stan(file='models/CI temp same.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit4H<-rstan::stan(file='models/CI temp precip same.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit5H<-rstan::stan(file='models/CI temp precip dd2 same.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit6H<-rstan::stan(file='models/CI temp dd2 same.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit7H<-rstan::stan(file='models/dd1 precip vary.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit8H<-rstan::stan(file='models/dd2 precip vary.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit9H<-rstan::stan(file='models/dd2 vary.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit10H<-rstan::stan(file='models/dd1 vary.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                    iter = 20000, thin = 3, cores=3)
fit11H<-rstan::stan(file='models/intercept vary.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                    iter = 20000, thin = 3, cores=3)
fit12H<-rstan::stan(file='models/intercept precip vary.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                    iter = 20000, thin = 3, cores=3)
fit13H<-rstan::stan(file='models/all same.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                    iter = 20000, thin = 3, cores=3)

fit14H<-rstan::stan(file='models/dd1 precip CI vary.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                    iter = 20000, thin = 3, cores=3)

fit15H<-rstan::stan(file='models/dd2 intercept vary.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                    iter = 20000, thin = 3, cores=3)

fit15.1H<-rstan::stan(file='models/dd2 intercept precip vary.stan',
                      data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                  "states"=states, "S" = max(states), "W"=W,
                                  "wetdry"=wetdry,
                                  "obsVariances"=obsVariances,
                                  "n_obsvar" = max(obsVariances),  
                                  "n_pos" = n_pos,
                                  "col_indx_pos" = col_indx_pos,
                                  "row_indx_pos" = row_indx_pos,
                                  "y_int"=round(y)),
                      pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                      iter = 20000, thin = 3, cores=3)
fit1E<-rstan::stan(file='models/full interaction model.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit2E<-rstan::stan(file='models/CI same.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit3E<-rstan::stan(file='models/CI temp same.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit4E<-rstan::stan(file='models/CI temp precip same.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit5E<-rstan::stan(file='models/CI temp precip dd2 same.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit6E<-rstan::stan(file='models/CI temp dd2 same.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit7E<-rstan::stan(file='models/dd1 precip vary.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit8E<-rstan::stan(file='models/dd2 precip vary.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit9E<-rstan::stan(file='models/dd2 vary.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit10E<-rstan::stan(file='models/dd1 vary.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                    iter = 20000, thin = 3, cores=3)
fit11E<-rstan::stan(file='models/intercept vary.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                    iter = 20000, thin = 3, cores=3)
fit12E<-rstan::stan(file='models/intercept precip vary.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                    iter = 20000, thin = 3, cores=3)
fit13E<-rstan::stan(file='models/all same.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                    iter = 20000, thin = 3, cores=3)

fit14E<-rstan::stan(file='models/dd1 precip CI vary.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                    iter = 20000, thin = 3, cores=3)

fit15E<-rstan::stan(file='models/dd2 intercept vary.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                    iter = 20000, thin = 3, cores=3)


fit15.1E<-rstan::stan(file='models/dd2 intercept precip vary.stan',
                      data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                  "states"=states, "S" = max(states), "W"=W,
                                  "wetdry"=wetdry,
                                  "obsVariances"=obsVariances,
                                  "n_obsvar" = max(obsVariances),  
                                  "n_pos" = n_pos,
                                  "col_indx_pos" = col_indx_pos,
                                  "row_indx_pos" = row_indx_pos,
                                  "y_int"=round(y)),
                      pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                      iter = 20000, thin = 3, cores=3)
fit16<-rstan::stan(file='models/full dd temp precip.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_temp"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit17<-rstan::stan(file='models/full model no pt interaction.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp"), chains = 3,
                   iter = 20000, thin = 3, cores=3)

fit18<-rstan::stan(file='models/full dd temp.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_temp"), chains = 3,
                   iter = 20000, thin = 3, cores=3)

fit19<-rstan::stan(file='models/full dd precip.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip"), chains = 3,
                   iter = 20000, thin =5, cores=3)
fit20<-rstan::stan(file='models/full model no dd2.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                   iter = 20000, thin = 3, cores=3)

fit21<-rstan::stan(file='models/full dd CI.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","beta_CI"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit22<-rstan::stan(file='models/full dd precip CI.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","beta_precip","beta_CI"), chains = 3,
                   iter = 20000, thin = 3, cores=3)
fit23<-rstan::stan(file='models/full dd temp CI.stan',
                   data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=CImat,
                               "states"=states, "S" = max(states), "W"=W,
                               "wetdry"=wetdry,
                               "obsVariances"=obsVariances,
                               "n_obsvar" = max(obsVariances),  
                               "n_pos" = n_pos,
                               "col_indx_pos" = col_indx_pos,
                               "row_indx_pos" = row_indx_pos,
                               "y_int"=round(y)),
                   pars = c("pred","log_lik", "alpha_0","alpha_1","beta_CI","beta_temp"), chains = 3,
                   iter = 20000, thin = 3, cores=3)

fit16H<-rstan::stan(file='models/full dd temp precip.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_temp"), chains = 3,
                    iter = 20000, thin = 3, cores=3)
fit17H<-rstan::stan(file='models/full model no pt interaction.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp"), chains = 3,
                    iter = 20000, thin = 3, cores=3)

fit18H<-rstan::stan(file='models/full dd temp.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_temp"), chains = 3,
                    iter = 20000, thin = 3, cores=3)

fit19H<-rstan::stan(file='models/full dd precip.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip"), chains = 3,
                    iter = 20000, thin =5, cores=3)
fit20H<-rstan::stan(file='models/full model no dd2.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                    iter = 20000, thin = 3, cores=3)

fit21H<-rstan::stan(file='models/full dd CI.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","beta_CI"), chains = 3,
                    iter = 20000, thin = 3, cores=3)
fit22H<-rstan::stan(file='models/full dd precip CI.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","beta_precip","beta_CI"), chains = 3,
                    iter = 20000, thin = 3, cores=3)
fit23H<-rstan::stan(file='models/full dd temp CI.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=HCImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","beta_CI","beta_temp"), chains = 3,
                    iter = 20000, thin = 3, cores=3)

fit16E<-rstan::stan(file='models/full dd temp precip.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_temp"), chains = 3,
                    iter = 20000, thin = 3, cores=3)
fit17E<-rstan::stan(file='models/full model no pt interaction.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip","beta_CI","beta_temp"), chains = 3,
                    iter = 20000, thin = 3, cores=3)

fit18E<-rstan::stan(file='models/full dd temp.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_temp"), chains = 3,
                    iter = 20000, thin = 3, cores=3)

fit19E<-rstan::stan(file='models/full dd precip.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","alpha_2","beta_precip"), chains = 3,
                    iter = 20000, thin =5, cores=3)
fit20E<-rstan::stan(file='models/full model no dd2.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","beta_precip","beta_CI","beta_temp","beta_pt"), chains = 3,
                    iter = 20000, thin = 3, cores=3)

fit21E<-rstan::stan(file='models/full dd CI.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","beta_CI"), chains = 3,
                    iter = 20000, thin = 3, cores=3)
fit22E<-rstan::stan(file='models/full dd precip CI.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","beta_precip","beta_CI"), chains = 3,
                    iter = 20000, thin = 3, cores=3)
fit23E<-rstan::stan(file='models/full dd temp CI.stan',
                    data = list("N"=N,"M"=M, "y"=y, 'precip'=precipscaled,'temp'=tempscaled, 'CI'=ECImat,
                                "states"=states, "S" = max(states), "W"=W,
                                "wetdry"=wetdry,
                                "obsVariances"=obsVariances,
                                "n_obsvar" = max(obsVariances),  
                                "n_pos" = n_pos,
                                "col_indx_pos" = col_indx_pos,
                                "row_indx_pos" = row_indx_pos,
                                "y_int"=round(y)),
                    pars = c("pred","log_lik", "alpha_0","alpha_1","beta_CI","beta_temp"), chains = 3,
                    iter = 20000, thin = 3, cores=3)


print(fit1, pars=c("alpha_0", "alpha_1", "alpha_2","beta_precip","beta_temp","beta_pt","beta_CI", "lp__"), probs=c(.1,.5,.9))
plot(fit1,pars=c("alpha_0", "alpha_1", "alpha_2","beta_precip",'beta_temp','beta_pt',"beta_CI"))
traceplot(fit1, pars = c("alpha_0", "alpha_1", "alpha_2"), inc_warmup = TRUE, nrow = 2)

posterior <- as.matrix(fit1)

summary(fit11)

str(posterior)
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

mcmc_areas(posterior,
           pars = c("alpha_0[1]","alpha_0[2]"),
           prob = 0.95, prob_outer = 0.999,) +scale_y_discrete(labels=c('Wet',"Dry"))+ggtitle(expression(alpha[0]))+theme(plot.title = element_text(hjust = 0.5))

mcmc_areas(posterior,
           pars = c("alpha_1[1]","alpha_1[2]"),
           prob = 0.95, prob_outer = 0.99,) +scale_y_discrete(labels=c('Wet',"Dry"))+ggtitle(expression(alpha[1]))+theme(plot.title = element_text(hjust = 0.5))


mcmc_areas(posterior,
           pars = c("alpha_2[1]","alpha_2[2]"),
           prob = 0.95, prob_outer = 0.99,)+scale_y_discrete(labels=c('Wet',"Dry"))+ggtitle(expression(alpha[2]))+theme(plot.title = element_text(hjust = 0.5))


mcmc_areas(posterior,
           pars = c("beta_precip[1]","beta_precip[2]"),
           prob = 0.95, prob_outer = 0.99,) +scale_y_discrete(labels=c('Wet',"Dry"))+ggtitle(expression(beta[precip]))+theme(plot.title = element_text(hjust = 0.5))

mcmc_areas(posterior,
           pars = c("beta_temp[1]","beta_temp[2]"),
           prob = 0.95, prob_outer = 0.99,) +scale_y_discrete(labels=c('Wet',"Dry"))+ggtitle(expression(beta[temp]))+theme(plot.title = element_text(hjust = 0.5))


mcmc_areas(posterior,
           pars = c("beta_CI[1]","beta_CI[2]"),
           prob = 0.95, prob_outer = 0.99,) +scale_y_discrete(labels=c('Wet',"Dry"))+ggtitle(expression(beta[CI]))+theme(plot.title = element_text(hjust = 0.5))

mcmc_areas(posterior,
           pars = c("alpha_0"),
           prob = 0.95, prob_outer = 0.999,) +scale_y_discrete(labels=c('Wet',"Dry"))+ggtitle(expression(alpha[0]))+theme(plot.title = element_text(hjust = 0.5))

mcmc_areas(posterior,
           pars = c("alpha_1"),
           prob = 0.95, prob_outer = 0.99,) +scale_y_discrete(labels=c('Wet',"Dry"))+ggtitle(expression(alpha[1]))+theme(plot.title = element_text(hjust = 0.5))


mcmc_areas(posterior,
           pars = c("alpha_2"),
           prob = 0.95, prob_outer = 0.99,)+scale_y_discrete(labels=c('Wet',"Dry"))+ggtitle(expression(alpha[2]))+theme(plot.title = element_text(hjust = 0.5))

mcmc_areas(posterior,
           pars = c("beta_precip"),
           prob = 0.95, prob_outer = 0.99,) +scale_y_discrete(labels=c('Wet',"Dry"))+ggtitle(expression(beta[precip]))+theme(plot.title = element_text(hjust = 0.5))

mcmc_areas(posterior,
           pars = c("beta_temp"),
           prob = 0.95, prob_outer = 0.99,) +scale_y_discrete(labels=c('Wet',"Dry"))+ggtitle(expression(beta[temp]))+theme(plot.title = element_text(hjust = 0.5))


mcmc_areas(posterior,
           pars = c("beta_pt"),
           prob = 0.95, prob_outer = 0.99,) +scale_y_discrete(labels=c('Wet',"Dry"))+ggtitle(expression(beta[temp%*%precip]))+theme(plot.title = element_text(hjust = 0.5))

hist(precipscaled)


mcmc_areas(posterior,
           pars = c("beta_CI"),
           prob = 0.95, prob_outer = 0.99,) +scale_y_discrete(labels=c('Wet',"Dry"))+ggtitle(expression(beta[CI]))+theme(plot.title = element_text(hjust = 0.5))


##Fit 5
posterior5 <- as.matrix(fit5)
mcmc_areas(posterior5,
           pars = c("alpha_0[1]","alpha_0[2]","alpha_1[1]",'alpha_1[2]','alpha_2',"beta_precip",'beta_temp',"beta_pt",'beta_CI'),
           prob = 0.90, prob_outer = 1,) +scale_y_discrete(labels=c(expression(alpha[paste(0,' , ', "wet")]),expression(alpha[paste(0,' , ', "dry")]),expression(alpha[paste(1,' , ', "wet")]),expression(alpha[paste(1,' , ', "dry")]),expression(alpha[2]),expression(beta[precip]),expression(beta[temp]),expression(beta[temp%*%precip]),expression(beta[connectivity])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=20))



posterior15.2 <- as.matrix(fit15.2)
mcmc_areas(posterior15.2,
           pars = c("alpha_0","alpha_1",'alpha_2[1]','alpha_2[2]',"beta_precip",'beta_temp',"beta_pt",'beta_CI','beta_CIp'),
           prob = 0.90, prob_outer = 1,) +scale_y_discrete(labels=c(expression(alpha[0]),expression(alpha[1]),expression(alpha[paste(2,' , ', "wet")]),expression(alpha[paste(2,' , ', "dry")]),expression(beta[precip]),expression(beta[temp]),expression(beta[temp%*%precip]),expression(beta[connectivity]),expression(beta[CI%*%precip])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=20))

posterior8 <- as.matrix(fit8)
mcmc_areas(posterior8,
           pars = c("alpha_0","alpha_1",'alpha_2[1]','alpha_2[2]',"beta_precip[1]","beta_precip[2]",'beta_temp',"beta_pt",'beta_CI'),
           prob = 0.90, prob_outer = 1,) +scale_y_discrete(labels=c(expression(alpha[0]),expression(alpha[1]),expression(alpha[paste(2,' , ', "wet")]),expression(alpha[paste(2,' , ', "dry")]),expression(beta[paste('precip',' , ', "wet")]),expression(beta[paste('precip',' , ', "dry")]),expression(beta[temp]),expression(beta[temp%*%precip]),expression(beta[connectivity])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=20))

posterior9 <- as.matrix(fit9)
p9<-mcmc_areas(posterior9,
               pars = c("alpha_0","alpha_1",'alpha_2[1]','alpha_2[2]',"beta_precip",'beta_temp',"beta_pt",'beta_CI'),
               prob = 0.90, prob_outer = 1,) +scale_y_discrete(labels=c(expression(alpha[0]),expression(alpha[1]),expression(alpha[paste(2,' , ', "wet")]),expression(alpha[paste(2,' , ', "dry")]),expression(beta[precip]),expression(beta[temp]),expression(beta[temp%*%precip]),expression(beta[connectivity])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=15))+ylab('Parameter')+xlim(-1,5)
posterior11 <- as.matrix(fit11)
P11<-mcmc_areas(posterior11,
           pars = c("alpha_0[1]","alpha_0[2]","alpha_1",'alpha_2',"beta_precip",'beta_temp',"beta_pt",'beta_CI'),
           prob = 0.90, prob_outer = 1,) +scale_y_discrete(labels=c(expression(alpha[paste(0,' , ', "wet")]),expression(alpha[paste(0,' , ', "dry")]),expression(alpha[1]),expression(alpha[2]),expression(beta[precip]),expression(beta[temp]),expression(beta[temp%*%precip]),expression(beta[connectivity])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=15))+xlab("Standardized Value")+ylab('Parameter')+xlim(-1,5)

plot_grid(p9,P11,labels=c('a','b'),nrow=2)

posterior12 <- as.matrix(fit12)
mcmc_areas(posterior12,
           pars = c("alpha_0[1]","alpha_0[2]","alpha_1",'alpha_2',"beta_precip[1]","beta_precip[2]",'beta_temp',"beta_pt",'beta_CI'),
           prob = 0.90, prob_outer = 1,) +scale_y_discrete(labels=c(expression(alpha[paste(0,' , ', "wet")]),expression(alpha[paste(0,' , ', "dry")]),expression(alpha[1]),expression(alpha[2]),expression(beta[paste("precip",' , ', "wet")]),expression(beta[paste("precip",' , ', "dry")]),expression(beta[temp]),expression(beta[temp%*%precip]),expression(beta[connectivity])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=20))

posterior15 <- as.matrix(fit15)
mcmc_areas(posterior15,
           pars = c("alpha_0[1]","alpha_0[2]",'alpha_1','alpha_2[1]','alpha_2[2]',"beta_precip",'beta_temp',"beta_pt",'beta_CI'),
           prob = 0.90, prob_outer = 1,) +scale_y_discrete(labels=c(expression(alpha[paste(0,' , ', "wet")]),expression(alpha[paste(0,' , ', "dry")]),expression(alpha[1]),expression(alpha[paste(2,' , ', "wet")]),expression(alpha[paste(2,' , ', "dry")]),expression(beta[precip]),expression(beta[temp]),expression(beta[temp%*%precip]),expression(beta[connectivity])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=20))
comptable
posterior15.1 <- as.matrix(fit15.1)
mcmc_areas(posterior15.1,
           pars = c("alpha_0[1]","alpha_0[2]",'alpha_1','alpha_2[1]','alpha_2[2]',"beta_precip[1]","beta_precip[2]",'beta_temp',"beta_pt",'beta_CI'),
           prob = 0.90, prob_outer = 1,) +scale_y_discrete(labels=c(expression(alpha[paste(0,' , ', "wet")]),expression(alpha[paste(0,' , ', "dry")]),expression(alpha[1]),expression(alpha[paste(2,' , ', "wet")]),expression(alpha[paste(2,' , ', "dry")]),expression(beta[paste('precip',',','wet')]),expression(beta[paste('precip',',','dry')]),expression(beta[temp]),expression(beta[temp%*%precip]),expression(beta[connectivity])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=20))

posterior13 <- as.matrix(fit13)
mcmc_areas(posterior13,
           pars = c("alpha_0","alpha_1",'alpha_2',"beta_precip",'beta_temp',"beta_pt",'beta_CI'),
           prob = 0.90, prob_outer = 1,) +scale_y_discrete(labels=c(expression(alpha[0]),expression(alpha[1]),expression(alpha[2]),expression(beta[precip]),expression(beta[temp]),expression(beta[temp%*%precip]),expression(beta[connectivity])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=20))


##### Get posterior intervals

posterior_interval(posterior9, prob=0.9, pars = c("alpha_0","alpha_1",'alpha_2',"beta_precip",'beta_temp',"beta_pt",'beta_CI'))

posterior_interval(posterior11, prob=0.9, pars = c("alpha_0","alpha_1",'alpha_2',"beta_precip",'beta_temp',"beta_pt",'beta_CI'))
posterior_interval(posterior12, prob=0.9, pars = c("alpha_0","alpha_1",'alpha_2',"beta_precip",'beta_temp',"beta_pt",'beta_CI'))
summary(fit12)


####Get WAICs


log_lik1 <-extract_log_lik(fit1, merge_chains = FALSE)
nrow(log_lik1)
waic1<-loo::waic(log_lik1)
rel_n_eff1 <- relative_eff(exp(log_lik1), chain_id = 1:3)
loo1<-loo(log_lik1, r_eff = rel_n_eff1, cores = 2)

log_lik2 <-extract_log_lik(fit2, merge_chains = FALSE)
waic2<-loo::waic(log_lik2)
rel_n_eff2 <- relative_eff(exp(log_lik2))
loo2<-loo(log_lik2, r_eff = rel_n_eff2, cores = 2)

log_lik3 <-extract_log_lik(fit3, merge_chains = FALSE)
waic3<-loo::waic(log_lik3)
rel_n_eff3 <- relative_eff(exp(log_lik3))
loo3<-loo(log_lik3, r_eff = rel_n_eff3, cores = 2)

log_lik4 <-extract_log_lik(fit4, merge_chains = FALSE)
waic4<-loo::waic(log_lik4)
rel_n_eff4 <- relative_eff(exp(log_lik4))
loo4<-loo(log_lik4, r_eff = rel_n_eff4, cores = 2)

log_lik5 <-extract_log_lik(fit5, merge_chains = FALSE)
waic5<-loo::waic(log_lik5)
rel_n_eff5 <- relative_eff(exp(log_lik5))
loo5<-loo(log_lik5, r_eff = rel_n_eff5, cores = 2)

log_lik6 <-extract_log_lik(fit6, merge_chains = FALSE)
waic6<-loo::waic(log_lik6)
rel_n_eff6 <- relative_eff(exp(log_lik6))
loo6<-loo(log_lik6, r_eff = rel_n_eff6, cores = 2)

log_lik7 <-extract_log_lik(fit7, merge_chains = FALSE)
waic7<-loo::waic(log_lik7)
rel_n_eff7 <- relative_eff(exp(log_lik7))
loo7<-loo(log_lik7, r_eff = rel_n_eff7, cores = 2)

log_lik8 <-extract_log_lik(fit8, merge_chains = FALSE)
waic8<-loo::waic(log_lik8)
rel_n_eff8 <- relative_eff(exp(log_lik8))
loo8<-loo(log_lik8, r_eff = rel_n_eff8, cores = 2,save_psis = T)


log_lik9 <-extract_log_lik(fit9, merge_chains = FALSE)
waic9<-loo::waic(log_lik9)
rel_n_eff9 <- relative_eff(exp(log_lik9))
loo9<-loo(log_lik9, r_eff = rel_n_eff9, cores = 2,save_psis = T)



log_lik10 <-extract_log_lik(fit10, merge_chains = FALSE)
waic10<-loo::waic(log_lik10)
rel_n_eff10 <- relative_eff(exp(log_lik10))
loo10<-loo(log_lik10, r_eff = rel_n_eff10, cores = 2)

log_lik11 <-extract_log_lik(fit11, merge_chains = FALSE)
waic11<-loo::waic(log_lik11)
rel_n_eff11 <- relative_eff(exp(log_lik11))
loo11<-loo(log_lik11, r_eff = rel_n_eff11, cores = 2)

log_lik12 <-extract_log_lik(fit12, merge_chains = FALSE)
waic12<-loo::waic(log_lik12)
rel_n_eff12 <- relative_eff(exp(log_lik12))
loo12<-loo(log_lik12, r_eff = rel_n_eff12, cores = 2)

log_lik13 <-extract_log_lik(fit13, merge_chains = FALSE)
waic13<-loo::waic(log_lik13)
rel_n_eff13 <- relative_eff(exp(log_lik13))
loo13<-loo(log_lik13, r_eff = rel_n_eff13, cores = 2)

log_lik14 <-extract_log_lik(fit14, merge_chains = FALSE)
waic14<-loo::waic(log_lik14)
rel_n_eff14 <- relative_eff(exp(log_lik14))
loo14<-loo(log_lik14, r_eff = rel_n_eff14, cores = 2)

log_lik15 <-extract_log_lik(fit15, merge_chains = FALSE)
waic15<-loo::waic(log_lik15)
rel_n_eff15 <- relative_eff(exp(log_lik15))
loo15<-loo(log_lik15, r_eff = rel_n_eff15, cores = 2)

log_lik15.1<-extract_log_lik(fit15.1, merge_chains = FALSE)
waic15.1<-loo::waic(log_lik15.1)
rel_n_eff15.1 <- relative_eff(exp(log_lik15.1))
loo15.1<-loo(log_lik15.1, r_eff = rel_n_eff15.1, cores = 2)

log_lik1H <-extract_log_lik(fit1H, merge_chains = FALSE)
waic1H<-loo::waic(log_lik1H)
rel_n_eff1H <- relative_eff(exp(log_lik1H), chain_id = 1:3)
loo1H<-loo(log_lik1H, r_eff = rel_n_eff1H, cores = 2)

log_lik2H <-extract_log_lik(fit2H, merge_chains = FALSE)
waic2H<-loo::waic(log_lik2H)
rel_n_eff2H <- relative_eff(exp(log_lik2H))
loo2H<-loo(log_lik2H, r_eff = rel_n_eff2H, cores = 2)

log_lik3H <-extract_log_lik(fit3H, merge_chains = FALSE)
waic3H<-loo::waic(log_lik3H)
rel_n_eff3H <- relative_eff(exp(log_lik3H))
loo3H<-loo(log_lik3H, r_eff = rel_n_eff3H, cores = 2)

log_lik4H <-extract_log_lik(fit4H, merge_chains = FALSE)
waic4H<-loo::waic(log_lik4H)
rel_n_eff4H <- relative_eff(exp(log_lik4H))
loo4H<-loo(log_lik4H, r_eff = rel_n_eff4H, cores = 2)

log_lik5H <-extract_log_lik(fit5H, merge_chains = FALSE)
waic5H<-loo::waic(log_lik5H)
rel_n_eff5H <- relative_eff(exp(log_lik5H))
loo5H<-loo(log_lik5H, r_eff = rel_n_eff5H, cores = 2)

log_lik6H <-extract_log_lik(fit6H, merge_chains = FALSE)
waic6H<-loo::waic(log_lik6H)
rel_n_eff6H <- relative_eff(exp(log_lik6H))
loo6H<-loo(log_lik6H, r_eff = rel_n_eff6H, cores = 2)

log_lik7H <-extract_log_lik(fit7H, merge_chains = FALSE)
waic7H<-loo::waic(log_lik7H)
rel_n_eff7H <- relative_eff(exp(log_lik7H))
loo7H<-loo(log_lik7H, r_eff = rel_n_eff7H, cores = 2)

log_lik8H <-extract_log_lik(fit8H, merge_chains = FALSE)
waic8H<-loo::waic(log_lik8H)
rel_n_eff8H <- relative_eff(exp(log_lik8H))
loo8H<-loo(log_lik8H, r_eff = rel_n_eff8H, cores = 2)

log_lik9H <-extract_log_lik(fit9H, merge_chains = FALSE)
waic9H<-loo::waic(log_lik9H)
rel_n_eff9H <- relative_eff(exp(log_lik9H))
loo9H<-loo(log_lik9H, r_eff = rel_n_eff9H, cores = 2)

log_lik10H <-extract_log_lik(fit10H, merge_chains = FALSE)
waic10H<-loo::waic(log_lik10H)
rel_n_eff10H <- relative_eff(exp(log_lik10H))
loo10H<-loo(log_lik10H, r_eff = rel_n_eff10H, cores = 2)

log_lik11H <-extract_log_lik(fit11H, merge_chains = FALSE)
waic11H<-loo::waic(log_lik11H)
rel_n_eff11H <- relative_eff(exp(log_lik11H))
loo11H<-loo(log_lik11H, r_eff = rel_n_eff11H, cores = 2)

log_lik12H <-extract_log_lik(fit12H, merge_chains = FALSE)
waic12H<-loo::waic(log_lik12H)
rel_n_eff12H <- relative_eff(exp(log_lik12H))
loo12H<-loo(log_lik12H, r_eff = rel_n_eff12H, cores = 2)

log_lik13H <-extract_log_lik(fit13H, merge_chains = FALSE)
waic13H<-loo::waic(log_lik13H)
rel_n_eff13H <- relative_eff(exp(log_lik13H))
loo13H<-loo(log_lik13H, r_eff = rel_n_eff13H, cores = 2)

log_lik14H <-extract_log_lik(fit14H, merge_chains = FALSE)
waic14H<-loo::waic(log_lik14H)
rel_n_eff14H <- relative_eff(exp(log_lik14H))
loo14H<-loo(log_lik14H, r_eff = rel_n_eff14H, cores = 2)

log_lik15H <-extract_log_lik(fit15H, merge_chains = FALSE)
waic15H<-loo::waic(log_lik15H)
rel_n_eff15H <- relative_eff(exp(log_lik15H))
loo15H<-loo(log_lik15H, r_eff = rel_n_eff15H, cores = 2)

log_lik15.1H <-extract_log_lik(fit15.1H, merge_chains = FALSE)
waic15.1H<-loo::waic(log_lik15.1H)
rel_n_eff15.1H <- relative_eff(exp(log_lik15.1H))
loo15.1H<-loo(log_lik15.1H, r_eff = rel_n_eff15.1H, cores = 2)

log_lik1E <-extract_log_lik(fit1E, merge_chains = FALSE)
waic1E<-loo::waic(log_lik1E)
rel_n_eff1E <- relative_eff(exp(log_lik1E), chain_id = 1:3)
loo1E<-loo(log_lik1E, r_eff = rel_n_eff1E, cores = 2)

log_lik2E <-extract_log_lik(fit2E, merge_chains = FALSE)
waic2E<-loo::waic(log_lik2E)
rel_n_eff2E <- relative_eff(exp(log_lik2E))
loo2E<-loo(log_lik2E, r_eff = rel_n_eff2E, cores = 2)

log_lik3E <-extract_log_lik(fit3E, merge_chains = FALSE)
waic3E<-loo::waic(log_lik3E)
rel_n_eff3E <- relative_eff(exp(log_lik3E))
loo3E<-loo(log_lik3E, r_eff = rel_n_eff3E, cores = 2)

log_lik4E <-extract_log_lik(fit4E, merge_chains = FALSE)
waic4E<-loo::waic(log_lik4E)
rel_n_eff4E <- relative_eff(exp(log_lik4E))
loo4E<-loo(log_lik4E, r_eff = rel_n_eff4E, cores = 2)

log_lik5E <-extract_log_lik(fit5E, merge_chains = FALSE)
waic5E<-loo::waic(log_lik5E)
rel_n_eff5E <- relative_eff(exp(log_lik5E))
loo5E<-loo(log_lik5E, r_eff = rel_n_eff5E, cores = 2)

log_lik6E <-extract_log_lik(fit6E, merge_chains = FALSE)
waic6E<-loo::waic(log_lik6E)
rel_n_eff6E <- relative_eff(exp(log_lik6E))
loo6E<-loo(log_lik6E, r_eff = rel_n_eff6E, cores = 2)

log_lik7E <-extract_log_lik(fit7E, merge_chains = FALSE)
waic7E<-loo::waic(log_lik7E)
rel_n_eff7E <- relative_eff(exp(log_lik7E))
loo7E<-loo(log_lik7E, r_eff = rel_n_eff7E, cores = 2)

log_lik8E <-extract_log_lik(fit8E, merge_chains = FALSE)
waic8E<-loo::waic(log_lik8E)
rel_n_eff8E <- relative_eff(exp(log_lik8E))
loo8E<-loo(log_lik8E, r_eff = rel_n_eff8E, cores = 2)

log_lik9E <-extract_log_lik(fit9E, merge_chains = FALSE)
waic9E<-loo::waic(log_lik9E)
rel_n_eff9E <- relative_eff(exp(log_lik9E))
loo9E<-loo(log_lik9E, r_eff = rel_n_eff9E, cores = 2)

log_lik10E <-extract_log_lik(fit10E, merge_chains = FALSE)
waic10E<-loo::waic(log_lik10E)
rel_n_eff10E <- relative_eff(exp(log_lik10E))
loo10E<-loo(log_lik10E, r_eff = rel_n_eff10E, cores = 2)

log_lik11E <-extract_log_lik(fit11E, merge_chains = FALSE)
waic11E<-loo::waic(log_lik11E)
rel_n_eff11E <- relative_eff(exp(log_lik11E))
loo11E<-loo(log_lik11E, r_eff = rel_n_eff11E, cores = 2)

log_lik12E <-extract_log_lik(fit12E, merge_chains = FALSE)
waic12E<-loo::waic(log_lik12E)
rel_n_eff12E <- relative_eff(exp(log_lik12E))
loo12E<-loo(log_lik12E, r_eff = rel_n_eff12E, cores = 2)

log_lik13E <-extract_log_lik(fit13E, merge_chains = FALSE)
waic13E<-loo::waic(log_lik13E)
rel_n_eff13E <- relative_eff(exp(log_lik13E))
loo13E<-loo(log_lik13E, r_eff = rel_n_eff13E, cores = 2)

log_lik14E <-extract_log_lik(fit14E, merge_chains = FALSE)
waic14E<-loo::waic(log_lik14E)
rel_n_eff14E <- relative_eff(exp(log_lik14E))
loo14E<-loo(log_lik14E, r_eff = rel_n_eff14E, cores = 2)

log_lik15E <-extract_log_lik(fit15E, merge_chains = FALSE)
waic15E<-loo::waic(log_lik15E)
rel_n_eff15E <- relative_eff(exp(log_lik15E))
loo15E<-loo(log_lik15E, r_eff = rel_n_eff15E, cores = 2)

log_lik15.1E <-extract_log_lik(fit15.1E, merge_chains = FALSE)
waic15.1E<-loo::waic(log_lik15.1E)
rel_n_eff15.1E <- relative_eff(exp(log_lik15.1E))
loo15.1E<-loo(log_lik15.1E, r_eff = rel_n_eff15.1E, cores = 2)

log_lik16 <-extract_log_lik(fit16, merge_chains = FALSE)
waic16<-loo::waic(log_lik16)
rel_n_eff16 <- relative_eff(exp(log_lik16))
loo16<-loo(log_lik16, r_eff = rel_n_eff16, cores = 2)


log_lik17 <-extract_log_lik(fit17, merge_chains = FALSE)
waic17<-loo::waic(log_lik17)
rel_n_eff17 <- relative_eff(exp(log_lik17))
loo17<-loo(log_lik17, r_eff = rel_n_eff17, cores = 2)

log_lik18 <-extract_log_lik(fit18, merge_chains = FALSE)
waic18<-loo::waic(log_lik18)
rel_n_eff18 <- relative_eff(exp(log_lik18))
loo18<-loo(log_lik18, r_eff = rel_n_eff18, cores = 2)

log_lik19 <-extract_log_lik(fit19, merge_chains = FALSE)
waic19<-loo::waic(log_lik19)
rel_n_eff19 <- relative_eff(exp(log_lik19))
loo19<-loo(log_lik19, r_eff = rel_n_eff19, cores = 2)

log_lik20 <-extract_log_lik(fit20, merge_chains = FALSE)
waic20<-loo::waic(log_lik20)
rel_n_eff20 <- relative_eff(exp(log_lik20))
loo20<-loo(log_lik20, r_eff = rel_n_eff20, cores = 2)

log_lik21 <-extract_log_lik(fit21, merge_chains = FALSE)
waic21<-loo::waic(log_lik21)
rel_n_eff21 <- relative_eff(exp(log_lik21))
loo21<-loo(log_lik21, r_eff = rel_n_eff21, cores = 2)

log_lik22 <-extract_log_lik(fit22, merge_chains = FALSE)
waic22<-loo::waic(log_lik22)
rel_n_eff22 <- relative_eff(exp(log_lik22))
loo22<-loo(log_lik22, r_eff = rel_n_eff22, cores = 2)

log_lik23 <-extract_log_lik(fit23, merge_chains = FALSE)
waic23<-loo::waic(log_lik23)
rel_n_eff23 <- relative_eff(exp(log_lik23))
loo23<-loo(log_lik23, r_eff = rel_n_eff23, cores = 2)

log_lik16H <-extract_log_lik(fit16H, merge_chains = FALSE)
waic16H<-loo::waic(log_lik16H)
rel_n_eff16H <- relative_eff(exp(log_lik16H))
loo16H<-loo(log_lik16H, r_eff = rel_n_eff16H, cores = 2)


log_lik17H <-extract_log_lik(fit17H, merge_chains = FALSE)
waic17H<-loo::waic(log_lik17H)
rel_n_eff17H <- relative_eff(exp(log_lik17H))
loo17H<-loo(log_lik17H, r_eff = rel_n_eff17H, cores = 2)

log_lik18H <-extract_log_lik(fit18H, merge_chains = FALSE)
waic18H<-loo::waic(log_lik18H)
rel_n_eff18H <- relative_eff(exp(log_lik18H))
loo18H<-loo(log_lik18H, r_eff = rel_n_eff18H, cores = 2)

log_lik19H <-extract_log_lik(fit19H, merge_chains = FALSE)
waic19H<-loo::waic(log_lik19H)
rel_n_eff19H <- relative_eff(exp(log_lik19H))
loo19H<-loo(log_lik19H, r_eff = rel_n_eff19H, cores = 2)

log_lik20H <-extract_log_lik(fit20H, merge_chains = FALSE)
waic20H<-loo::waic(log_lik20H)
rel_n_eff20H <- relative_eff(exp(log_lik20H))
loo20H<-loo(log_lik20H, r_eff = rel_n_eff20H, cores = 2)

log_lik21H <-extract_log_lik(fit21H, merge_chains = FALSE)
waic21H<-loo::waic(log_lik21H)
rel_n_eff21H <- relative_eff(exp(log_lik21H))
loo21H<-loo(log_lik21H, r_eff = rel_n_eff21H, cores = 2)

log_lik22H <-extract_log_lik(fit22H, merge_chains = FALSE)
waic22H<-loo::waic(log_lik22H)
rel_n_eff22H <- relative_eff(exp(log_lik22H))
loo22H<-loo(log_lik22H, r_eff = rel_n_eff22H, cores = 2)

log_lik23H <-extract_log_lik(fit23H, merge_chains = FALSE)
waic23H<-loo::waic(log_lik23H)
rel_n_eff23H <- relative_eff(exp(log_lik23H))
loo23H<-loo(log_lik23H, r_eff = rel_n_eff23H, cores = 2)


log_lik16E <-extract_log_lik(fit16E, merge_chains = FALSE)
waic16E<-loo::waic(log_lik16E)
rel_n_eff16E <- relative_eff(exp(log_lik16E))
loo16E<-loo(log_lik16E, r_eff = rel_n_eff16E, cores = 2)


log_lik17E <-extract_log_lik(fit17E, merge_chains = FALSE)
waic17E<-loo::waic(log_lik17E)
rel_n_eff17E <- relative_eff(exp(log_lik17E))
loo17E<-loo(log_lik17E, r_eff = rel_n_eff17E, cores = 2)

log_lik18E <-extract_log_lik(fit18E, merge_chains = FALSE)
waic18E<-loo::waic(log_lik18E)
rel_n_eff18E <- relative_eff(exp(log_lik18E))
loo18E<-loo(log_lik18E, r_eff = rel_n_eff18E, cores = 2)

log_lik19E <-extract_log_lik(fit19E, merge_chains = FALSE)
waic19E<-loo::waic(log_lik19E)
rel_n_eff19E <- relative_eff(exp(log_lik19E))
loo19E<-loo(log_lik19E, r_eff = rel_n_eff19E, cores = 2)

log_lik20E <-extract_log_lik(fit20E, merge_chains = FALSE)
waic20E<-loo::waic(log_lik20E)
rel_n_eff20E <- relative_eff(exp(log_lik20E))
loo20E<-loo(log_lik20E, r_eff = rel_n_eff20E, cores = 2)

log_lik21E <-extract_log_lik(fit21E, merge_chains = FALSE)
waic21E<-loo::waic(log_lik21E)
rel_n_eff21E <- relative_eff(exp(log_lik21E))
loo21E<-loo(log_lik21E, r_eff = rel_n_eff21E, cores = 2)

log_lik22E <-extract_log_lik(fit22E, merge_chains = FALSE)
waic22E<-loo::waic(log_lik22E)
rel_n_eff22E <- relative_eff(exp(log_lik22E))
loo22E<-loo(log_lik22E, r_eff = rel_n_eff22E, cores = 2)

log_lik23E <-extract_log_lik(fit23E, merge_chains = FALSE)
waic23E<-loo::waic(log_lik23E)
rel_n_eff23E <- relative_eff(exp(log_lik23E))
loo23E<-loo(log_lik23E, r_eff = rel_n_eff23E, cores = 2)

##Model comparisons

comp1 <- loo_compare(waic1,waic2,waic3,waic4,waic5,waic6,waic7,waic8,waic9,waic10,waic11, waic12,waic13,waic14,waic15,waic15.1)

comp2 <- loo_compare(loo1,loo2,loo3,loo4,loo5,loo6,loo7,loo8,loo9,loo10,loo11,loo12,loo13,loo14,loo15)

comp3 <- loo_compare(waic1,waic2,waic3,waic4,waic5,waic6,waic7,waic8,waic9,waic10,waic11, waic12,waic13,waic14,waic15,waic1H,waic2H,waic3H,waic4H,waic5H,waic6H,waic7H,waic8H,waic9H,waic10H,waic11H, waic12H,waic13H,waic14H,waic15H)

comp4 <- loo_compare(loo1,loo2,loo3,loo4,loo5,loo6,loo7,loo8,loo9,loo10,loo11, loo12,loo13,loo14,loo15,loo1H,loo2H,loo3H,loo4H,loo5H,loo6H,loo7H,loo8H,loo9H,loo10H,loo11H, loo12H,loo13H,loo14H,loo15H)
comp5 <- loo_compare(waic1H,waic2H,waic3H,waic4H,waic5H,waic6H,waic7H,waic8H,waic9H,waic10H,waic11H, waic12H,waic13H,waic14H,waic15H,waic15.1H)

comp6 <- loo_compare(waic1E,waic2E,waic3E,waic4E,waic5E,waic6E,waic7E,waic8E,waic9E,waic10E,waic11E, waic12E,waic13E,waic14E,waic15E,waic15.1E)

print(comp1, digits = 2,simplify = F)
print(comp2, digits = 2,simplify = F)
print(comp3, digits = 2,simplify = F)
print(comp4, digits = 2,simplify = F)


comptable<-data.frame(waic=comp1[,7])
comptable$delta_waic<-comptable$waic-comptable$waic[1]


comp5.1<-as.data.frame(comp5)
str(comp5.1)
comp5.2<-comp5.1[match(rownames(comptable),rownames(comp5.1)),]
comp5.2
comp6.1<-as.data.frame(comp6)
str(comp6.1)
comp6.2<-comp6.1[match(rownames(comptable),rownames(comp6.1)),]
comp6.2

comptable$waicH<-comp5.2[,7]
comptable$waicE<-comp6.2[,7]

comptable$dH<-comptable$waicH-comptable$waic
comptable$dE<-comptable$waicE-comptable$waic
comptable$model<-rownames(comptable)
print(comptable)

write_csv(comptable,'comptable_matscaled3.csv')

###Selection of base model


comp1.1 <- loo_compare(waic13,waic16,waic17,waic18,waic19,waic20,waic21,waic22,waic23)

comp2.1 <- loo_compare(loo13,loo16,loo17,loo18,loo19,loo20,loo21,loo22,loo23)

print(comp1.1, digits = 2)

print(comp2.1, digits = 2)

comptable1<-data.frame(waic=comp1.1[,7])
comptable1$delta_waic<-comptable1$waic-comptable1$waic[1]



comp1.1H <- loo_compare(waic13H,waic16H,waic17H,waic18H,waic19H,waic20H,waic21H,waic22H,waic23H)


comp1.1H.1<-as.data.frame(comp1.1H)
str(comp1.1H.1)
comp1.1H.2<-comp1.1H.1[match(rownames(comptable1),rownames(comp1.1H.1)),]
comp1.1H.2

comp1.1E <- loo_compare(waic13E,waic16E,waic17E,waic18E,waic19E,waic20E,waic21E,waic22E,waic23E)


comp1.1E.1<-as.data.frame(comp1.1E)
str(comp1.1E.1)
comp1.1E.2<-comp1.1E.1[match(rownames(comptable1),rownames(comp1.1E.1)),]
comp1.1E.2


comptable1$waicH<-comp1.1H.2[,7]
comptable1$dH<-comptable1$waicH-comptable1$waic
comptable1$waicE<-comp1.1E.2[,7]
comptable1$dE<-comptable1$waicE-comptable1$waic
comptable1$model<-rownames(comptable1)
print(comptable1)

write.table(comptable1,"modelcompstruct3.csv")


#### Posterior predicitive simulation of interaction between temp and precip

##Using model 9

p9<-as.matrix(fit9)
str(p9)

print(fit12, pars=c("alpha_0", "alpha_1", "alpha_2","beta_precip","beta_temp","beta_pt","beta_CI", "lp__"), probs=c(.1,.5,.9))

print(fit13, pars=c("alpha_0", "alpha_1", "alpha_2","beta_precip","beta_temp","beta_pt","beta_CI", "lp__"), probs=c(.1,.5,.9))
mean(y)
alpha_0<-(3.44+2.04)/2
alpha_1<-0.2
alpha_2<--.66
beta_precip<-0.66
beta_temp<-0.08
beta_pt<-0.69
beta_CI<-0.20
tbar<-mean(tempscaled)
tmin<-min(tempscaled)
tmin=-1
tmax<-max(tempscaled)
tmax=1
CIbar<-mean(CImat)
xbar<-mean(y)

pbar<-mean(precipscaled)
pmin<-min(precipscaled)
pmin<--1
pmax<-max(precipscaled)
pmax<-1

precips<-seq(from=min(precipscaled),to=max(precipscaled),by=0.01)

ppred1<-data.frame(Precipitation=precips,temp='Mean',Predicted=0,Draw=0)
ppred2<-data.frame(Precipitation=precips,temp='Min',Predicted=0,Draw=0)
ppred3<-data.frame(Precipitation=precips,temp='Max',Predicted=0,Draw=0)

for(i in 1:length(precips)){
  ppred1[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*precips[i] + beta_temp*tbar + beta_CI*CIbar + beta_pt*precips[i]*tbar
  ppred2[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*precips[i] + beta_temp*tmin + beta_CI*CIbar + beta_pt*precips[i]*tmin
  ppred3[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*precips[i] + beta_temp*tmax + beta_CI*CIbar + beta_pt*precips[i]*tmax
}

ppred<-rbind(ppred1,ppred2,ppred3)

str(ppred)

ggplot(data=ppred,aes(x=Precipitation,y=Predicted,color=temp))+geom_line()

##With temperature

matplot(datamat,type='l')

temps<-seq(from=min(tempscaled),to=max(precipscaled),by=0.01)

tpred1<-data.frame(Temperature=temps,precip='Mean',Predicted=0)
tpred2<-data.frame(Temperature=temps,precip='Min',Predicted=0)
tpred3<-data.frame(Temperature=temps,precip='Max',Predicted=0)

for(i in 1:length(temps)){
  tpred1[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*pbar + beta_temp*temps[i] + beta_CI*CIbar + beta_pt*pbar*temps[i]
  tpred2[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*pmin + beta_temp*temps[i] + beta_CI*CIbar + beta_pt*pmin*temps[i]
  tpred3[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*pmin + beta_temp*temps[i] + beta_CI*CIbar + beta_pt*pmax*temps[i]
}

tpred<-rbind(tpred1,tpred2,tpred3)

str(tpred)

ggplot(data=tpred,aes(x=Temperature,y=Predicted,color=precip))+geom_line()



str(p9)
sample()
str(fit9)

pfit9<-rstan::extract(fit9)
str(pfit9)

hist(pfit11$beta_precip)
##version with simulations

spread_draws(p11)
extract_samples(fit11)


preciplines<-function(
  alpha_0=sample(pfit9$alpha_0,size=1),
  alpha_1=sample(pfit9$alpha_1,size=1),
  alpha_2=sample(pfit9$alpha_2,size=1),
  beta_precip=sample(pfit9$beta_precip,size=1),
  beta_temp=sample(pfit9$beta_temp,size=1),
  beta_pt=sample(pfit9$beta_pt,size=1),
  beta_CI=sample(pfit9$beta_CI,size=1),
  tbar=mean(tempscaled),
  tmin=min(tempscaled),
  tmax=max(tempscaled),
  CIbar=mean(CImat),
  xbar=mean(y),
  precips=seq(from=min(precipscaled),to=max(precipscaled),by=0.01),
  draw=1
)
{
  for(i in 1:length(precips)){
    ppred1[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*precips[i] + beta_temp*tbar + beta_CI*CIbar + beta_pt*precips[i]*tbar
    ppred2[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*precips[i] + beta_temp*tmin + beta_CI*CIbar + beta_pt*precips[i]*tmin
    ppred3[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*precips[i] + beta_temp*tmax + beta_CI*CIbar + beta_pt*precips[i]*tmax
  }
  ppred<-rbind(ppred1,ppred2,ppred3)
  ppred<-as.data.frame(ppred)
  ppred$Draw<-draw
  return(ppred)
}
l1<-preciplines()
str(l1)
reps<-50
lps<-length(precips)*3 
lps
length(ppred1)
str(ppred1)


output<-data.frame(Precipitation=rep(precips,reps*3),temp='Max',Predicted=0,Draw=0)
str(output)
for(i in 1:reps){
  start<-(i-1)*lps+1
  end<-i*lps
  output[start:end,]<-preciplines(draw=i)
  
}

output$temp<-as.factor(output$temp)
temp_labels<-c('Max Temp','Mean Temp', "Min Temp")
names(temp_labels)<-c('Max',"Mean","Min")
precipintplot<-ggplot(output,aes(x=Precipitation,y=Predicted))+geom_line(aes(group=Draw),alpha=0.2)+facet_grid(.~temp,labeller = labeller(temp=temp_labels))+theme_classic()+ylab("Predicted Density")+stat_smooth(method='lm',se=F,color="black",size=2)
precipintplot
##with temp
templines<-function(
  alpha_0=sample(pfit9$alpha_0,size=1),
  alpha_1=sample(pfit9$alpha_1,size=1),
  alpha_2=sample(pfit9$alpha_2,size=1),
  beta_precip=sample(pfit9$beta_precip,size=1),
  beta_temp=sample(pfit9$beta_temp,size=1),
  beta_pt=sample(pfit9$beta_pt,size=1),
  beta_CI=sample(pfit9$beta_CI,size=1),
  pbar=mean(precipscaled),
  pmin=min(precipscaled),
  pmax=max(precipscaled),
  CIbar=mean(CImat),
  xbar=mean(y),
  temps=seq(from=min(tempscaled),to=max(tempscaled),by=0.01),
  draw=1
)
{
  for(i in 1:length(temps)){
    tpred1[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*pbar + beta_temp*temps[i] + beta_CI*CIbar + beta_pt*pbar*temps[i]
    tpred2[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*pmin + beta_temp*temps[i] + beta_CI*CIbar + beta_pt*pmin*temps[i]
    tpred3[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*pmin + beta_temp*temps[i] + beta_CI*CIbar + beta_pt*pmax*temps[i]
  }
  
  tpred<-rbind(tpred1,tpred2,tpred3)
  tpred<-as.data.frame(tpred)
  tpred$Draw<-draw
  return(tpred)
}

temps=seq(from=min(tempscaled),to=max(tempscaled),by=0.01)
lps<-length(temps)*3
output2<-data.frame(Temperature=rep(temps,reps*3),precip='Max',Predicted=0,Draw=0)
str(output2)
for(i in 1:reps){
  start<-(i-1)*lps+1
  end<-i*lps
  output2[start:end,]<-templines(draw=i)
  
}
output2$temp<-as.factor(output2$precip)
precip_labels<-c('Max Precip','Mean Precip', "Min Precip")
names(precip_labels)<-c('Max',"Mean","Min")

tempintplot<-ggplot(na.omit(output2),aes(x=Temperature,y=Predicted))+geom_line(aes(group=Draw),alpha=0.2)+facet_grid(.~precip,labeller=labeller(precip=precip_labels))+theme_classic()+ylab("Predicted Density")+stat_smooth(method='lm',se=F,color="black",size=2)

plot_grid(precipintplot,tempintplot,nrow=2,labels=c("A","B"))


##Fit 13
pfit13<-rstan::extract(fit13)


preciplines<-function(
  alpha_0=sample(pfit13$alpha_0,size=1),
  alpha_1=sample(pfit13$alpha_1,size=1),
  alpha_2=sample(pfit13$alpha_2,size=1),
  beta_precip=sample(pfit13$beta_precip,size=1),
  beta_temp=sample(pfit13$beta_temp,size=1),
  beta_pt=sample(pfit13$beta_pt,size=1),
  beta_CI=sample(pfit13$beta_CI,size=1),
  tbar=mean(tempscaled),
  tmin=min(tempscaled),
  tmax=max(tempscaled),
  CIbar=mean(CImat),
  xbar=mean(y),
  precips=seq(from=min(precipscaled),to=max(precipscaled),by=0.01),
  draw=1
)
{
  for(i in 1:length(precips)){
    ppred1[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*precips[i] + beta_temp*tbar + beta_CI*CIbar + beta_pt*precips[i]*tbar
    ppred2[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*precips[i] + beta_temp*tmin + beta_CI*CIbar + beta_pt*precips[i]*tmin
    ppred3[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*precips[i] + beta_temp*tmax + beta_CI*CIbar + beta_pt*precips[i]*tmax
  }
  ppred<-rbind(ppred1,ppred2,ppred3)
  ppred<-as.data.frame(ppred)
  ppred$Draw<-draw
  return(ppred)
}
l1<-preciplines()
str(l1)
reps<-50
lps<-length(precips)*3 
lps
length(ppred1)
str(ppred1)


output<-data.frame(Precipitation=rep(precips,reps*3),temp='Max',Predicted=0,Draw=0)
str(output)
for(i in 1:reps){
  start<-(i-1)*lps+1
  end<-i*lps
  output[start:end,]<-preciplines(draw=i)
  
}

precipintplot<-ggplot(output,aes(x=Precipitation,y=Predicted))+geom_line(aes(group=Draw),alpha=0.2)+facet_grid(.~temp)+theme_classic()+ylab("Predicted Density")+stat_smooth(method='lm',se=F,color="black",size=2)

##with temp
templines<-function(
  alpha_0=sample(pfit13$alpha_0,size=1),
  alpha_1=sample(pfit13$alpha_1,size=1),
  alpha_2=sample(pfit13$alpha_2,size=1),
  beta_precip=sample(pfit13$beta_precip,size=1),
  beta_temp=sample(pfit13$beta_temp,size=1),
  beta_pt=sample(pfit13$beta_pt,size=1),
  beta_CI=sample(pfit13$beta_CI,size=1),
  pbar=mean(precipscaled),
  pmin=min(precipscaled),
  pmax=max(precipscaled),
  CIbar=mean(CImat),
  xbar=mean(y),
  temps=seq(from=min(tempscaled),to=max(tempscaled),by=0.01),
  draw=1
)
{
  for(i in 1:length(temps)){
    tpred1[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*pbar + beta_temp*temps[i] + beta_CI*CIbar + beta_pt*pbar*temps[i]
    tpred2[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*pmin + beta_temp*temps[i] + beta_CI*CIbar + beta_pt*pmin*temps[i]
    tpred3[i,3] = alpha_0 + alpha_1*xbar + alpha_2*xbar + beta_precip*pmin + beta_temp*temps[i] + beta_CI*CIbar + beta_pt*pmax*temps[i]
  }
  
  tpred<-rbind(tpred1,tpred2,tpred3)
  tpred<-as.data.frame(tpred)
  tpred$Draw<-draw
  return(tpred)
}

temps=seq(from=min(tempscaled),to=max(tempscaled),by=0.01)
lps<-length(temps)*3
output2<-data.frame(Temperature=rep(temps,reps*3),precip='Max',Predicted=0,Draw=0)
str(output2)
for(i in 1:reps){
  start<-(i-1)*lps+1
  end<-i*lps
  output2[start:end,]<-templines(draw=i)
  
}


tempintplot<-ggplot(na.omit(output2),aes(x=Temperature,y=Predicted))+geom_line(aes(group=Draw),alpha=0.2)+facet_grid(.~precip)+theme_classic()+ylab("Predicted Density")+stat_smooth(method='lm',se=F,color="black",size=2)

plot_grid(precipintplot,tempintplot,nrow=2,labels=c("A","B"))

