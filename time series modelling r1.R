rm(list=ls())
library (rstan)
library(loo)
library(tidyverse)
library(tidybayes)
library(bayesplot)
library(cowplot)
library(loo)
library(rstantools)
library(reshape2)
library(dplyr)
setwd("~/Documents/Research/dissertation/12 sites analyses/12sitesdisease")

##Data processing
connectivity<-read.csv('connectivity2.csv')
CImat1<-connectivity
CImat<-CImat1[,2:16]
rownames(CImat)<-CImat1[,1]
CImat<-as.matrix(CImat)
CImatscaled<-CImat
CImat<-as.matrix(CImatscaled)
CImat<-t(CImat)
CImat<-t(CImat)
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
row_indx_pos = matrix((rep(1:M, N)), M, N)[which(!is.na(y))]
col_indx_pos = matrix(sort(rep(1:N, M)), M, N)[which(!is.na(y))]
n_pos = length(row_indx_pos)
y = y[which(!is.na(y))]
W=2
wetdry<-wetdry1[1:12]

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

##second selection

fit24<-rstan::stan(file='models/full model no pt interaction a2.stan',
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

fit25<-rstan::stan(file='models/full dd temp a2.stan',
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

fit26<-rstan::stan(file='models/full dd precip a2.stan',
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

fit27<-rstan::stan(file='models/full dd CI a2.stan',
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
fit28<-rstan::stan(file='models/full dd precip CI a2.stan',
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
fit29<-rstan::stan(file='models/full dd temp CI a2.stan',
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




#Figure 3

posterior9 <- as.matrix(fit9)
p9<-mcmc_areas(posterior9,
               pars = c("alpha_0","alpha_1",'alpha_2[1]','alpha_2[2]',"beta_precip",'beta_temp',"beta_pt",'beta_CI'),
               prob = 0.90, prob_outer = 1,
               transformations = list("alpha_1" =  function(x) x -1)
               ) +scale_y_discrete(labels=c(expression(alpha[0]),expression(alpha[1]),expression(alpha[paste(2,' , ', "wet")]),expression(alpha[paste(2,' , ', "dry")]),expression(beta[precip]),expression(beta[temp]),expression(beta[temp%*%precip]),expression(beta[connectivity])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=15))+ylab('Parameter')+xlim(-1,5)
p9
posterior11 <- as.matrix(fit11)
P11<-mcmc_areas(posterior11,
           pars = c("alpha_0[1]","alpha_0[2]","alpha_1",'alpha_2',"beta_precip",'beta_temp',"beta_pt",'beta_CI'),
           prob = 0.90, prob_outer = 1,
           transformations = list("alpha_1" =  function(x) x -1)
           ) +scale_y_discrete(labels=c(expression(alpha[paste(0,' , ', "wet")]),expression(alpha[paste(0,' , ', "dry")]),expression(alpha[1]),expression(alpha[2]),expression(beta[precip]),expression(beta[temp]),expression(beta[temp%*%precip]),expression(beta[connectivity])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=15))+xlab("Standardized Value")+ylab('Parameter')+xlim(-1,5)

plot_grid(p9,P11,labels=c('a','b'),nrow=2)

##Figure S1
posterior12 <- as.matrix(fit12)
mcmc_areas(posterior12,
           pars = c("alpha_0[1]","alpha_0[2]","alpha_1",'alpha_2',"beta_precip[1]","beta_precip[2]",'beta_temp',"beta_pt",'beta_CI'),
           prob = 0.90, prob_outer = 1,
           transformations = list("alpha_1" =  function(x) x -1)
           ) +scale_y_discrete(labels=c(expression(alpha[paste(0,' , ', "wet")]),expression(alpha[paste(0,' , ', "dry")]),expression(alpha[1]),expression(alpha[2]),expression(beta[paste("precip",' , ', "wet")]),expression(beta[paste("precip",' , ', "dry")]),expression(beta[temp]),expression(beta[temp%*%precip]),expression(beta[connectivity])))+theme(plot.title = element_text(hjust = 0.5,),text=element_text(size=20))+xlab("Standardized Value")+ylab('Parameter')



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

log_lik24 <-extract_log_lik(fit24, merge_chains = FALSE)
waic24<-loo::waic(log_lik24)
rel_n_eff24 <- relative_eff(exp(log_lik24))
loo24<-loo(log_lik24, r_eff = rel_n_eff24, cores = 2)

log_lik25 <-extract_log_lik(fit25, merge_chains = FALSE)
waic25<-loo::waic(log_lik25)
rel_n_eff25 <- relative_eff(exp(log_lik25))
loo25<-loo(log_lik25, r_eff = rel_n_eff25, cores = 2)

log_lik26 <-extract_log_lik(fit26, merge_chains = FALSE)
waic26<-loo::waic(log_lik26)
rel_n_eff26 <- relative_eff(exp(log_lik26))
loo26<-loo(log_lik26, r_eff = rel_n_eff26, cores = 2)

log_lik27 <-extract_log_lik(fit27, merge_chains = FALSE)
waic27<-loo::waic(log_lik27)
rel_n_eff27 <- relative_eff(exp(log_lik27))
loo27<-loo(log_lik27, r_eff = rel_n_eff27, cores = 2)

log_lik28 <-extract_log_lik(fit28, merge_chains = FALSE)
waic28<-loo::waic(log_lik28)
rel_n_eff28 <- relative_eff(exp(log_lik28))
loo28<-loo(log_lik28, r_eff = rel_n_eff28, cores = 2)


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

##Model comparison 3 - dropping terms from best model.
compalpha2 <- loo_compare(waic9,waic24,waic25,waic26,waic27,waic28)
compalpha2<-as.data.frame(compalpha2)
compalpha2$delta_waic<-compalpha2$waic-compalpha2$waic[1]
print(compalpha2)



#### Posterior predictive simulation of interaction between temp and precip

##Figure 4

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



##One step ahead predictions - Figure S2

print(fit9, pars=c("alpha_0", "alpha_1", "alpha_2","beta_precip","beta_temp","beta_pt","beta_CI", "lp__"), probs=c(.1,.5,.9))


alpha_0<-c(3.25,2.26)
alpha_1<-0.17
alpha_2<--.63
beta_precip<-0.69
beta_temp<-0.08
beta_pt<-0.68
beta_CI<-0.31

alpha_0_2<-2.34
alpha_1_2<-0.17
alpha_2_2<-c(-0.31,-0.67)
beta_precip_2<-0.65
beta_temp_2<-0.08
beta_pt_2<-0.7
beta_CI_2<-0.32

CI_pred<-melt(CImat)|>rename("site"=Var1,'year'=Var2,'CI'=value)
CI_pred$year<-as.numeric(gsub("X", "", CI_pred$year))
str(CI_pred)

datamat_pred<-melt(datamat)|>rename("site"=X,'year'=variable,'log.density'=value)
datamat_pred$year<-as.numeric(gsub("X", "", datamat_pred$year))
str(datamat_pred)
m1<-full_join(CI_pred,datamat_pred)
m1
env_pred<-data.frame(precip=precipscaled,year=2007:2021,temp=tempscaled)
env_pred
m2<-right_join(m1,env_pred)
str(m2)

wetdry_pred<-data.frame(site=unique(m2$site),wet.dry=wetdry)
m3<-right_join(m2,wetdry_pred)
m3$log.density.last.year<-NA
m3$log.density.last.year[m3$year!=2007]<-m3$log.density[m3$year!=2021]
m4<-m3[m3$year!=2007,]
m3$log.density.next.year[m3$year!=2021]<-m3$log.density[m3$year!=2007]
m4<-m3[m3$year!=2007,]
m4$log.density.next.year
m4$log.pred.next.year<-NA
m4$log.pred.next.year<-alpha_0[m4$wet.dry] + alpha_1*m4$log.density + alpha_2*m4$log.density.last.year + beta_precip*m4$precip + beta_temp*m4$temp + beta_CI*m4$CI+ beta_pt*m4$precip*m4$temp

m4$log.pred.next.year.2<-alpha_0_2 + alpha_1_2*m4$log.density + alpha_2_2[m4$wet.dry]*m4$log.density.last.year + beta_precip_2*m4$precip + beta_temp_2*m4$temp + beta_CI_2*m4$CI+ beta_pt_2*m4$precip*m4$temp

m5<-m4[m4$year!=2021,]
str(m5)
m5$year<-as.integer(m5$year)
time1<-ggplot(m5,aes(x=year,y=log.pred.next.year))+geom_line()+geom_point(aes(y=log.density.next.year))+xlab('Year')+ylab('ln(Count+1)')+scale_x_continuous(breaks=c(2008,2010,2012,2014,2016,2018,2020))+facet_wrap(.~site,nrow=4)

time2<-ggplot(m5,aes(x=year,y=log.pred.next.year.2))+geom_line()+geom_point(aes(y=log.density.next.year))+xlab('Year')+ylab('ln(Count+1)')+scale_x_continuous(breaks=c(2008,2010,2012,2014,2016,2018,2020))+facet_wrap(.~site,nrow=4)

plot_grid(time1,time2,ncol=1,labels='auto')
