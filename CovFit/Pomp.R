library(subplex)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(pomp)
library(iterators)

library(doMC)
registerDoMC(cores=6)


meas <- read_csv(paste0("https://kingaa.github.io/sbied/stochsim/","Measles_Consett_1948.csv")) %>%
  select(week,reports=cases)
meas %>% as.data.frame() %>% ggplot(aes(x=week,y=reports))+geom_line()

# P(X=k)=(n über k) * p^k * (1-p)^(n-k)
# Number of infected  calculated as ranom number with propability 1-exp(-Beta*I/N*delta.t) to be in infected
# Number of recovered calculated as ranom number with propability 1-exp(-mu_IR*delta.t)    to recover
# this function estimates the counts of SIRH at t+1 from their counts at t
# Parameters values for daily reports, thus delta.t=1/7, and H accumulates during one week, then reset
sir_step <- function (S, I, R, N, H, Beta, mu_IR, delta.t, ...) {
  dN_SI <- rbinom(n=1,size=S,prob=1-exp(-Beta*I/N*delta.t))
  dN_IR <- rbinom(n=1,size=I,prob=1-exp(-mu_IR*delta.t))
  S <- S - dN_SI
  I <- I + dN_SI - dN_IR
  R <- R + dN_IR
  H <- H + dN_IR;
  c(S = S, I = I, R = R, H = H)
}

sir_rinit <- function (N, eta, ...) {
  c(S = round(N*eta), I = 1, R = round(N*(1-eta)), H=0)
}

measSIR <- meas %>% 
  pomp(times="week",t0=0,
       rprocess=euler(sir_step,delta.t=1/7),
       rinit=sir_rinit, 
       accumvars="H"
   )

# returns a density, i.e. the probability that 
# - a certain number of reports will be generated 
# - from a compartment of size H
# - given a report probabiltiy of rho
sir_dmeas <- function (reports, H, rho, log, ...) {
  dbinom(x=reports, size=H, prob=rho, log=log)
}

# absolute number of reports given compartment size H and reporting probability rho
sir_rmeas <- function (H, rho, ...) {
  c(reports=rbinom(n=1, size=H, prob=rho))
}

# update measSIR
measSIR <- measSIR %>%
  pomp(
    rmeasure=sir_rmeas,
    dmeasure=sir_dmeas,
    statenames=c("S","I","R","H"),
    paramnames=c("Beta","mu_IR","N","eta","rho")
  )

# Run simulation
sims <- measSIR %>%
  simulate(
    params=c(Beta=25,mu_IR=0.25,rho=0.5,eta=0.03,N=38000),
    nsim=3,format="data.frame",include.data=TRUE
  )

sims %>% 
  ggplot(aes(x=week,y=reports,group=.id,color=.id=="data"))+
  geom_line()+
  guides(color=FALSE)


# ------------------------------------------------------------------------------------------
# Pomp: Observed data: Confirmed, Recovered, Deaths 
# TODO: Tested
# ------------------------------------------------------------------------------------------

begDate <- as.Date("2020-02-25")
endDate <- as.Date("2020-07-15")
df <- data.frame(Date=(seq(from=begDate,to=endDate,by=1)))

# New Cases
dc <- read.csv("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/CovFit/Epikurve.csv", stringsAsFactors=FALSE, sep=";")
colnames(dc) <- c("time","newConfirmed","TimeStamp")
dc <- dc %>% dplyr::mutate(totConfirmed=cumsum(newConfirmed)) %>%
  #dplyr::mutate(Datetime=as.Date(time, format="%d.%m.%Y")) %>% 
  dplyr::mutate(xt=1:n()) %>%
  dplyr::select(xt, totConfirmed, newConfirmed) %>%
  head(70)

# Recovered
dr <- read.csv("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/CovFit/GenesenTimeline.csv", stringsAsFactors=FALSE, sep=";")
colnames(dr) <- c("time","recovered","TimeStamp")
dr <- dr %>% dplyr::mutate(newRecovered=recovered-dplyr::lag(recovered,default=0)) %>%
  #dplyr::mutate(Datetime=as.Date(time, format="%d.%m.%Y")) %>% 
  dplyr::mutate(xt=1:n()) %>%
  dplyr::select(xt, totRecovered=recovered, newRecovered) %>%
  head(70)

# deaths
dd <- read.csv("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/CovFit/TodesfaelleTimeline.csv", stringsAsFactors=FALSE, sep=";")
colnames(dd) <- c("time","deaths","TimeStamp")
dd <- dd %>% dplyr::mutate(newDeaths=deaths-dplyr::lag(deaths,default=0)) %>%
  #dplyr::mutate(Datetime=as.Date(time, format="%d.%m.%Y")) %>% 
  dplyr::mutate(xt=1:n()) %>%
  dplyr::select(xt, totDeaths=deaths,newDeaths) %>%
  head(70)

# Complete dataset based on Gesundheitsminiterium data
df <- dc %>% dplyr::inner_join(dr) %>% dplyr::inner_join(dd)
# new cases
dn <- df %>% dplyr::select(!starts_with("tot")) %>% tidyr::gather(key=Status, value=Count, -xt)
ggplot(data=dn, aes(x=xt, y=Count, color=Status)) + geom_line() + scale_y_log10()
# total cases
dt <- df %>% dplyr::select(!starts_with("new")) %>% tidyr::gather(key=Status, value=Count, -xt)
ggplot(data=dt, aes(x=xt, y=Count, color=Status)) + geom_line() + scale_y_log10()

# Generate hidden and observed data with sensible values of parameters (ISF=1, daysI=7, daysR=21, daysD=14, SDD=21)
source("./CovFit/CovGenICRD.R")
parms=c(1.00,7,21,14,0.05,21,0,70,4)
gt <- CovGenICRD(parms)
dh <- gt %>% select(xt=Day, newInfectious, newConfirmed, newRecovered, newDeaths)
str(dh)

icrd_rProc <- function(t, I,C,R,D, dh, delta.t=1, ...) {
  c(I=dh[t+1,"newInfectious"], C=dh[t+1,"newConfirmed"], R=dh[t+1,"newRecovered"], D=dh[t+1,"newDeaths"])
}
# ------------------------------------------------------------------------------------------

sir_rProc <- function (S, I, R, C, mu_SI, mu_IR, delta.t=1,...) {
  dN_SI <- mu_SI * S * I/(S+I+R)
  dN_IR <- mu_IR * I
  S <- S - dN_SI
  I <- I + dN_SI - dN_IR
  R <- R + dN_IR
  C <- dN_SI;
  c(S=S, I=I, R=R, C=C)
}
#sir_rProc <- Csnippet("
#  double dN_SI;
#  double dN_IR;
#  dN_SI = mu_SI * S * I/(S+I+R);
#  dN_IR = mu_IR * I;
#  S = S - dN_SI;
#  I = I + dN_SI - dN_IR;
#  R = R + dN_IR;
#  C = dN_SI; ")

sir_rInit <- function (N, ...) {
  c(S=N, I=1, R=0, C=0)
}
#sir_rInit <- Csnippet("
#  S=N; 
#  I=1; 
#  R=0; 
#  C=0;")

#sir_rMeas <- function(newConfirmed, ...) {
#  # c(newConfirmed = I/(1+mu_SI) + S/mu_IR)  
#  c(sirConfirmed = newConfirmed)  
#}

#sir_rMeas <- Csnippet("
#  double sirConfirmed;
#  sirConfirmed = C; ")

#sir_dMeas <- Csnippet("
#  lik = 1;")

sirPomp <- pomp(times="xt", t0=0,
  data = df,
  rprocess=discrete_time(step.fun=sir_rProc),
  rinit=sir_rInit,
#  dprocess=sir_rMeas,
  statenames=c("S","I","R","C"),
  paramnames=c("mu_SI","mu_IR","N"),
  params=c(mu_SI=.5, mu_IR=0.2, N=100000)
)
spy(sirPomp)

# Sensible Verlauf: max 3 weeks, duration 10 weeks: c(mu_SI=.5,mu_IR=.1,N=10000))
sim <- simulate(times=1:70, t0=0,
                include.data=TRUE,
                format="data.frame", 
                sirPomp,
                params=c(mu_SI=.55,mu_IR=.2,N=10000))

dg <- cbind(sim[71:140,c("S","I","R","C")],sim[1:70,c("xt","newConfirmed")])
dg %>%  tidyr::gather(key=SIR, value=Count, S,I,R,C,newConfirmed) %>%
  ggplot(aes(x=xt, y=Count, col=SIR, shape=SIR)) + 
  geom_line() #+ 
  geom_point() +scale_y_continuous(trans = 'log10')


p <- parmat(coef(sim),5)
p["mu_SI",] <- seq(.40,.60,by=.05)
colnames(p) <- LETTERS[1:5]
  
simulate(times=1:70, t0=0, 
         format="data.frame", 
         include.data=FALSE,
         sirPomp,
         params=p) -> sims
simt <- sims %>% dplyr::select(-newConfirmed) %>% tidyr::gather(key,count,S,I,R,C)
ggplot(data=simt,aes(x=xt,y=count, color=interaction(.id,key))) +
  # group=interaction(.id,key), , linetype=key)
  geom_line()+
  #scale_y_log10()+
  expand_limits(y=1)+
  #facet_grid(variable~.id,scales="free_y")+
  labs(y="",color="")




# ------------------------------------------------------------------------------------------

#det_Skel <- Csnippet("
#  double dN_SI;
#  double dN_IR;
#  dN_SI = mu_SI * S * I/(S+I+R);
#  dN_IR = mu_IR * I;
#  DS = S - dN_SI;
#  DI = I + dN_SI - dN_IR;
#  DR = R + dN_IR; 
#  DC = dN_SI; ")

# ------------------------------------------------------------------------------------------
# Working experiment
# ------------------------------------------------------------------------------------------
# hidden data
det_rProc <- function (S, I, R, C, mu_SI, mu_IR, delta.t=1,...) {
  dN_SI <- mu_SI * S * I/(S+I+R)
  dN_IR <- mu_IR * I
  S <- S - dN_SI
  I <- I + dN_SI - dN_IR
  R <- R + dN_IR
  C <- dN_SI;
  c(S=S, I=I, R=R, C=C)
}

det_rInit <- function (N, ...) { # (N, mu_SI, mu_IR, t0=1, ...)
  c(S=N, I=1, R=0, C=0)
}

# Observed data
det_dMeas <- function(newConfirmed, C, ..., log) {
  # hidden is C, newConfirmed is data. At any point in time, the hidden C is poisson distributed
  # dpois is the probability that measurement will take on value newConfirmed
  dpois(newConfirmed, lambda=C, log=log)
}

# pomp structure
det_Pomp <- pomp(data=df, times="xt", t0=0,
                skeleton = pomp::map(det_rProc, delta.t=1),
                rinit = det_rInit,
                dmeasure = det_dMeas,
                statenames=c("S","I","R","C"),
                paramnames=c("mu_SI","mu_IR","N"),
                params=c(mu_SI=.55,mu_IR=.2,N=10000)
)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# not needed now
# trajectory of skeleton: Calculation of hidden features over time
det_Traj <- det_Pomp %>% 
  pomp::trajectory(
    params=c(mu_SI=.55,mu_IR=.2,N=10000),
    times=1:70, t0=0, format="data.frame", verbose=TRUE)

# sequence of parameters for pomp model
p <- parmat(coef(det_Pomp),10)
p["mu_SI",] <- seq(0.25,.7,by=.05)

# caölculate trajectories for parameter sequence
det_Pomp %>%
  pomp::trajectory(params=p,format="data.frame") %>%
  ggplot(mapping=aes(x=xt,y=I,color=.id,group=.id))+
  geom_line()
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Calc negative log likelihood for parameters
det_Ofun <- det_Pomp %>%
  traj_objfun(
    est = c("mu_SI","mu_IR","N")
    #dmeasure = det_dMeas
  )
d <- det_Ofun(c(.5,.2,15000))

# use above calculated neg log likelihood with optim kind of optimizers
det_Fit <- subplex(c(1,1,10000),fn=det_Ofun)
round(det_Fit$par,2)
det_Fit$value

coef(det_Ofun)
logLik(det_Ofun)
dp <- det_Pomp %>%
  pomp::trajectory(params=c(mu_SI=det_Fit$par[1],mu_IR=det_Fit$par[2],N=det_Fit$par[3]),format="data.frame") 

plot(df$xt,df$newConfirmed)
lines(dp$xt,dp$C, type="l")

plot(df$xt, df$newConfirmed-dp$C, type="b")
abline(h=0)
sum(df$newConfirmed-dp$C)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# sidestep: construct logLik surface -> find minimum -> same as with subplex in one lineq
det_Inits <- expand.grid(
  mu_SI=seq(0.4,0.6,by=0.01),
  mu_IR=seq(0.15,0.25,by=0.01),
  N=seq(14000,17000, by=100)
  )

f <- foreach(p=iter(det_Inits,"row"), 
        .combine=rbind, .inorder=FALSE, .options.multicore=list(set.seed=TRUE)) %dopar%
  {
    library(pomp)
    p$logLik <- det_Ofun(p)
    # run subplex from different stating value -> same result
    #det_Fit <- subplex(par=unlist(p),fn=det_Ofun)
    #p$mu_SI_f <- -det_Fit$par[1]
    #p$mu_IR_f <- -det_Fit$par[2]
    #p$mu_N_f <- -det_Fit$par[3]
    #p$logLik <- -det_Fit$value
    p
  }
o <- det_Inits[which(f$logLik==min(f$logLik,na.rm=TRUE)),]
plot(tapply(f$logLik,f$N,min), type="b")

ggplot(data=f, aes(x=mu_SI, y=mu_IR, fill=logLik)) +
  geom_tile() + 
  facet_wrap(N~.) +
  scale_fill_gradient(low="white", high="black")

plot(dg$xt,dg$newConfirmed)
lines(dp$xt,dp$C, type="l")
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# End working experiment
# ------------------------------------------------------------------------------------------


# if: for particle filtering only, that is, not applicable to deterministic models
mf <- det_Pomp %>%
  mif2(
    params=det_Inits[55,],
    Np=100,
    Nmif=10,
    dmeasure=det_dMeas,
    partrans=parameter_trans(log=c("mu_SI","mu_IR")),
    # rw.sd=rw.sd(mu_SI=0.02,mu_IR=0.02),
    # cooling.fraction.50=0.5,
    statenames=c("S","I","R","C"),
    paramnames=c("mu_SI","mu_IR","N"),   
  )


# ==============================================================================
# PoC: non Markov model with hidden states from external array
# ==============================================================================
h = seq(1,19,by=2) # data field used infuse deterministic model into pomp as 'pre'
d = data.frame(Day=1:10, Count=seq(1,10)) # observed data
rpfun <- function(t, pre, ...) {
  c(H=as.numeric(pre[t+1]))
}
ifun <- function(t0, pre, ...) {
  c(H=as.numeric(pre[t0]))
}
dmfun <- function(Count, H, ..., log) {
  dpois(Count, lambda=H, log=log)
}
p <- pomp(data=d, times="Day", t0=1,
          pre=h,
          skeleton = pomp::map(rpfun, delta.t=1),
          rinit = ifun,
          dmeasure = dmfun,
          statenames=c("H"),
          params=NULL,
          verbose=TRUE
) 
p %>% trajectory(format="data.frame", verbose=TRUE)
tofun <- p %>% traj_objfun()
# likelyhood of observed data given hidden state calculated with params=par which are handed to skeleton
# which in turn calls rpoc(t,states,params,...) rpoc needs to access the userdata array.
# This array needs to be created for each run with a specific parameter set, that is, in rinit !!!
# --> Do not know how to create a userdataset in rinit, that is visible to the other mode components <--
# --> need to resort to creating a likelihood surface in a two dim loop that creates the userdata upfront
# as the userdataset is created from a stochastic process, convergence of the optimizer is not guarantied !
tofun(par=NULL) 
fit <- subplex(initparms=NULL,fn=tofun) # 
# -> test sucessfull, could ransfer data from array into skeleton function.
# -> care must be taken with rinit data format and to get the first two records right !

# ==============================================================================
# PoC: non Markov model with hidden states from array generated in rinit()
# ==============================================================================
ic_rpfun <- function(t,I,C,IC, ...) {
  #cat('ic_rpfun:', t, IC[t,"icInfectious"], IC[t,"icConfirmed"], '\n')
  c(I=as.numeric(IC[t+1,"icInfectious"]),C=as.numeric(IC[t+1,"icConfirmed"]))
}
ic_ifun <- function(t0, IC, ...) {
  #cat('ic_ifun:', t0, IC[t0,"newInfectious"], IC[t0,"newConfirmed"], '\n')
  c(I=as.numeric(IC[t0,"icInfectious"]),C=as.numeric(IC[t0,"icConfirmed"]))
}
ic_dmfun <- function(t,newConfirmed, C, ..., log) {
  # Workaround: in case expected number of newConfirmed C==0, dpois cannot be calculated.
  if(C==0) C=.001
  dpois(newConfirmed, lambda=C, log=log)
}

# Grid for logLik surface
ic_pgrid <- expand.grid(
  ISF=seq(0.45,0.80,by=0.025),
  ISFD=seq(0.97,1.00,by=0.0025),
  SDSF=seq(0.15,0.45,by=0.025)
)
str(ic_pgrid)
# Calculate logLik surface
pl <- foreach(parms=iter(ic_pgrid,"row"), 
             .combine=rbind, 
             .inorder=FALSE, 
             .options.multicore=list(set.seed=TRUE)) %dopar%  { ic_pomp(parms, bPlot=FALSE, bLogLik=FALSE) }

# calculate logLik for parms
# p=data.frame(ISF=.65,ISDF=.97, SDSF=0.1)
ic_pomp <- function(parms, bPlot=FALSE, bLogLik=TRUE) {

  p <- data.frame(parms)
  
  # calculate 'Hidden Data' as stochastic process with infectious period poison distributed
  # parameters: ISF: Disease Spread Factor during Infectious period. SDSF: Spread Factor after ShutDow
  ic <- CovGenIC(unlist(p)) %>%
    dplyr::select(icInfectious=newInfectious, icConfirmed=newConfirmed)
  
  pm <- pomp(data=df, times="xt", t0=1,
             IC = ic,
             skeleton = pomp::map(ic_rpfun, delta.t=1),
             rinit = ic_ifun,
             dmeasure = ic_dmfun,
             statenames=c("I","C"),
             #paramnames=c("ISF","SDSF"),
             #params=c(ISF=1,SDSF=0.1),
             verbose=FALSE
             
  )
  ic_tofun <- pm %>% traj_objfun()
  p$logLik <- ic_tofun(par=unlist(p))
  # cat(p[1,1],p[1,2],p[1,3],'\n')
  
  if(bPlot) {
    fileName=paste0("./CovFit/plots/",paste("ic_pomp",p[1,1],p[1,2],p[1,3],round(p[1,4]),sep="_"), ".png")
    
    #dp <- pm %>% pomp::trajectory(params=NULL,format="data.frame")
    dg <- cbind(df,ic)
    
    gg <- ggplot(data=dg, aes(x=xt, y=newConfirmed)) + geom_line() + geom_point() +
      geom_line(aes(y=icConfirmed)) + geom_point(aes(y=icConfirmed)) +
      ggtitle(fileName)
    ggsave(fileName, gg, scale=2, width=4, height=3, dpi=300, units="in")
  }
  
  # subplex wants logLik only
  if (bLogLik) {
    p$logLik[1]
  } else {
    p
  }
}

#colnames(f)=c("ISF","SDSF","logLik")
pl[order(pl$logLik),] %>% head(n=12)
pl[which(pl$logLik==min(pl$logLik)),]

ggplot(data=pl, aes(x=ISF, y=SDSF, fill=log(logLik))) +
  geom_tile() + 
  scale_fill_gradient(low="white", high="black") +
  facet_wrap(.~ISFD, nrow=4)

fileName=paste0("./CovFit/plots/",paste("ic_pomp","logLikSurface-%3d",sep="_"), ".png")
ggsave(fileName, gg, scale=2, width=4, height=3, dpi=300, units="in")

# calculate single logLik for parms
ic_pomp(c(0.65,.98,.20), bPlot=TRUE, bLogLik=FALSE)

# Optimize parameter estimation using subplex
sp <- subplex(c(0.4,.99,.25),fn=ic_pomp, control=list(reltol=1e-3, parscale=.1))
sp$par
# "0.665 0.976 0.24"
# 0.7711271 0.9608891 0.2096121
# 0.6344581 0.9809003 0.2503852
# 0.5501013 0.9944705 0.2895110
# 0.4728322 1.0095516 0.3269710

# Calculate and plot hidden process for optimum solution
par=unlist(pl[863,1:3])
ic <- CovGenIC(par) %>% select(icInfectious=newInfectious, icConfirmed=newConfirmed )
dg <- cbind(df,ic)
ggplot(data=dg, aes(x=xt, y=newConfirmed)) + geom_line() + geom_point() +
  geom_line(aes(y=icConfirmed)) + geom_point(aes(y=icConfirmed)) +
  geom_line(aes(y=icInfectious)) + geom_point(aes(y=icInfectious)) +
  ggtitle(paste(round(sp$par[1],3),round(sp$par[2],3),round(sp$par[3],3)))
fileName=paste0("./CovFit/plots/",paste("ic_pomp","logLikSubPlex-%3d",sep="_"), ".png")
ggsave(fileName, gg, scale=2, width=4, height=3, dpi=300, units="in")
# ==============================================================================








spy(pomp)
p %>% trajectory(format="data.frame", verbose=TRUE)
tofun <- p %>% traj_objfun()
tofun(par=NULL) # likelyhood of observed data given hidden state


o <- det_Inits[which(f$logLik==min(f$logLik,na.rm=TRUE)),]
plot(tapply(f$logLik,f$N,min), type="b")

ggplot(data=f, aes(x=mu_SI, y=mu_IR, fill=logLik)) +
  geom_tile() + 
  #facet_wrap(N~.) +
  scale_fill_gradient(low="white", high="black")

# ==============================================================================
# non Markov model 
# ==============================================================================
p <- c(0.57, 0.36)
nGenerates=100
for (k in 1:nGenerates) {
  ic <- CovGenIC(unlist(p)) %>% 
    dplyr::select(icInfectious=newInfectious, icConfirmed=newConfirmed) %>%
    dplyr::mutate(ID=1:n(), k=k)
  
  if(k==1)
    ick <- ic
  else
    ick <- rbind(ick,ic)
}
ic <- ick %>% 
  group_by(ID) %>% 
  summarize(icInfectious=mean(icInfectious), icConfirmed=mean(icConfirmed)) %>%
  ungroup()

ggplot(data=ick, aes(x=ID, y=icInfectious, group=k, color=k)) + geom_line() + 
  geom_line(data=ic, aes(x=ID, y=icInfectious), colour="white")

 

