library(deSolve)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(optimx)
library(doMC)
registerDoMC(cores=4)

# ---------------------------------------------------------------------------------------
# COVID-19 dataset for Austria
# data downloaded and extracted from https://info.gesundheitsministerium.at/data/data.zip
# ---------------------------------------------------------------------------------------
# Date Grid
begDate <- as.Date("2020-02-25")
endDate <- as.Date("2020-07-15")
df <- data.frame(Date=(seq(from=begDate,to=endDate,by=1)))

# New Cases
newcon <- read.csv("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/CovFit/Epikurve.csv", stringsAsFactors=FALSE, sep=";")
colnames(newcon) <- c("time","newcon","TimeStamp")
newcon <- newcon %>% 
  dplyr::mutate(Date=as.Date(time, format="%d.%m.%Y")) %>% 
  dplyr::mutate(confirmed=cumsum(newcon)) %>%
  dplyr::select(Date,newcon,confirmed)

# Deaths
deaths <- read.csv("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/CovFit/TodesfaelleTimeline.csv", stringsAsFactors=FALSE, sep=";")
colnames(deaths) <- c("time","deaths","TimeStamp")
deaths <- deaths %>% 
  dplyr::mutate(Date=as.Date(time, format="%d.%m.%Y")) %>% 
  dplyr::select(Date,deaths)

# Recovered
recovs <- read.csv("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/CovFit/GenesenTimeline.csv", stringsAsFactors=FALSE, sep=";")
colnames(recovs) <- c("time","recovered","TimeStamp")
recovs <- recovs %>% 
  dplyr::mutate(Date=as.Date(time, format="%d.%m.%Y")) %>%
  dplyr::select(Date,recovered)

# Join together
df <- df %>% left_join(deaths) %>% left_join(recovs) %>% left_join(newcon)
df$deaths[1]=0
df$recovered[1]=0
df <- df[1:141,]
df <- df %>% 
  dplyr::mutate(newrec=recovered-lag(recovered,1)) %>%
  dplyr::mutate(newdeaths=deaths-lag(deaths,1))
  
dg <- df %>% tidyr::gather(key=Status, value=Count, newcon, newrec, newdeaths)
ggplot(data=dg, aes(x=Date, y=Count, colour=Status)) + geom_line() + 
  scale_y_continuous(trans = 'log10', breaks=10^(0:6)) +
  scale_x_date(limits=c(min(dg$Date+days(5)),min(dg$Date)+days(75)))




icrd.fun <- function(parms) {
  # Parameters
  # parms=c(1,10,21,14,0.05,21,0,21,4)
  ISF=1
  daysI=10
  daysR=21
  daysD=14
  CFP=0.05
  SDD=20
  SDSF=0
  N=35
  I=4
  
  # Parameters for optimization
  ISF=parms[1]
  daysI=parms[2]
  #daysR=parms[3]
  #daysD=parms[4]
  #CFP=parms[5]
  SDD=parms[3]
  #SDSF=parms[7]
  #N=parms[8]
  #I=parms[9]
  
  
  # Constants
  #ISF <- 1.25    # InfectionSpreadFactor
  #CFP <- .05     # CaseFatalityProportion
  #daysI <- 7
  #daysR <- 14
  #daysD <- 21
  
  # Function to calculate time varying constants and compartment propagation time distributions
  # isf <- function(t=0) {k=ISF*1.0; if(t>.1*N )k=.9*k; if(t>.2*n)k=.8*k; if(t>.3*N)k=.7*k;if(t>.4*N)k=.5;return(k)} # infection spread factor
  isf <- function(t=0) {k=ISF; if(t>=SDD)k=SDSF*ISF; return(k)} # infection spread factor
  cfp <- function(t=0) {return(CFP)} # case fatatility proportion
  
  # Infection to Case time Distribution (default: delay of daysI days)
  # Case to Recovered time Distribution
  # Case to Death time Distribution

  #icd <- function(t=0,n,daysI) {return(c(rep(0,daysI),n))}
  #crd <- function(t=0,n,daysR) {return(c(rep(0,daysR),round(n*(1-cfp(t)))))}
  #cdd <- function(t=0,n,daysD) {return(c(rep(0,daysD),round(n*cfp(t))))}
 
  #icd <- function(t=0,n,daysI) {k<-rpois(n,daysI-1)+1; km<-max(k); cat("icd",t,n,daysI-1,k,km,"\n");h<-hist(k,breaks=0:km,plot=FALSE)$counts; return(h)}
  #crd <- function(t=0,n,daysR) {k<-rpois(n,daysR-1)+1; km<-max(k); cat("crd",t,n,daysR-1,k,km,"\n");h<-hist(k,breaks=0:km,plot=FALSE)$counts; return(round(h*(1-cfp(t))))}
  #cdd <- function(t=0,n,daysD) {k<-rpois(n,daysD-1)+1; km<-max(k); cat("cdd",t,n,daysD-1,k,km,"\n");h<-hist(k,breaks=0:km,plot=FALSE)$counts; return(round(h*cfp(t)))}

  icd <- function(t=0,n,daysI) {k<-rpois(n,daysI-1)+1; km<-max(k); h<-hist(k,breaks=0:km,plot=FALSE)$counts; return(h)}
  crd <- function(t=0,n,daysR) {k<-rpois(n,daysR-1)+1; km<-max(k); h<-hist(k,breaks=0:km,plot=FALSE)$counts; return(round(h*(1-cfp(t))))}
  cdd <- function(t=0,n,daysD) {k<-rpois(n,daysD-1)+1; km<-max(k); h<-hist(k,breaks=0:km,plot=FALSE)$counts; return(round(h*cfp(t)))}
  
  
  # Initialize arrays
  nI=dI=cI=tI=rep(0,N)
  nC=dC=cC=tC=rep(0,N)
  nR=dR=tR=rep(0,N)
  nD=dD=tD=rep(0,N)
  mC <- matrix(data=0, nrow=N, ncol=N)
  
  # Initialize infection
  t=1
  nI[t]=dI[t]=cI[t]=tI[t]=I
  fC <- icd(t,nI[t],daysI) # new infection to cases time distribution
  fCn <- length(fC)-1
  # mC[t,t:min(t+fCn,N)] <- fC[1:(min(t+fCn+1,N)-t)]
  fCm <- t:min(t+fCn,N)
  mC[t,fCm] <- fC[1:length(fCm)]
  
  # cat(" fun: ", ISF, daysI, daysR, daysD, CFP)
  cat(" fun: ", ISF, daysI)
  
  for (t in 2:N) {
    # cat(paste(t,""))
    
    # Compartment I (Infectious)
    # ---------------
    # new infections
    nI[t] <- round(cI[t-1] * isf(t-1))
    # ---------------
    
    # Compartment C (Cases, tested positiv) 
    if(nI[t]>0) {
      fC <- icd(t,nI[t],daysI) # new infection to cases time distribution
      # record future cases arising from new infections to row t of the new cases matrix
      fCn <- length(fC)-1
      fCm <- t:min(t+fCn,N)
      mC[t,fCm] <- fC[1:length(fCm)]
    }
    
    # new Cases of today (spread out on previous days)
    nC[t] <- sum(mC[,t])        # new Cases
    
    # add new infections and remove new Cases to/from the Infectious compartment
    dI[t] <- nI[t] - nC[t]      # difference to yesterday (remove cases propagated to confirmed)
    cI[t] <- cI[t-1] + dI[t]    # current infectious
    tI[t] <- tI[t-1] + nI[t]    # total number of ever infectious
    
    # Compartment R (Recovered)
    # time distribution of future recover events
    #nR[t]<-0
    if(nC[t]>0) {
      fR <- crd(t,nC[t],daysR)
      fRn <- length(fR)-1
      nR[t:min(t+fRn,N)] <- nR[t:min(t+fRn,N)] + fR[1:(min(fRn,N-t)+1)]
    }
    dR[t] <- nR[t]
    tR[t] <- tR[t-1] + dR[t]
    
    # Compartment D (Deaths)
    #nD[t]<-0
    if(nC[t]>0) {
      fD <- cdd(t,nC[t],daysD) 
      fDn <- length(fD)-1
      nD[t:min(t+fDn,N)] <- nD[t:min(t+fDn,N)] + fD[1:(min(fDn,N-t)+1)]
    }
    dD[t] <- nD[t]
    tD[t] <- tD[t-1] + dD[t]
    
    # Adjustments to compartment C from propagations to compartments R and D
    dC[t] <- nC[t] - dR[t] - dD[t]
    cC[t] <- cC[t-1] + dC[t]
    tC[t] <- tC[t-1] + nC[t]
  }  
  dd <- data.frame(Day=1:N,
                   Infectious=tI, Confirmed=tC, Recovered=tR, Deaths=tD, 
                   curInfectious=cI, curConfirmed=cC, curRecovered=tR, curDeaths=tD,
                   newInfectious=nI, newConfirmed=nC, newRecovered=nR, newDeaths=nD)
  dd <- cbind(dd, as.data.frame(mC, colnames=paste0("day",1:N)))
  #dd <- data.frame(Day=1:N, Infectious=log10(tI), Confirmed=log10(tC), Recovered=log10(tR), Deaths=log10(tD), newInfected=log10(nI), newConfirmed=log10(nC), newDeaths=log10(nD))
  return(dd)
}

# ground truth data
parms=c(0.65,7,20,14,0.05,8,0,8,4)
parms=c(1,10,20,14,0.05,20,0,40,4)
#parms=c(1.05,12,21,14,0.1,21,0,21,4)
gt <- icrd.fun(parms)
dp <- gt %>% tidyr::gather(key=Status, val=Count, newInfectious, newConfirmed, newRecovered, newDeaths)
ggplot(data=dp, aes(x=Day, y=Count, col=Status, shape=Status)) + geom_line() + geom_point(size=3) +
  ggtitle(paste("Ground Truth",parms[1],parms[2])) +
  scale_x_continuous(breaks=1:10*7) #+ scale_y_continuous(trans="log10")
#print(gg)

#cat("\n")
# dp <- gt %>% tidyr::gather(key=Status, val=Count, Infectious, Confirmed, Recovered, Deaths, newInfectious, newConfirmed, newRecovered, newDeaths)
# dp <- gt %>% tidyr::gather(key=Status, val=Count, Confirmed, Deaths, newInfectious, newConfirmed, newRecovered, newDeaths, curInfectious, curConfirmed)
 
icrd.rss <- function(params) {
  df <- icrd.fun(params)
  #rss.in <- sum(gt$newInfectious-df$newInfectious)^2
  rss.cn <- sum(log(gt$newConfirmed+1)-log(df$newConfirmed+1))^2
  #rss.rn <- sum(gt$newRecovered-df$newRecovered)^2
  #rss.dn <- sum(gt$newDeaths-df$newDeaths)^2
  #rss.i <- sum((gt$Infectious-df$Infectious)^2)
  #rss.r <- sum((gt$Recovered-df$Recovered)^2)
  rss <- rss.cn
  cat(" rss: " , params, rss, 
      #format(rss.in, scientific=TRUE, digits=3), 
      #format(rss.cn, scientific=TRUE, digits=3), 
      #format(rss.rn, scientific=TRUE, digits=3), 
      #format(rss.dn, scientific=TRUE, digits=3),
      format(rss, scientific=TRUE, digits=3), "\n")  
  return(rss)
}

# Search for parameters that fit the generated ground truth data
optx <- optimx(par=c(0.65,7,8),
             fn=icrd.rss,
             # method = c("Nelder-Mead"),
             lower = c(0.5,3,5),
             upper = c(1.0,10,15),
             # tnmax=1000,
             control=list(all.methods=TRUE, save.failures=TRUE, parscale=c(1,10), ndeps=c(0.05,1))
             # control=list(save.failures=TRUE, maxit=10000)
             # control=list(ndeps=c(.01,1), parscale=c(1,1))
)
optx


pms <- expand.grid(seq(.75,1.25,by=.05),seq(5,15,by=1))
pms$rss <- 0
colnames(pms) <- c("ISF","daysI","rss")
set.seed(12345)
for(k in 1:dim(pms)[1]) {
  pms$rss[k] <- icrd.rss(params=c(pms[k,1],pms[k,2]))
}
ggplot(pms, aes(x=ISF, y=daysI)) + geom_raster(aes(fill=log(rss)))

m <- which(pms$rss==min(pms$rss))
pms[m,]


pms <- expand.grid(seq(1-.1,1+.1,by=.005),seq(8,12,by=.1))
pms$rss <- 0
colnames(pms) <- c("ISF","daysI","rss")
set.seed(12345)
for(k in 1:dim(pms)[1]) {
  pms$rss[k] <- icrd.rss(params=c(pms[k,1],pms[k,2]))
}
ggplot(pms, aes(x=ISF, y=daysI)) + geom_raster(aes(fill=log(rss)))


dim(pms)


opt = matrix(0, nrow=5, ncol=3)
k=3
opt[k,1] = 17
opt[k,2] = optx$p1
opt[k,3] = optx$value


#do <- gt <- icrd.fun(Opt$par) 
#dp <- do %>% tidyr::gather(key=Status, val=Count, Infectious, Confirmed, Recovered, Deaths, newInfected, newConfirmed)
#gg <- ggplot(data=dp, aes(x=Day, y=Count, col=Status)) + geom_line() + ggtitle(paste("Optimized",Opt$par[1], Opt$par[2]))
# + scale_y_continuous(trans="log10")
#print(gg)




