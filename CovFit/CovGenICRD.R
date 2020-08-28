library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)


CovGenICRD <- function(parms) {
  # Default Parameters
  # parms=c(1,7,21,14,0.05,21,0,70,4)
  ISF=1          # Infection Spread Factor
  daysI=7        # Number of days specimen is infectios
  daysR=21       # Number of days from infection to recover
  daysD=14       # Number of days from infection to death
  CFP=0.05       # CaseFatality proportion
  SDD=21         # ShutDown Day
  SDSF=0         # ShutDown Spread Factor (percentage)
  N=70           # Number of days to calculate data
  I=4            # Initial number of Infections
  
  # Parameters for optimization
  ISF=parms[1]
  daysI=parms[2]
  daysR=parms[3]
  daysD=parms[4]
  CFP=parms[5]
  SDD=parms[6]
  SDSF=parms[7]
  N=parms[8]
  I=parms[9]
  
  # Calculate values of time varying parameters
  # isf <- function(t=0) {k=ISF*1.0; if(t>.1*N )k=.9*k; if(t>.2*n)k=.8*k; if(t>.3*N)k=.7*k;if(t>.4*N)k=.5;return(k)} # infection spread factor
  isf <- function(t=0) {k=ISF; if(t>=SDD)k=SDSF*ISF; return(k)} # infection spread factor
  cfp <- function(t=0) {return(CFP)} # case fatatility proportion
  
  # Distribute case states on timeline: infectious->confirmed, confirmed->recovered, confirmed->dead
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
  cat(" fun: ", ISF, daysI, daysR, daysD, SDD)
  
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
parms=c(0.65,7,20,14,0.05, 8,0, 8,4)
parms=c(1.00,7,21,14,0.05,21,0,70,4)
gt <- CovGenICRD(parms)
dh <- gt %>% select(xt=Day, newInfectious, newConfirmed, newRecovered, newDeaths)
str(dh)
#dp <- gt %>% tidyr::gather(key=Status, val=Count, newInfectious, newConfirmed, newRecovered, newDeaths) %>% select(Day,Status,Count)
#ggplot(data=dp, aes(x=Day, y=Count, col=Status, shape=Status)) + geom_line() + geom_point(size=3) #+
  #ggtitle(paste("Ground Truth",parms[1],parms[2])) +
#  scale_y_continuous(trans="log10")
#  scale_x_continuous(breaks=1:10*7) 

