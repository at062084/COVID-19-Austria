library(deSolve)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(optimx)


icrd.fun <- function(parms) {
  # Parameters
  ISF=2
  daysI=2
  daysR=3
  daysD=5
  CFP=0
  SDD=9
  SDSF=0
  N=16
  
  # Parameters for optimization
  ISF=parms[1]
  daysI=parms[2]
  daysR=parms[3]
  daysD=parms[4]
  CFP=parms[5]
  SDD=parms[6]
  SDSF=parms[7]
  N=parms[8]
  
  
  # Constants
  I <- 1
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
  mC[t,t:min(t+fCn,N)] <- fC[1:(min(t+fCn+1,N)-t)]
  
  cat(" fun: ", ISF, daysI, daysR, daysD, CFP, "\n")
  
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
parms=c(0.8,4,10,14,0.1,21,0.1,49)
(gt <- icrd.fun(parms))

#cat("\n")
# dp <- gt %>% tidyr::gather(key=Status, val=Count, Infectious, Confirmed, Recovered, Deaths, newInfectious, newConfirmed, newRecovered, newDeaths)
dp <- gt %>% tidyr::gather(key=Status, val=Count, Confirmed, Deaths, newInfectious, newConfirmed, newRecovered, curInfectious, curConfirmed)
gg <- ggplot(data=dp, aes(x=Day, y=Count, col=Status, shape=Status)) + geom_line() + geom_point(size=3) + ggtitle(paste("Ground Truth",parms[1],parms[2])) #+ scale_y_continuous(trans="log10")
print(gg)
 
icrd.rss <- function(params) {
  df <- icrd.fun(params)
  rss.in <- sum(gt$newInfectious-df$newInfectious)^2
  rss.cn <- sum(gt$newConfirmed-df$newConfirmed)^2
  rss.rn <- sum(gt$newRecovered-df$newRecovered)^2
  rss.dn <- sum(gt$newDeaths-df$newDeaths)^2
  #rss.i <- sum((gt$Infectious-df$Infectious)^2)
  #rss.r <- sum((gt$Recovered-df$Recovered)^2)
  rss <- rss.in + rss.rn
  cat(" rss: " , params, max(df$Infectious), 
      format(rss.in, scientific=TRUE, digits=3), 
      format(rss.cn, scientific=TRUE, digits=3), 
      format(rss.rn, scientific=TRUE, digits=3), 
      format(rss.dn, scientific=TRUE, digits=3),
      format(rss, scientific=TRUE, digits=3), "\n")  
  return(rss)
}

# Search for parameters that fit the generated ground truth data
optx <- optimx(par=c(1.5),
             fn=icrd.rss,
             method = c("Nelder-Mead"),
             #lower = c(1.0,10),
             #upper = c(2.0,20),
             # control=list(all.methods=TRUE, save.failures=TRUE, maxit=1000)
             control=list(save.failures=TRUE, maxit=10000)
             # control=list(ndeps=c(.01,1), parscale=c(1,1))
)


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




