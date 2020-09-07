# ---------------------------------------------------------------------------------------
# SIR Model
# ---------------------------------------------------------------------------------------
N <- 10000000
S0 <- 9999999
I0 <- 1
R0 <- 0
init <- c(S=S0,I=I0,R=R0)

t <- 1:100
beta <- 1
gamma <- 0.1
parameters <- c(beta=beta,gamma=gamma)
R0 <- beta/gamma 

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta * I * S / N
    dI <- beta * I * S / N - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

ODE <- ode(y=init, times=t, func=SIR, parms=parameters)
cov <- data.frame(ODE)

ggplot(data=cov %>% tidyr::gather(key=SIR, value=count, S,I,R), aes(x=time, y=count, color=SIR)) + 
  geom_line() +
  scale_y_continuous(trans = 'log10', breaks=10^(0:7))



# ---------------------------------------------------------------------------------------
# SEIRD Model with compartments for dead and recovered
# ---------------------------------------------------------------------------------------
N <- 10000000
S0 <- N-10
E0 <- 10
I0 <- 0
R0 <- 0
D0 <- 0
init <- c(S=S0,E=E0,I=I0,R=R0,D=D0)

t <- 1:365
dI <- 7    # days from infection till symptoms
dR <- 14   # days from symptoms to recover
dD <- 21   # days from symptoms to death 
R0 <- 5   #  

delta <- 1.0/dI   # 7 days incubation period
gamma <- 1.0/dR   # days from infection till recovery (2 weeks)
rho <-   1.0/dD   # days from infection till death (3 weeks)
alpha <- 0.05     # 5% death rate 
beta <-  R0*gamma
parameters <- c(beta=beta, delta=delta, alpha=alpha, gamma=gamma, rho=rho)

SEIRD <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta * S * I/N
    dE <-  beta * S * I/N - delta * E
    dI <-  delta * E - (1-alpha) * gamma * I - alpha * rho * I
    dR <- (1-alpha) * gamma * I
    dD <- alpha * rho * I
    list(c(dS, dE, dI, dR, dD))
  })
}

# General Solver for Ordinary Differential Equations
ODE <- ode(y=init, times=t, func=SEIRD, parms=parameters)
cov <- data.frame(ODE)

ggplot(data=cov %>% dplyr::select(-S) %>% tidyr::gather(key=SEIRD, value=count, E,I,R,D) , aes(x=time, y=count, color=SEIRD)) + 
  geom_line() +
  scale_y_continuous(trans = 'log10', breaks=10^(0:7))


# ---------------------------------------------------------------------------------------
# Fit parameters of model to data
# The solvers (ode) may be used as part of a modeling package for differential equations, 
# or for parameter estimation using any appropriate modeling tool for 
# non-linear models in R such as optim, nls, nlm or nlme, or FME.
# ---------------------------------------------------------------------------------------
df <- read.csv("data.csv")
Infected <- df$Infected

# define error function
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[, 3]
  sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5),    # beta, gamma
             RSS,
             method = "L-BFGS-B",
             lower = c(0, 0),
             upper = c(1, 1)
)
Opt$message


# ---------------------------------------------------------------------------------------
# Fit data to non-linear function
# ---------------------------------------------------------------------------------------
formula <- y ~ a + b*x + c*x2
model <- nls(formula, start, data [, parms])
predict(model)
cor(y,predict(model,x))
# nls.multstart::nls_multstart
# minpack.lm::nlsLM


# ---------------------------------------------------------------------------------------
# Fit data to parameterized differential equations
# ---------------------------------------------------------------------------------------
# differential equations with init, times and parms
func.ode <- function(time, State, Parms) {
  with(as.list(c(State, Parms)), {
    dN <- R*N*(1-N/K)
    return(list(c(dN)))
  })
}
init <- c(N=1)
parms <- c(R=.1, K=100)
times=1:100

# calculate values of func.ode at time times
vals <- ode(init, times, func.ode, parms)

# calculate LS error for observed vs. expected by ode 
func.rss <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  # calculate values of func.ode at time times
  out <- ode(y=init, times=times, func=func.ode, parms=parms)
  expected <- out[, 3]
  sum((oberved - expected)^2)
}

# minimize LS error using optim
opt <- optim(fn=func.rss, par=parms, data=data)
opt$par

# Parameter estimation by ML for poisson distributed data (could use 'fitdist(x,"Poission"))
# mean(p(k))=lambda, var(p(k))=lambda
log.lklh.poisson <- function(x, lambda) {                 # x = {1,2,3,...}
  # lklh <- p(k) <- lambda^k / fact(k) * exp(-lambda)     # p, dass k auftritt unter lambda
  # log(p(k)) < k*log(lambda -log(fact(k)) - lambda)
  -sum(x*log(lambda) - log(fact(x)) - lambda)             # negative for optim
}
# plot log.lklh.poisson for data under lambda=seq(min,max)

# maximize log likelyhood (minimize -log.lklh)
opt <- optim(fn=log.lklh.poisson, par=parms, data=data)
opt$par

# compact form:
f <- function(x,lambda) -sum(log(dpois(x,lambda))) 
a <- optimize(f, c(1,20), x=x)

# Parameter estimation by ML for normal distributed data (two parameters: mean, variance)
# compact form:
f <- function(x,parms) -sum(dnorm(x, parms[1], parms[2], log=TRUE))
o <- optim(f, c(mu,sigma), hessian=TRUE, x=x)
# ::bbmle: optim with some sensible defautls


# ---------------------------------------------------------------------------------------
# SIR model using MLE
# ---------------------------------------------------------------------------------------
require(deSolve)
sir <- function(t,x,parms){
  S <- x[1]
  I <- x[2]
  R <- x[3]
  with(as.list(parms),
       {
         dS <- -beta*S*I
         dI <- beta*S*I - nu*I
         dR <- nu*I
         res <- c(dS,dI,dR)
         list(res)
       })
}
# likelihood function
sirLL <- function(lbeta, lnu, logN, logI0) {
  parms <- c(beta=plogis(lbeta), nu=plogis(lnu))
  x0 <- c(S=exp(logN), I=exp(logI0), R=0)
  # integrate sir
  out <- ode(y=x0, weeks, sir, parms)
  # estimate MSE from data
  SD <- sqrt(sum( (cumbombay-out[,4])^2)/length(weeks) )
  # calc likelihood as sum of dnorm of data with mean and SD
  # nomally distributed errors
  -sum(dnorm(cumbombay, mean=out[,4], sd=SD, log=TRUE))
  # poisson distributed deaths
  -sum(dpois(cumbombay, lambda=out[,4], log=TRUE))
}
# minimize negative-log-likelihood
fit <- mle2(sirLL, 
            start=list(lbeta=qlogis(1e-5), 
                       lnu=qlogis(.2), 
                       logN=log(1e6), logI0=log(1) ),  
            method="Nelder-Mead",
            control=list(maxit=1E5,trace=0),
            trace=FALSE)

summary(fit)

# Recalling that β and γ must (by definition) be greater than one, 
# we will think of β and γ assimple transformations of two other parameters β=exp(b) and γ=exp(g).  
# The parametersbandgare definedfrom negative infinity to positive infinity.  
# This will help our numerical algorithms (e.g.,mle2) to behave better.

# Further, we will assume te observations to be Poisson distributed.  
# This seems appropriate, sincewe have discrete observations of cases and 
# it is natural to expect the variance to scale with the numberinfected.



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

dg <- df %>% tidyr::gather(key=Status, value=Count, confirmed, recovered, deaths, newcon)
ggplot(data=dg, aes(x=Date, y=Count, colour=Status)) + geom_line() + 
  scale_y_continuous(trans = 'log10', breaks=10^(0:6)) +
  scale_x_date(limits=c(min(dg$Date),min(dg$Date)+days(50)))



# ---------------------------------------------------------------------------------------
# SICRD Model with compartments for dead and recovered
# ---------------------------------------------------------------------------------------
# S: susceptible
# I: Infectious
# C: Confirmed, that is, Tested positiv
# R: Recovered
# D: Casualties
# ---------------------------------------------------------------------------------------

sicrd.ode <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <-  -S/N*(betaI*I + betaC*C)
    dI <-  S/N*(betaI*I + betaC*C) - rateI*1*I
    dC <-  rateI*1*I - rateR*(1-propD)*C - rateD*propD*C
    dR <-  rateR*(1-propD)*C 
    dD <-  rateD*propD*C
    list(c(dS, dI, dC, dR, dD))
  })
}
# Initial values
N <- 1e7
S0 <- N-1
I0 <- 1
C0 <- 0
R0 <- 0
D0 <- 0
init <- c(S=S0,I=I0,C=C0,R=R0,D=D0)

t <-0:10
daysI <- 1000    # days from infectious till symptoms=tested
daysR <- 1000   # days from tested to recover
daysD <- 1000   # days from tested to death 
propD <- 0.0*daysD/daysR   # proportion of deaths scaled by period in compartment C

rateI <- 1/daysI   # days from infectious till symptoms
rateR <- 1/daysR   # days from infection till recovery (2 weeks)
rateD <- 1/daysD   # days from infection till death (3 weeks)

# Infection is governed by individuals that have not been tested (quarantine, hospital if positiv)
betaI <- 1 
betaC <- 0

# General Solver for Ordinary Differential Equations
(parameters <- c(betaI=betaI, betaC=betaC, rateI=rateI, rateR=rateR, rateD=rateD, propD=propD))
cov <- data.frame(ode(y=init, times=t, func=sicrd.ode, parms=parameters, verbose=TRUE, hmin=0.01, hmax=0.1))
cov <- data.frame(ode(y=init, times=t, func=sicrd.ode, parms=parameters))
head(round(cov,2),10)
head(round(cov[seq(1,1000,100),],2),20)

# dplyr::select(-S)
ggplot(data=cov %>% tidyr::gather(key=SICRD, value=count, S,I,C,R,D) , aes(x=time, y=count, color=SICRD)) + 
  geom_line() +
  scale_x_continuous(breaks=seq(0,150,5)) +
  ggtitle((paste(round(betaI,2), round(betaC,2), daysI, daysR, daysD)))
scale_y_continuous(trans = 'log10', breaks=10^(0:7))


# calculate LS error for observed vs. expected by ode 
sicrd.rss <- function(parameters) {
  # parameters <- c("betaI", "betaC", "rateI", "rateR", "rateD", "propD")
  do <- ode(y=init, times=t, func=sicrd.ode, parms=parameters)
  
  # calculate rss for tested, recovered, deaths
  sum(df$confirmed-do[,"C"])^2 + sum(df$recovered-do[,"R"])^2 + sum(df$deaths-do[,"D"])^2
}

# minimize LS error using optim
opt <- optim(fn=sicrd.rss, par=parameters)
opt$par

ODE <- ode(y=init, times=t, func=sicrd.ode, opt$par)
cov <- data.frame(ODE)
tail(round(cov))

# dplyr::select(-S)
ggplot(data=cov %>% tidyr::gather(key=SICRD, value=count, S,I,C,R,D) , aes(x=time, y=count, color=SICRD)) + 
  geom_line() +
  # scale_y_continuous(trans = 'log10', breaks=10^(0:7), limits=c(0,10000))
  scale_y_continuous(limits=c(0,10000))


# icrd <- function(t, state, params) {
#icrd <- function(t) {
daysI <- 2
daysR <- 4
beta <- 1
N=64
nI=c(1,rep(0,N-1))
dI=c(1,rep(0,N-1))
tI=c(1,rep(0,N-1))
dC=rep(0,N)
tC=rep(0,N)
dR=rep(0,N)
tR=rep(0,N)
NI=sI=sC=sR=0
for (t in 2:N) {
  if (t>16) beta=0.25
  if(t <= daysI) {
    nI[t] <- round(tI[t-1]*beta)
    dI[t] <- nI[t]
    dC[t] <- 0
    dR[t] <- 0
    cat("A",t,nI[t],dI[t],tI[t],dC[t],tC[t],dR[t],tR[t],beta,"\n")
  }
  else if (t<=(daysI+daysR)) {
    nI[t] <- round(tI[t-1]*beta)
    dI[t] <- nI[t] - nI[t-daysI]
    dC[t] <-         nI[t-daysI]
    dR[t] <- 0
    cat("B",t,nI[t],dI[t],tI[t],dC[t],tC[t],dR[t],tR[t],beta,"\n")
  }
  else {
    nI[t] <- round(tI[t-1]*beta)
    dI[t] <- nI[t] - nI[t-daysI]
    dC[t] <-       + nI[t-daysI] - nI[t-daysI-daysR] 
    dR[t] <-                     + nI[t-daysI-daysR] 
    cat("C",t,nI[t],dI[t],tI[t],dC[t],tC[t],dR[t],tR[t],beta,"\n")
  }
  
  tI[t] <- tI[t-1] + dI[t]
  tC[t] <- tC[t-1] + dC[t]
  tR[t] <- tR[t-1] + dR[t]
  
  if (tI[t]<0) tI[t]=0
  if (tC[t]<0) tC[t]=0
  if (tR[t]<0) tR[t]=0
  
  NI <- NI + nI[t]
  sI <- sI + dI[t]
  sC <- sC + dC[t]
  sR <- sR + dR[t]
  cat("X",t,nI[t],dI[t],tI[t],dC[t],tC[t],dR[t],tR[t],beta,"\n")
}
#}

plot(1:N,log10(tR), col="red", type="l")
lines(1:N,log10(tC), col="green")
lines(1:N,log10(tI), col="blue")

#plot(1:N,(tR), col="red", type="l")
plot(1:N,(tR), col="blue", type="b")
lines(1:N,(tI), col="lightblue", type="b")
points(1:N,(tC), col="green", pch=16)
lines(1:N,(dI), col="darkblue", type="b")
lines(1:N,(tC), col="red")
lines(1:N,(tR), col="gray")
grid()


abline(v=1:50, lt=3, col="gray")
grid()

daysI <- 5
daysR <- 10
beta <- 1
N=50
I=c(1,rep(0,N-1))
C=rep(0,N)
R=rep(0,N)
IT=CT=RT=0
params <- c(beta=beta, daysI=daysI, daysR=daysR)
state <- list(I=I, C=C, R=R, IT=0, CT=0, RT=0)



for (t in 2:N) {
  state <- icrd(t)
}
state

plot(1:N,(state$C), type="l")
lines(1:N,(state$I), col="red")
lines(1:N,(state$R), col="blue")
grid()


plot(1:N,log10(state$C), type="l")
lines(1:N,log10(state$I), col="red")
lines(1:N,log10(state$R), col="blue")
grid()


plot(1:N,(tI), col="red", type="b", log="y")
lines (1:N,(tR), col="green")
lines(1:N,(cC), col="lightblue")
lines(1:N,(tC), col="blue")
lines(1:N,(cI), col="orange")
lines(1:N,(tD), col="black")
abline(v=0:10*7, col="grey")
abline(h=0:100*100, col="grey")

abline(v=1:70, col="grey")
abline(v=0:10*7, col="black")
abline(h=0:5)


# from: https://www.ibm.com/developerworks/library/ba-optimR-john-nash/index.html
weeds <- data.frame(y=ydat, x=tt)
weed.par <- anlxb1g$coefficients

weed.f <- function(b,mydata){
  sum((mydata$y-b[1]/(1+b[2]*exp(-b[3]*mydata$x)))^2)
}

weed.optx <- optimx(par=start1, fn=weed.f, mydata=weeds, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(weed.optx)

library(magrittr)
library(dplyr)
library(ggplot2)
library(lubridate)
cvr <- read.csv("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk/data/COVID-19-austria.regions.csv", stringsAsFactors=FALSE)
str(cvr)
df <- cvr %>% 
  dplyr::select(-Status) %>%
  dplyr::mutate(Date=as.Date(Stamp), Confirmed=Count) %>%
  dplyr::group_by(Date,Region) %>%
  dplyr::summarize(Confirmed=last(Confirmed)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(Date>=as.Date("2020-06-29")) %>%
  dplyr::filter(!is.na(Confirmed)) %>%
  dplyr::group_by(Region) %>%
  dplyr::mutate(newConfirmed=Confirmed-dplyr::lag(Confirmed, n=1, order_by=Date)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(newConfirmed)) %>%
  dplyr::group_by(Region) %>%
  dplyr::mutate(hotSpot=max(newConfirmed)>12) %>%
  dplyr::ungroup() %>%
  dplyr::filter(hotSpot==TRUE, Region!="Wien_Stadt", Region!="Linz-Land", Region!="Linz_Stadt")
str(df)
ggplot(data=df, aes(x=Date, y=newConfirmed, col=Region)) +  
  geom_point(aes(shape=Region), size=3) + 
  geom_line(aes(linetype=Region),size=1.5)
  