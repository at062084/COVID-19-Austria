library(optimx)


# Local data store
source("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk/COVID-19-AGES-Data.R")
setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk")
df <- caAgesRead_cftl)(

# Local configuration
setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/CovFit")

# add rolling means
dfrm <- df %>%
  dplyr::arrange(Date, Region) %>%
  dplyr::group_by(Region) %>%
  dplyr::mutate_at(vars(starts_with("new")), rollmean, k=7, align="center", fill=NA) %>%
  dplyr::ungroup()

# Select first 7 weeks in Wien, Österrreich
begDate=as.Date("2020-03-03")
endDate=as.Date("2020-04-22") # end of exponential decline. 7 weeks
aga <- dfrm %>% dplyr::filter(Region=="Österreich") %>% dplyr::filter(Date>=begDate & Date<=endDate)
agw <- dfrm %>% dplyr::filter(Region=="Wien") %>% dplyr::filter(Date>=begDate & Date<=endDate)

ggplot(aga,aes(x=Date,y=log(newConfirmed)))+geom_line()+geom_point() + geom_line(data=agw)
ggplot(aga,aes(x=Date,y=(newConfirmed)))+geom_line()+geom_point() + geom_line(data=agw)



# ------------------------------------------------------------------
# COVID-19 Data generators for first seven weeks after outbreak, including lock down on 2020-03-15
# ------------------------------------------------------------------

# Parameters: 
# p1/p2: Spread factor before/after lockdown effects visisble
# tx: day after outbreak that the lock down effects govern the spread (should estimate incubation period and human behavour)
CovGenI2 <- function(par, init, times, tx) {
  k1 <- par[1]
  k2 <- par[2]
  
  n <- times
  n[1] <- init[1]
  for (i in 2:length(times)) {
    if (times[i]<=tx) {
      n[i] <- n[i-1]*k1
    } else {
      n[i] <- n[i-1]*k2
    } 
  }
  return(n)
}

# Parameters: 
# p1/p2: Spread factor before/after lockdown effects visisble
# s1: Damping of spread even before lockdown effects visisble. Maybe models depletion of susceptibles in the neighbourhood of infected
# tx: day after outbreak that the lock down effects govern the spread (should estimate incubation period and human behavour)
CovGenI3 <- function(par, init, times, tx) {
  k1 <- par[1]
  k2 <- par[2]
  s1 <- par[3] # damping factor increasing from day 1 of outbreak
  
  n <- times
  n[1] <- init[1]
  for (i in 2:length(times)) {
    if (times[i]<=tx) {
      n[i] <- n[i-1]*k1*(1-i*s1)
    } else {
      n[i] <- n[i-1]*k2
    } 
  }
  return(n)
}

# Parameters: 
# p1/p2: Spread factor before/after lockdown effects visisble
# s1: Damping of spread even before lockdown effects visisble. Maybe models depletion of susceptibles in the neighbourhood of infected
# tx1: day after outbreak that the lock down effects govern the spread (should estimate incubation period and human behavour)
# tx2: day after outbreak that the lock down effects govern the spread (should estimate incubation period and human behavour)
CovGenI4 <- function(par, init, times, tx) {
  k1 <- par[1] # Increase: infection + incubation
  k2 <- par[2] # damped increase after lockdown: incubation period
  s1 <- par[3] # damping factor increasing after lockdown until break down of infection after lockdown effect prevails
  
  n <- times
  n[1] <- init[1]
  for (i in 2:length(times)) {
    if (times[i]<=tx[1]) {
      n[i] <- n[i-1]*k1
    } else if (times[i]<=tx[2]) {
      n[i] <- n[i-1]*k1*(1-(i-tx[1])*s1)
    } else {
      n[i] <- n[i-1]*k2
    } 
  }
  return(n)
}

# ------------------------------------------------------------------------------------------------
# COVID-19 Parameter Estimation: objective functoin for optimization with optimx using LS and MLE
# These objective functions only cover the continous parameters
# None of the optimx provided methods can handle integer parameters
# In fact, there is not a single method in R that can handle integer parameters for nonlinear optimization
# Thus, the integer parameters (dates in this case) will be handled as hyper parameters in manual loops
# ------------------------------------------------------------------------------------------------

# LS objective function
oss <- function(par, init, times, tx, y, CovGen="CovGenI3") {
  x <- CovGenI3(par=par, init=init, times=times, tx=tx) # from globals
  ss <- sum((x-y)^2)
  return(ss)
}

# use logLikelihood function of normal distribution
nll.n <- function(par, init, times, tx, y, CovGen="CovGenI3") {
  x <- CovGenI3(par=par, init=init, times=times, tx=tx) # from globals
  
  # log lik for normal distribution
  n <- length(times)
  m <- y
  s <- y/10
  ll <- -(n/2)*(log(2*pi*s^2)) + (-1/(2*s^2)) *((x-m)^2)
  return(-sum(ll))
}

# Optimize with 3 Continuous Parameters and one Integer
# use dnorm function instead of ll of normal distribution
# This works. do not change !
nll.n3 <- function(par, init, times, tx, y, CovGen="CovGenI3") {
  x <- CovGenI3(par=par, init=init, times=times, tx=tx) # from globals
  
  # This works well:
  # estimate sd as common sd, 
  # calc likelihood of generated count from N~(mean of measured data, common sd)
  n <- length(times)
  sd <- sqrt(sum(y-x)^2)/n
  ll <- -sum(dnorm(y,mean=x,sd=sd, log=TRUE))
  return(ll)
}

# Optimize with 3 Continuous Parameters and one Integer:
# The same as above, but with optional log transformed spread factors
nll.n3l <- function(par, init, times, tx, y, CovGen=CovGenI3, bLogTrans=FALSE) {
  if (bLogTrans==TRUE) {
    # cat("nll.n3l: ", exp(par),"'\n'")
    par <- exp(par)
  }
  x <- CovGen(par=par, init=init, times=times, tx=tx)
  
  n <- length(times)
  sd <- sqrt(sum(y-x)^2)/n
  # must have x as mean here, otherwise optimization does not work !!!
  ll <- -sum(dnorm(y,mean=x,sd=sd, log=TRUE))
  return(ll)
}

nll.pois <- function(par, init, times, tx, y, CovGen=CovGenI3, bLogTrans=FALSE) {
  if (bLogTrans==TRUE) {
    # cat("nll.n3l: ", exp(par),"'\n'")
    par <- exp(par)
  }
  x <- CovGen(par=par, init=init, times=times, tx=tx)
  ll <- -sum(dpois(round(x),lambda=y, log=TRUE))
  return(ll)
}


# ------------------------------------------------------------------------------------------------
# COVID-19 Parameter Estimation: objective functoin for optimization with optimx using LS and MLE
# Optimzation experiments Part 1: Parameter estimates for a single option for the 'LockDownEffectiveDay tx'
# ------------------------------------------------------------------------------------------------

# Initial parameters
init=1
times=1:49
par=c(1.5,0.9,0.01)
TX=25

# Single optimize run: must pass arguments to objective function in optimx '...' section
y <- aga$newConfirmed[1:length(times)]

# sample results of parameter driven data generator functions
plot(times, CovGenI3(init=1,time=times, par=c(1.7,.9,.015), tx=c(TX)), type="l", log="y"); lines(y, col="red"); grid()
plot(times, CovGenI4(init=1,time=times, par=c(1.45,.9,.025), tx=c(13,TX)), type="l", log="y"); lines(y, col="red"); grid()


# Methods available in optimx
# methods <- c('Nelder-Mead', 'BFGS', 'CG', 'L-BFGS-B', 'nlm', 'nlminb', 'spg', 'ucminf', 'newuoa', 'bobyqa', 'nmkb', 'hjkb', 'Rcgmin', 'Rvmmin')
#ox <- optimx(par=c(1.01,0.01), oss, control=list(all.methods=TRUE)) 

# LS: 3 Parameters, single day
# ----------------------------
(oxls <- optimx(par=c(1.5,0.9,0.1), oss, method="Nelder-Mead", init=1, times=times, tx=TX, y=y) )
plot(times, CovGenI3(init=1, times=times, par=c(oxls$p1,oxls$p2,oxls$p3), tx=TX))
lines(times,y)

# MLE: 3 Parameters, single day
# -----------------------------
(oxnll <- optimx(par=c(1.5,0.9,0.01), nll.n3, method="Nelder-Mead", init=1, times=times, tx=TX, y=y))
plot(times, CovGenI3(init=1, times=times, par=c(oxnll$p1,oxnll$p2,oxnll$p3), tx=TX))
lines(times,y)

# MLE: 3 Parameters, single day, with log transformed parameters
# --------------------------------------------------------------
(oxlll <- optimx(par=log(c(1.5,0.9,0.01)), nll.n3l, method="Nelder-Mead", control=list(trace=0), init=1, times=times, tx=TX, y=y, bLogTrans=TRUE) )
exp(c(oxlll$p1,oxlll$p2,oxlll$p3))
plot(times, CovGenI3(init=1, times=times, par=exp(c(oxlll$p1,oxlll$p2,oxlll$p3)), tx=TX))
lines(times,y)

# MLE: Same as above, put pass data generation routine as parameter
# -----------------------------------------------------------------
(oxllp <- optimx(par=log(c(1.5,0.9,0.01)), nll.n3l, method="Nelder-Mead", control=list(trace=0), init=1, times=times, tx=TX, y=y, CovGen=CovGenI3, bLogTrans=TRUE))
exp(c(oxllp$p1,oxllp$p2,oxllp$p3))
plot(times, CovGenI3(init=1, times=times, par=exp(c(oxllp$p1,oxllp$p2,oxllp$p3)), tx=TX))
lines(times,y)

# plot of first results that optimze parameters for a single day
# --------------------------------------------------------------
main="COVID-19 Austria first 50 days: Estimation of spread parameters"
sub="Single day optimization (tx=25). Red=LS, Blue=nll, Black=nll(logTransformed)"
plot(times, y, main=main, sub=sub, type="p")
lines(times, CovGenI3(init=1, times=times, par=c(oxnll$p1,oxnll$p2,oxnll$p3), tx=TX), col="red")
lines(times, CovGenI3(init=1, times=times, par=c(oxls$p1,oxls$p2,oxls$p3), tx=TX), col="blue")
lines(times, CovGenI3(init=1, times=times, par=exp(c(oxlll$p1,oxlll$p2,oxlll$p3)), tx=TX), col="black", lty=2, lwd=2)
lines(times, CovGenI3(init=1, times=times, par=exp(c(oxllp$p1,oxllp$p2,oxllp$p3)), tx=TX), col="green", lty=3, lwd=3)



# ------------------------------------------------------------------------------------------------
# COVID-19 Parameter Esttimation: objective functoin for optimization with optimx using LS and MLE
# Optimzation experiments Part 2: Hyper parameter tuning for the integer parameters
# ------------------------------------------------------------------------------------------------

# Optimization for data generator CovGenI3 (p1, p2, s1, integer(tx))
init=1
times=1:49
CovGen <- CovGenI3
par=c(1.5,0.9,0.01)
TX <- 10:45
SS <- matrix(NA, nrow=length(TX), ncol=4)
y <- aga$newConfirmed[1:length(times)]

for (i in 1:length(TX)) {
  ox <-  optimx(par=log(par), nll.n3l, method="Nelder-Mead", init=1, times=times, tx=TX[i], y=y, CovGen=CovGen, bLogTrans=TRUE)
  SS[i,1] <- exp(ox$p1)
  SS[i,2] <- exp(ox$p2)
  SS[i,3] <- exp(ox$p3)
  SS[i,4] <- ox$value
}
plot(TX,SS[,4])
txm <- which(SS[,4]==min(SS[,4]))
SS[txm,]
(tx=TX[txm])
x <- CovGenI3(init=1, times=times, par=(c(SS[txm,1],SS[txm,2],SS[txm,3])),tx=tx)
plot(times,y, ylim=c(-100,800))
lines(times,x)
points(x-y, cex=.5, pch=16)
grid()
# -----------------------------------------------------------



# Optimization for data generator CovGenI4 (p1, p2, s1, integer(tx1,tx2,init))
times=1:49
TT=49
I=15
SS <- matrix(NA, nrow=TT*TT*I, ncol=7)
CovGen <- CovGenI4
par=c(1.5,0.9,0.01)
y <- aga$newConfirmed[1:length(times)]

# subset of hyper parameter space after initial testrun
for (i in seq(2,16,by=1)) {
  for(j in seq((i+7),(TT-16),by=1)) {
    for(k in 7:11) {
      ox <-  optimx(par=log(par), nll.pois, method="Nelder-Mead", init=k, times=times, tx=c(i,j), y=y, CovGen=CovGen, bLogTrans=TRUE)
      id <- ((i-1)*TT+j-1)*I+k 
      SS[id,1] <- exp(ox$p1)
      SS[id,2] <- exp(ox$p2)
      SS[id,3] <- exp(ox$p3)
      SS[id,4] <- ox$value
      SS[id,5] <- i
      SS[id,6] <- j
      SS[id,7] <- k
      cat(id,i,j,k,ox$value,exp(c(ox$p1,ox$p2,ox$p3)),"\n")
    }
  }
}
colnames(SS)=c("p1","p2","s1","val","SD","EFF","INIT")
ST <- data.frame(SS)

# Extract parameters with maximum likelihood
SSC <- ST[complete.cases(ST),]
txm <- which(SSC[,4]==min(SSC[,4]))
SSC[txm,]

# Plot likelihood surface (closeup with filter)
ggplot(data=data.frame(SSC) %>% dplyr::filter(INIT==SSC[txm,7], val<min(val*1.025)), aes(x=SD, y=EFF, fill=val)) +
  geom_tile() + scale_fill_gradient(low="white", high="black")

# Plot of model and data
x <- CovGen(init=SSC[txm,7], times=times, par=c(SSC[txm,1],SSC[txm,2],SSC[txm,3]),tx=c(SSC[txm,5],SSC[txm,6]))
plot(times,y, log="y")
lines(times,x)
