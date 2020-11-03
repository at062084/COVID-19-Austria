library(optimx)
library(ROI)
library("EstimationTools")

# ------------------------------------------------------------------
# Examle 1: Exponential growth
# ------------------------------------------------------------------
R <- 1.1
t <- 0:10*1.0

# exponential growth
n <- round(exp(R*t))
s <- n
# add jitter
for (k in 1:length(t)) {
  s[k] <- ceiling(rnorm(1,mean=n[k], sd=n[k]/3))
}
m <- matrix(c(t,s), ncol=2)

# likelihood density function
dEP <- function(x,R,log=FALSE) {
  lambda <- exp(x[,1]*R)
  dL <- dpois(x=x[,2], lambda=lambda, log=log)
  return(dL)
}

# mle estimation of R
mlL <- maxlogL(x=m, dist="dEP", start=1.0)
summary(mlL)


plot(t, abs(exp(mlL$fit$par*t)-s), type="b", log="y")
points(t,s, pch=16, col="blue")

lm(log(s)~t)


# ------------------------------------------------------------------
# Example 2: 2 Phase Exponential growth
# ------------------------------------------------------------------
R <- 1
R1 <- 2
R2 <- .5
S <- 10
T <- 20

t <- 0:T*1.0
n <- R*R1^t
n[S:T+1] <- n[S+1] * R2^(t[S:(T+1)-S])

plot(n, type="l", log="y")


# exponential growth
s <- n
# add jitter
for (k in 1:length(t)) {
  s[k] <- round(rnorm(1,mean=n[k], sd=n[k]/10))
}
x <- matrix(c(t,s), ncol=2)

# likelihood density function
dEP2 <- function(x,R1,R2,S,R,log=FALSE) {
  t <- x[,1]
  T <- length(t)-1
  n <- R*R1^t
  n[S:T+1] <- n[S+1] * R2^(t[S:T-S+1])
  lambda <- n
  
  dL <- dpois(x=x[,2], lambda=lambda, log=log)
  return(dL)
}

fnmap_f <- function(x) c(x[1], x[2], round(x[3]))

# mle estimation of R
mlL <- maxlogL(x=x, dist="dEP2", start=c(1.0, .75), 
               fixed=list(S=10, R=1), 
               link=list(over=c("R1","R2"), fun=c("log_link", "log_link")),
               optimizer="nlminb")
summary(mlL)

# OK: optim(BFGS,CG,L-BFGS-B,SANN), DEoptim(nok), nlminb(without lower/upper)
# lower=c(1.5,0.1), upper=c(2.5,1.0)

mlLS <- function (S) {
  l <- maxlogL(x=x, dist="dEP2", start=c(1.0, .75), 
          fixed=list(S=S, R=1), 
          link=list(over=c("R1","R2"), fun=c("log_link", "log_link")),
          optimizer="nlminb")
  return(l)
}


O <- matrix(nrow=11,ncol=4)
for (i in 5:15) {
  o <- mlLS(i)
  O[i-4,1] <- i
  O[i-4,2:3] <- o$fit$par
  O[i-4,4] <- o$fit$objective
}

plot(O[,1],O[,4])






# ------------------------------------------------------------------
# AGES data for Wien for first 7 weeks after outbreak
# ------------------------------------------------------------------

begDate=as.Date("2020-03-03")
endDate=as.Date("2020-04-08") # end of linear decline. 5 weeks
endDate=as.Date("2020-04-24") # end of first log decline. 7 weeks
endDate=as.Date("2020-05-04") # end of second log decline. 9 weeks
aga <- dfrm %>% dplyr::filter(Region=="Österreich") %>% dplyr::filter(Date>=begDate & Date<=endDate)
agw <- dfrm %>% dplyr::filter(Region=="Wien") %>% dplyr::filter(Date>=begDate & Date<=endDate)
ggplot(aga,aes(x=Date,y=log(newConfirmed)))+geom_line()+geom_point() + geom_line(data=agw)
ggplot(aga,aes(x=Date,y=(newConfirmed)))+geom_line()+geom_point() + geom_line(data=agw)


CovGenI <- function(par, init, times, tx) {
  k1 <- par[1]
  k2 <- par[2]
  s1 <- par[3] # damping factor to simulate depletion of near infectibles
  N0 <- init[1]
  t <- times

  n <- times
  n[1] <- N0
  for (i in 2:length(times)) {
    if (times[i]<=tx) {
      n[i] <- n[i-1]*k1*(1-i*s1)
    } 
    if (times[i]>tx) {
      n[i] <- n[i-1]*k2
    } 
  }
  return(n)
}

N0=1
times=1:35
plot(times, CovGenI(1,times,c(2,.8,21)), type="l")
plot(times, CovGenI(1,times,c(2,.8,21)), type="l", log="y")
plot(times, CovGenI(1,times,c(2,.5,21)), type="l")

oss <- function(par, init, times, tx, y) {
  x <- (CovGenI(par=par, init=init, times=times, tx)) # from globals
  #y <- log(y)
    ss <- sum((x-y)^2)
    return(ss)
}

#methods <- c('Nelder-Mead', 'BFGS', 'CG', 'L-BFGS-B', 'nlm', 'nlminb', 'spg', 'ucminf', 'newuoa', 'bobyqa', 'nmkb', 'hjkb', 'Rcgmin', 'Rvmmin')
#ox <- optimx(par=c(1.01,0.01), oss, control=list(all.methods=TRUE)) 
ox <- optimx(par=c(1.5,0.9,0.01), oss, method="Nelder-Mead", init=1, times=times, tx=20, y=y) 
summary(ox)

plot(times, CovGenI(init=N0, times=times, par=c(ox$p1,ox$p2), tx=10))
lines(times,y)

N0=1
times=1:35
TX <- 15:25
SS <- matrix(NA, nrow=length(TX), ncol=4)
y <- aga$newConfirmed[1:length(times)]
for (i in 1:length(TX)) {
    #ox <-  optimx(par=c(1.01,0.01), oss, method="nlminb", init=c(N0), times=times, tx=TX[i], y=y)
    ox <-  optimx(par=c(1.5,0.8,.01), oss, method="Nelder-Mead", init=c(N0), times=times, tx=TX[i], y=y)
    SS[i,1] <- ox$p1
    SS[i,2] <- ox$p2
    SS[i,3] <- ox$p3
    SS[i,4] <- ox$value
}

plot(TX,SS[,4])
txm <- which(SS[,4]==min(SS[,4]))
SS[txm,]
tx=TX[txm]
plot(times, CovGenI(init=N0, times=times, par=c(SS[txm,1],SS[txm,2],SS[txm,3]), tx=tx))
lines(times,y)

plot(times, CovGenI(init=N0, times=times, par=c(SS[txm,1],SS[txm,2],SS[txm,3]), tx=tx), log="y")
lines(times,y)

