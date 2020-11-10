library(optimx)
#library(ROI)
library("EstimationTools")


# ------------------------------------------------------------------
# AGES data for Wien for first 7 weeks after outbreak
# ------------------------------------------------------------------

wd <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk"
setwd(wd)
source("./COVID-19-AGES-Data.R")

begDate=as.Date("2020-03-03")
endDate=as.Date("2020-04-22") # end of exponential decline. ~7 weeks
df <- caAgesRead_cftl()
dfrm <- df %>%
  dplyr::arrange(Date, Region) %>%
  dplyr::group_by(Region) %>%
  dplyr::mutate_at(vars(starts_with("new")), rollmean, k=7, align="center", fill=NA) %>%
  dplyr::ungroup()
aga <- dfrm %>% dplyr::filter(Region=="Österreich") %>% dplyr::filter(Date>=begDate & Date<=endDate)
agw <- dfrm %>% dplyr::filter(Region=="Wien") %>% dplyr::filter(Date>=begDate & Date<=endDate)
ggplot(aga,aes(x=Date,y=log(newConfirmed)))+geom_line()+geom_point() + geom_line(data=agw)
ggplot(aga,aes(x=Date,y=(newConfirmed)))+geom_line()+geom_point() + geom_line(data=agw)



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







