


# -----------------------------------------------------------------------------------------
# Deterministic pomp model with cases by day mplemented by rpois
# --> large variations on actual cases per day -> large variations on logLik on parameter estimation
# --> no deterministic path for logik optimization -> need to resort to mean value of many simulation runs 
# --> not feasible --> abandon rpois approch
# new cases by day implementation by dpois. see current code
# -------------------------------------------------------------------------------------------

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
  #cat('ic_dmfun:', t, newConfirmed, C, log, d, '\n')
  #dpois(newConfirmed, lambda=C, log=log)
  #d <- dpois(newConfirmed, lambda=C, log=log)
  #if (d==-Inf) d=2e-307
  #if (d==Inf) d=2e+307
  # d
}

# parameter grid for logLik surface
ic_pgrid <- expand.grid(
  ISF=seq(0.45,0.65,by=0.01),
  SDSF=seq(0.25,0.45,by=0.01)
)
p=data.frame(ISF=.53,SDSF=0.25)


# Calculate 32 logLik surfaces
K=32
for (k in 1:K) {
  cat(k,' ')
  pl <- foreach(parms=iter(ic_pgrid,"row"), 
                .combine=rbind, 
                .inorder=FALSE, 
                .options.multicore=list(set.seed=TRUE)) %dopar%  { ic_pomp(parms, bPlot=FALSE, bLogLik=FALSE) }
  pl$k=k
  if(k==1) 
    plm <- pl
  else
    plm <- rbind(plm,pl)
}

# plot logLik
plmm<- plm %>% group_by(ISF,SDSF) %>% summarize(sumLogLik=sum(logLik), sdLogLik=sd(logLik)) %>% ungroup()
plmm[which(plmm$sumLogLik==min(plmm$sumLogLik)),]
ggplot(data=plmm, aes(x=ISF, y=SDSF, fill=log(sumLogLik))) +
  geom_tile() + 
  scale_fill_gradient(low="white", high="black") +
  ggtitle(paste("ISF=seq(0.45,0.65,by=0.005)","SDSF=seq(0.25,0.45,by=0.005)"))
fileName=paste0("./CovFit/plots/",paste("ic_pomp","logLik-%03d",sep="_"), ".png")
ggsave(fileName, gg, scale=2, width=4, height=3, dpi=300, units="in")


ic_pomp <- function(parms, bPlot=FALSE, bLogLik=TRUE, nGenerates=16) {
  
  p <- data.frame(parms)
  
  # calculate 'Hidden Data' as stochastic process with infectious period poison distributed
  # parameters: ISF: Disease Spread Factor during Infectious period. SDSF: Spread Factor after ShutDown
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
    fileName=paste0("./CovFit/plots/",paste("ic_pomp",p[1,1],p[1,2],round(p[1,3]),sep="_"), ".png")
    
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
head(pl)


gg <- ggplot(data=pl, aes(x=ISF, y=SDSF, fill=log(logLik))) +
  geom_tile() + 
  scale_fill_gradient(low="white", high="black")
fileName=paste0("./CovFit/plots/",paste("ic_pomp","logLik-%3d",sep="_"), ".png")
ggsave(fileName, gg, scale=2, width=4, height=3, dpi=300, units="in")

pl[which(pl$logLik==min(pl$logLik)),]

ic_pomp(c(0.5,.35), bPlot=TRUE, bLogLik=FALSE)

sp <- subplex(c(0.50,.40),fn=ic_pomp, control=list(reltol=1e-3, parscale=.1))
sp$par
ic <- CovGenIC(sp$par) %>% select(icInfectious=newInfectious, icConfirmed=newConfirmed )
dg <- cbind(df,ic)
ggplot(data=dg, aes(x=xt, y=newConfirmed)) + geom_line() + geom_point() +
  geom_line(aes(y=icConfirmed)) + geom_point(aes(y=icConfirmed)) +
  ggtitle(paste(round(sp$par[1],3),round(sp$par[2],3)))

