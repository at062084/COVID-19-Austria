

# Well's curve (1934)
R <- 10^seq(-1,2,by=.01)
ts <- 1/R^2
te0 <- R^2/(1-.1)
te1 <- R^2/(1-.5)
te2 <- R^2/(1-.9)

main="Well's curve (1935)"
plot(ts~R, type="l", log="x", ylim=c(2.5,0), col="red", main=main)
lines(ts/2~R, type="l", col="darkred")
lines(ts*2~R, type="l", col="orange")
lines(te0~R, type="l", col="lightblue")
lines(te1~R, type="l", col="blue")
lines(te2~R, type="l", col="darkblue")
grid()



# ---------------------------------------------------------
# HomeWork Chapter 4
# ---------------------------------------------------------

A = 185.8
H = 3.05
V = A*H 
Cq = 72
la = 12
lv = 0.36
Qb = 0.49
Pm = 1
epsilon = 0.1
r = 2.5  # mu
RH = .6


#vs <- 0.9382*(1-RH)^(-2/3) # [m/h] # wrong in script
vs <- 1.466*(1-RH)^(-2/3) # [m/h]
ls <- vs/H

# Sum up all releasing factors
lc = la + lv + ls

# Preparation Questions 1-2
# N * T = epsilon/beta
beta = (Qb^2*Pm^2*Cq)/(lc*V)
NT = epsilon / beta

# Question 1  
N0 = 80
Pm = 1
(T0 = NT/N0 * 60)

# Question 2
N1 = 185.8/3.34
(T1 = NT/N1 * 60)

# Preparation Questions 3-5
Pmf = .75  # mask filteration capacity
Pme = 0.9  # mask efficiency
Pm = 1 - Pmf*Pme
beta = (Qb^2*Pm^2*Cq)/(lc*V)
NT = epsilon / beta

# Question 3
NO = 80
(TM = NT/(N0-1))

# Question 4
T=40
(N = NT/T +1)

# Question 5: pi in %
# N2 * T = epsilon/beta
N = 80
T = 40
N2T = N^2 * T
(pi <- NT/N2T * 100)

# ---------------------------------------------------------
# Final Exam
# ---------------------------------------------------------

# Room
A=83.6
H=3.66
V=A*H
N0=30
T0=30 # (6h, 5d)
# Air
la=5.4
lf=3.6
pf=0.5 # MERV-6 vent filters
pflf=pf*lf
RH=0.2
# Mask
Pmesc=0.75 # surgical or cotten
Pmegc=0.95 # goot compliance
Pme=0.288 # effective mask penetration probability
# People
Qb=0.49
Cq=72
# Other parameters
lvmax=0.6
lv=lvmax*RH 
# App
#  lv=0.6 # RH=100%
# SpreadSheet
#  lv=0.3 # RH=50%

# Droplet settling speed
vs=0.9382*(1-RH)^(-2/3)
vsH=vs/H
# App
#  RH=0.6
#  rbar=2 # mu

lc = la + pflf + lv + vsH

# Question 1
# N=2, one infected, one hour --> Rin==beta
Rin = (2-1)beta*1 
(beta = (Qb^2*Pme^2*Cq)/(lc*V)) # Steady transmission rate: transmissions/hour

# Question 2
epsilon=0.1
# Number of transmission with one infector
#  Rin = (N0-1)*beta*T0 < epsilon
(beta = epsilon/T0/(N0-1)) # maximum allowable transmissions in 30h

# Question 3
(beta = (Qb^2*Pme^2*Cq)/(lc*V))
(pi = epsilon/N0/(N0-1)/beta/T0*100)

# Question 4
# Original setup
lf=3.6
pf=0.5 # MERV-6 vent filters
pflf=pf*lf
lc = la + pflf + lv + vsH
(beta1 = (Qb^2*Pme^2*Cq)/(lc*V))

# improved filters
lf=3.6
pf=0.9997 # MERV-6 vent filters
pflf=pf*lf
lc = la + pflf + lv + vsH
(beta2 = (Qb^2*Pme^2*Cq)/(lc*V))

(beta2-beta1)/beta1

# Question 5
#Original setup
lf=3.6
pf=0.5 # MERV-6 vent filters
pflf=pf*lf
Pmesc=0.75 # surgical or cotten
Pmegc=0.95 # goot compliance
Pme=0.288 # effective mask penetration probability
lc = la + pflf + lv + vsH
(beta1 = (Qb^2*Pme^2*Cq)/(lc*V))

# improved masks
Pme=0.193 # effective mask penetration probability
(beta2 = (Qb^2*Pme^2*Cq)/(lc*V))

(beta2-beta1)/beta1*100

# no masks
Pme=1 # effective mask penetration probability
(beta3 = (Qb^2*Pme^2*Cq)/(lc*V))

(beta3-beta1)/beta1*100


# Question 6
#Original setup
la=5.4
lf=3.6
pf=0.5 # MERV-6 vent filters
pflf=pf*lf
Pmesc=0.75 # surgical or cotten
Pmegc=0.95 # goot compliance
Pme=0.288 # effective mask penetration probability
RH=0.2
lvmax=0.6
lv=lvmax*RH 
vs=0.9382*(1-RH)^(-2/3)
vsH=vs/H
lc = la + pflf + lv + vsH
(beta1 = (Qb^2*Pme^2*Cq)/(lc*V))

# Modified conditions: Higher RH, less la, more lf
RH=0.5
la=2.5
lf=6.5
pf=0.5 # MERV-6 vent filters
pflf=pf*lf
lvmax=0.6
lv=lvmax*RH
vs=0.9382*(1-RH)^(-2/3)
vsH=vs/H
lc = la + pflf + lv + vsH

(beta2 = (Qb^2*Pme^2*Cq)/(lc*V))
(beta2-beta1)/beta1*100


