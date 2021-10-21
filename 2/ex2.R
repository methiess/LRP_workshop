rm(list=ls())
library(ggplot2)
source("2/source.R") # contains functions "survivorship_F" and "MSYcalc"

# Read in the two data sets

AA <- read.csv("2/Ex_2_At_Age_Data.csv")[-1]
D <- read.csv("2/Ex2_Data.csv")[-1]

########################################################################################################################
# Move h/R0/phi0 parameterization of B-H SRR to a/b parameterization and calculate "unfished equilibrium SSB"
########################################################################################################################

# Model assumes a Beverton-Holt SRR with steepness h = 0.75
h <- 0.75 # model assumed steepness
R0 <- 3.505041 # model estimated equilibrium unfished recruitment [provided]
M <- 0.35 # model assumed natural mortality rate

# survivorship_F: function from source.R (calculates survivorship-at-age from f=F, M=M, waa=weight-at-age, mat=maturity-at-age, sel=vulnerability-at-age when F !=0)
# survivorship_F <- function(f=0,M,waa,mat,sel)
l_age <- survivorship_F(M=M,waa=AA$w_age,mat=AA$m_age) # survivorship-at-age (unfished, F=0)
phi0 <- sum(l_age*AA$w_age*AA$m_age) # unfished spawning biomass  per recruit

# B-H a and b
BHa <- 4*h/(phi0*(1-h)) # estimated Beverton-Holt a
BHb <- 1/R0*(BHa-1/phi0) # estimated Beverton-Holt b

# Unfished equilibrum SSB0
SSB0 <- R0/(BHa-BHb*R0)

########################################################################################################################
# Calculate equilibrium SSBmsy
########################################################################################################################

# MSYcalc: function from source.R returns a list with Fmsy, msy, SSBmsy from M=M, waa=weight-at-age, mat=maturity-at-age, sel=vulnerability-at-age, Beverton-Holt a and b
# MSYcalc <- function(M,waa,mat,sel,a,b)

calc <- MSYcalc(M=0.35,waa=AA$w_age,mat=AA$m_age,sel=AA$v_age,a=BHa,b=BHb)

Fmsy <- calc$Fmsy
msy <- calc$msy
SSBmsy <- calc$SSBmsy


########################################################################################################################
# Plots from data frame AA
########################################################################################################################

# Plots for Weight-at-age, Maturity-at-age, and vulnerability-at-age
ggplot(AA,aes(y=w_age/1000,x=age)) + geom_path() + theme_classic() + labs(x="Age", y="Weight (kg)") + expand_limits(y=0) + expand_limits(x=0) 
ggplot(AA,aes(y=m_age,x=age)) + geom_path() + theme_classic() + labs(x="Age", y="Maturity") + expand_limits(y=0) + expand_limits(x=0) 
ggplot(AA,aes(y=v_age,x=age)) + geom_path() + theme_classic() + labs(x="Age", y="Vulnerability") + expand_limits(y=0) + expand_limits(x=0) 

########################################################################################################################
# Plots from data frame AA
########################################################################################################################
#plot SR observations
ggplot(D[!is.na(D$Rec),],aes(y=Rec,x=SSB,label=Year)) + geom_point() + theme_classic() + labs(x="SSB (kt)", y="Recruitment (10^9)") + expand_limits(y=0) + expand_limits(x=0) +
  geom_text(mapping=aes(y=Rec,x=SSB,label=Year),nudge_y = 0.5,size=2) 
#plot SRR and observations
ggplot() + geom_point(D[!is.na(D$Rec),],mapping=aes(y=Rec,x=SSB)) + theme_classic() + labs(x="SSB (kt)", y="Recruitment (10^9)") + expand_limits(y=0) + expand_limits(x=0) +
  geom_function(fun=function(x) BHa*x/(1+BHb*x)) 
#Plot Historical Recruitment
ggplot(D[!is.na(D$Rec),],aes(y=Rec,x=Year)) + geom_path() + theme_classic() + labs(x="Year", y="Recruitment (10^9)") + expand_limits(y=0) 
#Plot Historical SSB 
ggplot(D,aes(y=SSB,x=Year)) + geom_path() +theme_classic() + labs(x="Year", y="SSB (kt)") + expand_limits(y=0)
#Plot Historical Catch
ggplot(D,aes(y=Catch,x=Year)) + geom_path() + theme_classic() + labs(x="Year", y="Catch (kt)") + expand_limits(y=0)
#Plot F
ggplot(D,aes(y=Apical_F,x=Year)) + geom_path() + theme_classic() + labs(x="Year", y="F") + expand_limits(y=0) 

#Plot dynamic SSB0, Equilibrium SSB0, Equilibrium SSBmsy
ggplot(D) + 
  geom_path(mapping=aes(y=SSB,x=Year)) +
  geom_path(mapping=aes(y=dynamic_SSB0,x=Year),color="blue") +
  theme_classic() + labs(x="Year", y="SSB (kt)") + expand_limits(y=0) +
  geom_hline(yintercept=SSB0, linetype="dashed", color = "blue") + 
  geom_hline(yintercept=SSBmsy, color = "green") 

#Plot Acoustic Index
ggplot(D[!is.na(D$Acoustic_Index),]) + geom_path(mapping=aes(y=Acoustic_Index,x=Year)) +
  theme_classic() + labs(x="Year", y="Acoustic SSB (kt)") + expand_limits(y=0,x=1968)
