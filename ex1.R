rm(list=ls())
library(ggplot2)
L <- read.csv("ex1_data/landings.csv")
D <- read.csv("ex1_data/ex1.csv")
D$CPUE <- (D$PS_Catch/1000)/D$Effort

head(L)
head(D)

############################################################
#Plots
############################################################
#landings by management component
ggplot() + geom_area(data=L, mapping=aes(y=Landings/1000,x=Year,fill=MU)) +
  theme_classic() + labs(x='Year', y="Landings (kt)") +
  scale_x_continuous(limits = c(1960,2020), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,300), expand = c(0, 0)) 

#SWNS/BoF PS Catch
ggplot(D,aes(y=PS_Catch/1000,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="Purse Seine Landings (t)") + expand_limits(y=0)

#SWNS/BoF PS Effort
ggplot(D,aes(y=Effort,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="Purse Seine Effort (trips)") + expand_limits(y=0)

#SWNS/BoF PS CPUE
ggplot(D,aes(y=CPUE,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="Purse Seine CPUE (kt/trip)") + expand_limits(y=0)

#4VWX RV survey
ggplot(D,aes(y=RV_4VWX/1000,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="4VWX Survey Biomass Index (t)") + expand_limits(y=0) +
  geom_smooth(span=0.25,se=F)

#SWNS/BoF RV survey 
ggplot(D,aes(y=RV_SWNSBoF/1000,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="SWNS/BoF Survey Biomass Index(kt)") + expand_limits(y=0) +
  geom_smooth(span=0.25,se=F)

#SWNS/BoF Acoustic Survey
ggplot(D,aes(y=AI_SWNSBoF/1000 ,x=Year)) + geom_path() +
  theme_classic() + labs(x='Year', y="Acoustic Index of SSB (kt)") + 
  scale_x_continuous(limits = c(1967,2021), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1.1*max(D$AI_SWNSBoF/1000,na.rm = T)), expand = c(0, 0))  +
  geom_smooth(span=0.5,se=F)
  

max1=max(D$AI_SS/1000,na.rm = T)
max2=max(D$AI_ES/1000,na.rm = T)
#SS and ES Acoustic Surveys
ggplot(D) + 
  geom_path(mapping=aes(y=AI_SS/1000 ,x=Year)) +
  geom_smooth(mapping=aes(y=AI_SS/1000 ,x=Year), span=0.5,se=F) +
  ggtitle("South Shore") +
  theme_classic() + labs(x='Year', y="Acoustic Index of SSB (kt)") + 
  scale_x_continuous(limits = c(1967,2021), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1.1*max(max1,max2)), expand = c(0, 0))
ggplot(D) + 
  geom_path(mapping=aes(y=AI_ES/1000 ,x=Year)) +
  geom_smooth(mapping=aes(y=AI_ES/1000 ,x=Year), span=0.5,se=F) +
  ggtitle("Eastern Shore") +
  theme_classic() + labs(x='Year', y="Acoustic Index of SSB (kt)") + 
  scale_x_continuous(limits = c(1967,2021), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1.1*max(max1,max2)), expand = c(0, 0))
