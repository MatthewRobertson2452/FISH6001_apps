


#how long will the simulation run for
yrs<-seq(from=2000, to=2050)
nyears<-length(yrs)

#number of ages for the population
nages<-15

#growth models
linf<-100
laa<-rep(NA, nages)
for(i in 1:15){
laa[i]<-linf*(1-exp(-0.5*(i-0.01)))
}
plot(laa~seq(from=1, to=nages, by=1))

waa<-rep(NA, nages)
b<-3
for(i in 1:15){
  waa[i]<-(0.001*laa[i]^b)/100
  }

plot(waa~seq(from=1, to=nages, by=1))

#maturity model
l50<-90
maa<-rep(NA, nages)
for(i in 1:15){
  maa[i]<-1/(1+exp(-0.5*(laa[i]-l50)))
}
plot(maa~seq(from=1, to=nages, by=1))

#lorenzen M if desired
#Mtrend<-0.2*(laa/max(laa))^-1
#M<-matrix(rep(Mtrend, each=nyears), nyears,nages)

#basic M
M<-matrix(0.2, nyears,nages)

#BH parameters
alpha=0.25
beta=-7

#objects for storage
N<-matrix(NA, nyears,nages)
log_N<-matrix(NA, nyears,nages)
B<-matrix(NA, nyears,nages)
F<-matrix(NA, nyears,nages)
Z<-matrix(NA, nyears,nages)
R<-rep(NA, nyears)
log_R<-rep(NA, nyears)
SSB<-rep(NA, nyears)


#defining initial conditions
starting_N<-100
N[1,1]<-starting_N
for(i in 2:nages){
N[1,i]<-N[1,i-1]*exp(-0.2)
}

log_N[1,]<-log(N[1,])
B[1,]<-N[1,]*waa
SSB[1]<-sum(B[1,]*0.5*maa)


#defining F trend
Fsel<-ifelse(maa<0.5, 0,1)

Ftrend<-rep(NA, nyears)
incF<-round(nyears*0.67,0)
Ftrend[1:incF]<-seq(from=0, to=0.2, length=incF)
Ftrend[(incF+1):nyears]<-0.2


for(i in 1:nyears){
F[i,] <- Ftrend[i]*Fsel
}

for(y in 1:nyears){
  for(a in 1:nages){
    if(Fsel[a]>0){
F[y,a]<-abs(F[y,a]+rnorm(1,0,0.1))
    } 
}
}


#the actual population dynamics
for(y in 2:nyears){
  R[y]<-abs((alpha*SSB[y-1])/(1+exp(beta) * SSB[y-1])+rnorm(1,0,50))
  N[y,1]<-R[y]
  B[y,1]<-N[y,1] * waa[1]
  
  for(a in 2:nages){
      M[y,a] <- M[y,a] + rnorm(1,0,0.05)
      N[y,a]<-N[y-1,a-1]*exp(-(M[y-1,a-1]+F[y-1,a-1]))
      B[y,a]<-N[y,a] * waa[a]
    }
  SSB[y]<-sum(B[y,]*maa*0.5)
}

par(mfrow=c(2,1))
plot(SSB~yrs, type="l", xlab="Year", ylab="Spawning Stock Biomass", las=1, lwd=2)
plot(rowSums(N)~yrs, type="l", xlab="Year", ylab="Total Abundance", las=1, lwd=2)


par(mfrow=c(1,1))
sim_ssb<-seq(from=0, to=max(SSB), by=1)

plot(R~SSB, xlab="Spawning Stock Biomass", ylab="Recruitment (numbers)", las=1, lwd=2, pch=19, xlim=c(0,max(SSB)+100),
     ylim=c(0, max(R[2:nyears])+100))
lines(sim_ssb, (alpha*sim_ssb)/(1+exp(beta)*sim_ssb))

library(ggplot2)
M_df<-data.frame(M=c(M),year=rep(yrs, 15), age=(rep(seq(from=1, to=15), each=51)))

ggplot(data=M_df, aes(x=year, y=M))+xlab("Year")+ylab("M")+
  geom_line()+
  geom_hline(yintercept=0.2, colour="red", linetype="dashed")+
  facet_wrap(~age, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))


N_df<-data.frame(N=c(N),year=rep(yrs, 15), age=(rep(seq(from=1, to=15), each=51)))

ggplot(data=N_df, aes(x=year, y=N))+xlab("Year")+ylab("Abundance at Age")+
  geom_line()+
  facet_wrap(~age, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

cohort_df<-N_df

cohort_df$cohort<-cohort_df$year-cohort_df$age

sub_cohort_df<-subset(cohort_df, cohort>1995 & cohort<2020)

ggplot(data=sub_cohort_df, aes(x=year, y=N))+
  geom_line()+xlab("Cohort")+ylab("Abundance")+
  facet_wrap(~cohort, scales = "free")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

N_df<-data.frame(N=c(N),year=rep(yrs, 15), age=(rep(seq(from=1, to=15), each=51)))

library(ggridges)

sub_N_df<-subset(N_df, year>2025)

ggplot(data=sub_N_df, aes(x=age, y=year, height=N, group=year, fill=year))+
  geom_ridgeline(stat="identity", scale=0.005)+xlab("Age")+ylab("Year")


