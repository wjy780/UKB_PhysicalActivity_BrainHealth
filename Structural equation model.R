library(lavaan)
library(semPlot)
data = data1

# PAEE
HS.model<-"
PAEE=~1*PA1
BLOOD=~1*bl1+bl6+bio4+bio8
Meta=~1*me1  +me3 +me5+me4
status=~1*status1+status2+status3+status4+status5

status~PAEE+BLOOD+Meta+cov1+cov2
BLOOD~PAEE+cov1+cov2
Meta~PAEE+cov1+cov2
BLOOD~~Meta
"
fit<-sem(HS.model,data=data,ordered=c('status1','status2','status3','status4','status5'))
fitmeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))
sum = summary(fit,standardized=T)

# MVPA_energy
HS.model<-"
PAEE=~1*PA2
BLOOD=~1*bl1+bl6+bio4+bio8
Meta=~1*me1  +me3 +me5+me4
status=~1*status1+status2+status3+status4+status5
status~PAEE+BLOOD+Meta+cov1+cov2

BLOOD~PAEE+cov1+cov2
Meta~PAEE+cov1+cov2
BLOOD~~Meta
"
fit<-sem(HS.model,data=data,ordered=c('status1','status2','status3','status4','status5'))
fitmeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))
sum = summary(fit,standardized=T)

# LPA
HS.model<-"
PAEE=~1*PA3
BLOOD=~1*bl1+bl6+bio4+bio8
Meta=~1*me1  +me3 +me5+me4
status=~1*status1+status2+status3+status4+status5

status~PAEE+BLOOD+Meta+cov1+cov2
BLOOD~PAEE+cov1+cov2
Meta~PAEE+cov1+cov2
BLOOD~~Meta
"
fit<-sem(HS.model,data=data,ordered=c('status1','status2','status3','status4','status5'))
fitmeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))
sum = summary(fit,standardized=T)

# MVPA_time
HS.model<-"
PAEE=~1*PA4
BLOOD=~1*bl1+bl6+bio4+bio8
Meta=~1*me1  +me3 +me5+me4
status=~1*status1+status2+status3+status4+status5

status~PAEE+BLOOD+Meta+cov1+cov2
BLOOD~PAEE+cov1+cov2
Meta~PAEE+cov1+cov2
BLOOD~~Meta
"
fit<-sem(HS.model,data=data,ordered=c('status1','status2','status3','status4','status5'))
fitmeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))
sum = summary(fit,standardized=T)

# Sedentary
HS.model<-"
PAEE=~1*PA5
BLOOD=~1*bl1+bl6+bio4+bio8
Meta=~1*me1  +me3 +me5+me4
status=~1*status1+status2+status3+status4+status5

status~PAEE+BLOOD+Meta+cov1+cov2
BLOOD~PAEE+cov1+cov2
Meta~PAEE+cov1+cov2
BLOOD~~Meta
"
fit<-sem(HS.model,data=data,ordered=c('status1','status2','status3','status4','status5'))
fitmeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))
sum = summary(fit,standardized=T)
modindices(fit, sort = TRUE)

