#### Cox proportional hazard model ####
library("survival")
library("survminer")
# Dementia
data=data1
basurv <- Surv(time =data$Dementia_days,event =data$Dementia_status)
data$basurv <-with(data,basurv)
UniCox <- function(x){
  FML <- as.formula(paste0('basurv~',paste(x,'cov1','cov2','cov3','cov4',sep = "+")))
  cox<-coxph(FML, data = data)
  zph0 <- cox.zph(cox)
  a <- nrow(zph0[["table"]])
  zphP_global <- round(zph0[["table"]][a,3],2)
  sum<-summary(cox)
  HR <- round(sum$coefficients[1,2],2)
  CI <- paste0(round(sum$conf.int[1,3:4],2),collapse = "-")
  PValue <- sum$coefficients[1,5]
  Unicox <- data.frame ('exposure' = x,'zphP_global' = zphP_global,
                        'HR(95CI)' = paste0(HR,'(',CI,')'),'raw PValue' = PValue)
  return(Unicox)
}
UniCox('paee')
VarNames <- colnames(data)[c(4:8)]
UniVar <- lapply(VarNames,UniCox)
UniVar <- ldply(UniVar,data.frame)
dementia <- UniVar

# Stroke 
data=data1
basurv <- Surv(time = data$Stroke_days,event = data$Stroke_status)
data$basurv <-with(data,basurv)
UniCox <- function(x){
  FML <- as.formula(paste0('basurv~',paste(x,'cov1','cov2','cov3','cov4','cov5','cov6',sep = "+")))
  cox<-coxph(FML, data = data)
  zph0 <- cox.zph(cox) 
  a <- nrow(zph0[["table"]])
  zphP_global <- round(zph0[["table"]][a,3],2)
  sum<-summary(cox)
  HR <- round(sum$coefficients[1,2],2)
  CI <- paste0(round(sum$conf.int[1,3:4],2),collapse = "-")
  PValue <- sum$coefficients[1,5]
  Unicox <- data.frame ('exposure' = x,'zphP_global' = zphP_global,
                        'HR(95CI)' = paste0(HR,'(',CI,')'),'raw PValue' = PValue)
  return(Unicox)
}
UniCox('paee')
VarNames <- colnames(data)[c(4:8)]
UniVar <- lapply(VarNames,UniCox)
UniVar <- ldply(UniVar,data.frame)
stroke <- UniVar

# Depression
data = data1
basurv <- Surv(time = data$MDD_days,event = data$MDD_status)
data$basurv <-with(data,basurv)
UniCox <- function(x){
  FML <- as.formula(paste0('basurv~',paste(x,'cov1','cov2','cov3','cov4',sep = "+")))
  cox<-coxph(FML, data = data)
  zph0 <- cox.zph(cox)  #ph检验
  a <- nrow(zph0[["table"]])
  zphP_global <- round(zph0[["table"]][a,3],2)
  sum<-summary(cox)
  HR <- round(sum$coefficients[1,2],2)
  CI <- paste0(round(sum$conf.int[1,3:4],2),collapse = "-")
  PValue <- sum$coefficients[1,5]
  Unicox <- data.frame ('exposure' = x,'zphP_global' = zphP_global,
                        'HR(95CI)' = paste0(HR,'(',CI,')'),'raw PValue' = PValue)
  return(Unicox)
}
UniCox('paee')
VarNames <- colnames(data)[c(4:8)]
UniVar <- lapply(VarNames,UniCox)
UniVar <- ldply(UniVar,data.frame)
depression <- UniVar


# Anxiety
data = data1
basurv <- Surv(time = data$Anxiety_days,event = data$Anxiety_status)
data$basurv <-with(data,basurv)
UniCox <- function(x){
  FML <- as.formula(paste0('basurv~',paste(x,'cov1','cov2','cov3','cov4',sep = "+")))
  cox<-coxph(FML, data = data)
  zph0 <- cox.zph(cox)
  a <- nrow(zph0[["table"]])
  zphP_global <- round(zph0[["table"]][a,3],2)
  sum<-summary(cox)
  HR <- round(sum$coefficients[1,2],2)
  CI <- paste0(round(sum$conf.int[1,3:4],2),collapse = "-")
  PValue <- sum$coefficients[1,5]
  Unicox <- data.frame ('exposure' = x,'zphP_global' = zphP_global,
                        'HR(95CI)' = paste0(HR,'(',CI,')'),'raw PValue' = PValue)
  return(Unicox)
}
UniCox('paee')
VarNames <- colnames(data)[c(4:8)]
UniVar <- lapply(VarNames,UniCox)
UniVar <- ldply(UniVar,data.frame)
anxiety <- UniVar


# Sleep
data = data1
basurv <- Surv(time = data$Sleep_days,event = data$Sleep_status)
data$basurv <-with(data,basurv)
UniCox <- function(x){
  FML <- as.formula(paste0('basurv~',paste(x,'cov1','cov2','cov3','cov4',sep = "+")))
  cox<-coxph(FML, data = data)
  zph0 <- cox.zph(cox)
  a <- nrow(zph0[["table"]])
  zphP_global <- round(zph0[["table"]][a,3],2)
  sum<-summary(cox)
  HR <- round(sum$coefficients[1,2],2)
  CI <- paste0(round(sum$conf.int[1,3:4],2),collapse = "-")
  PValue <- sum$coefficients[1,5]
  Unicox <- data.frame ('exposure' = x,'zphP_global' = zphP_global,
                        'HR(95CI)' = paste0(HR,'(',CI,')'),'raw PValue' = PValue)
  return(Unicox)
}
UniCox('paee')
VarNames <- colnames(data)[c(4:8)]
UniVar <- lapply(VarNames,UniCox)
UniVar <- ldply(UniVar,data.frame)
sleep <- UniVar

#### Restricted cubic spline #### 
library(rms)
library(survival)
library(survminer)
# Dementia/Stroke/PD/Anxiety/Depression/Sleep disorders
S<- Surv(time = data$Dementia_days,event = data$Dementia_status)
fit<- cph(S ~ rcs(paee,4) + cov1 + cov2 + cov3 +cov4 ,data=data) 
fit <- update(fit)
anova(fit)
HR <- Predict(fit,paee,fun=exp,ref.zero = TRUE)