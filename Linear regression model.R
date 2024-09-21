#### Linear regression model####
data1 <- Exposure_Outcome
model <-lm(Outcome ~ Exposure  + cov1 + cov2 + cov3 + cov4 + cov5 + cov6 +cov7 + cov8 +cov9,data=data1)
coe <- summary(model)$coefficient
est[num] <-coe['paee','Estimate']
pr[num] <- coe['paee','Pr(>|t|)']