summary(glm(adopted.t2.bin ~ zScore.mar+perceivedCoupling+technicalCoupling,data=data.01,family=binomial))
summary(lm(adopted.t2.frq ~ zScore.mar+perceivedCoupling+technicalCoupling,data=data.01))

summary(r.01.ph2 <- lm(adopted.t2.frq ~ t1+perceivedCoupling+zScore.mar.ph2+log(technicalCoupling+1),data=data.01)) 
summary(r.01.ph3 <- lm(adopted.t3.frq ~ t2+perceivedCoupling+zScore.mar.ph3+log(technicalCoupling+1),data=data.01))
summary(r.01.ph4 <- lm(adopted.t4.frq ~ t3+perceivedCoupling+zScore.mar.ph4+log(technicalCoupling+1),data=data.01))

summary(r.02.ph2 <- lm(adopted.t2.frq ~ t1+zScore.mar*perceivedCoupling+zScore.mar*log(technicalCoupling+1),data=data.01)) 
summary(r.02.ph3 <- lm(adopted.t3.frq ~ t2+zScore.mar*perceivedCoupling+zScore.mar*log(technicalCoupling+1),data=data.01))
summary(r.02.ph4 <- lm(adopted.t4.frq ~ t3+zScore.mar*perceivedCoupling+zScore.mar*log(technicalCoupling+1),data=data.01)) 

summary(r.03.ph3 <- lm(adopted.t3.frq ~ t2+zScore.ph2*perceivedCoupling+zScore.mar*log(technicalCoupling+1),data=data.01))
summary(r.03.ph4 <- lm(adopted.t4.frq ~ t3+zScore.ph3*perceivedCoupling+zScore.mar*log(technicalCoupling+1),data=data.01)) 

summary(r.04.ph2 <- lm(adopted.t2.frq ~ t1+perceivedCoupling+zScore.mar.ph2*log(technicalCoupling+1),data=data.01)) 
summary(r.04.ph3 <- lm(adopted.t3.frq ~ t2+perceivedCoupling+zScore.mar.ph3*log(technicalCoupling+1),data=data.01))
summary(r.04.ph4 <- lm(adopted.t4.frq ~ t3+perceivedCoupling+zScore.mar.ph4*log(technicalCoupling+1),data=data.01))

summary(r.01.ph2 <- glm.nb(adopted.t2.frq ~ t1+zScore.mar+perceivedCoupling+log(technicalCoupling+1),data=data.01)) 
summary(r.01.ph3 <- glm.nb(adopted.t3.frq ~ t2+zScore.mar+perceivedCoupling+log(technicalCoupling+1),data=data.01)) 
data.01$adopted.t4.frq.nonZeor <- ifelse(data.01$adopted.t4.frq<0,NA,data.01$adopted.t4.frq)
summary(r.01.ph4 <- glm.nb(adopted.t4.frq.nonZeor ~ t3+zScore.mar+perceivedCoupling+log(technicalCoupling+1),data=data.01)) 

summary(r.01.ph2 <- glm(adopted.t2.frq ~ t1+perceivedCoupling+zScore.mar+log(technicalCoupling+1),data=data.01,family="poisson")) 
summary(r.01.ph3 <- glm(adopted.t3.frq ~ t2+perceivedCoupling+zScore.mar+log(technicalCoupling+1),data=data.01,family="poisson"))
summary(r.01.ph4 <- glm(adopted.t4.frq ~ t3+perceivedCoupling+zScore.mar+log(technicalCoupling+1),data=data.01,family="poisson")) 

stargazer(r.01.ph2,r.01.ph3,r.01.ph4,type="text")
stargazer(r.02.ph2,r.02.ph3,r.02.ph4,type="text")
stargazer(r.03.ph3,r.03.ph4,type="text")

hist(data.01$technicalCoupling)
stargazer(cor(data.01[,c("t3","perceivedCoupling","technicalCoupling")]),type="text",title="Correlations") ## in paper.
vif(r.01.ph2)
vif(r.01.ph3)
vif(r.01.ph4)

vif(r.02.ph2)
vif(r.02.ph3)
vif(r.02.ph4)

AIC(lm(adopted.t2.frq ~ t1,data=data.01))
AIC(lm(adopted.t2.frq ~ t1+zScore.mar,data=data.01))
AIC(lm(adopted.t2.frq ~ t1+zScore.mar+perceivedCoupling,data=data.01))
AIC(lm(adopted.t2.frq ~ t1+zScore.mar+perceivedCoupling+log(technicalCoupling+1),data=data.01))
AIC(lm(adopted.t3.frq ~ t1,data=data.01))
AIC(lm(adopted.t3.frq ~ t1+zScore.mar,data=data.01))
AIC(lm(adopted.t3.frq ~ t1+zScore.mar+perceivedCoupling,data=data.01))
AIC(lm(adopted.t3.frq ~ t1+zScore.mar+perceivedCoupling+log(technicalCoupling+1),data=data.01))
AIC(lm(adopted.t4.frq ~ t1,data=data.01))
AIC(lm(adopted.t4.frq ~ t1+zScore.mar,data=data.01))
AIC(lm(adopted.t4.frq ~ t1+zScore.mar+perceivedCoupling,data=data.01))
AIC(lm(adopted.t4.frq ~ t1+zScore.mar+perceivedCoupling+log(technicalCoupling+1),data=data.01))
BIC(lm(adopted.t2.frq ~ t1,data=data.01))
BIC(lm(adopted.t2.frq ~ t1+zScore.mar,data=data.01))
BIC(lm(adopted.t2.frq ~ t1+zScore.mar+perceivedCoupling,data=data.01))
BIC(lm(adopted.t2.frq ~ t1+zScore.mar+perceivedCoupling+log(technicalCoupling+1),data=data.01))
BIC(lm(adopted.t3.frq ~ t1,data=data.01))
BIC(lm(adopted.t3.frq ~ t1+zScore.mar,data=data.01))
BIC(lm(adopted.t3.frq ~ t1+zScore.mar+perceivedCoupling,data=data.01))
BIC(lm(adopted.t3.frq ~ t1+zScore.mar+perceivedCoupling+log(technicalCoupling+1),data=data.01))
BIC(lm(adopted.t4.frq ~ t1,data=data.01))
BIC(lm(adopted.t4.frq ~ t1+zScore.mar,data=data.01))
BIC(lm(adopted.t4.frq ~ t1+zScore.mar+perceivedCoupling,data=data.01))
BIC(lm(adopted.t4.frq ~ t1+zScore.mar+perceivedCoupling+log(technicalCoupling+1),data=data.01))

plot(r.01.ph2)
lmtest::bptest(r.01.ph2)

summary(sandwich::vcovHC(r.01.ph2))
r.01.ph2.White <- coeftest(r.01.ph2, vcov.=vcovHC)
r.01.ph3.White <- coeftest(r.01.ph3, vcov.=vcovHC)
r.01.ph4.White <- coeftest(r.01.ph4, vcov.=vcovHC)
stargazer(r.01.ph2.White,r.01.ph3.White,r.01.ph4.White,type="html",out="reg001.html") ### in paper.

r.04.ph2.White <- coeftest(r.04.ph2, vcov.=vcovHC)
r.04.ph3.White <- coeftest(r.04.ph3, vcov.=vcovHC)
r.04.ph4.White <- coeftest(r.04.ph4, vcov.=vcovHC)
stargazer(r.04.ph2.White,r.04.ph3.White,r.04.ph4.White,type="html",out="reg002.html") ### in paper. (interactions).

## zScore to DV:
summary(r.01.ph2 <- glm(adopted.t2.negativeZs ~ t1+perceivedCoupling+log(technicalCoupling+1),data=data.01,family="poisson")) 
summary(r.01.ph3 <- glm(adopted.t3.negativeZs ~ t2+perceivedCoupling+log(technicalCoupling+1),data=data.01,family="poisson"))
summary(r.01.ph4 <- glm(adopted.t4.negativeZs ~ t3+perceivedCoupling+log(technicalCoupling+1),data=data.01,family="poisson")) 

summary(r.01.ph2 <- glm(adopted.t2.negativeZs ~ t1+perceivedCoupling+log(technicalCoupling+1),data=data.01))
summary(r.01.ph3 <- glm(adopted.t3.negativeZs ~ t2+perceivedCoupling+log(technicalCoupling+1),data=data.01))
summary(r.01.ph4 <- glm(adopted.t4.negativeZs ~ t3+perceivedCoupling+log(technicalCoupling+1),data=data.01))

summary(r.01.ph2 <- glm(adopted.t2.positiveZs ~ t1+perceivedCoupling+log(technicalCoupling+1),data=data.01))
summary(r.01.ph3 <- glm(adopted.t3.positiveZs ~ t2+perceivedCoupling+log(technicalCoupling+1),data=data.01))
summary(r.01.ph4 <- glm(adopted.t4.positiveZs ~ t3+perceivedCoupling+log(technicalCoupling+1),data=data.01))



