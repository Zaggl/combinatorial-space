summary(r.01.ph2 <- lm(adopted.t2.frq ~ t1+perceivedCoupling+m.rareness.mar+zScore.ph2+log(technicalCoupling+1),data=data.01)) 
summary(r.01.ph3 <- lm(adopted.t3.frq ~ t2+perceivedCoupling+m.rareness.mar+zScore.ph3+log(technicalCoupling+1),data=data.01))
summary(r.01.ph4 <- lm(adopted.t4.frq ~ t3+perceivedCoupling+m.rareness.mar+zScore.ph4+log(technicalCoupling+1),data=data.01))

summary(r.04.ph2 <- lm(adopted.t2.frq ~ t1+perceivedCoupling+m.rareness.mar*zScore.ph2*log(technicalCoupling+1),data=data.01)) 
summary(r.04.ph3 <- lm(adopted.t3.frq ~ t2+perceivedCoupling+m.rareness.mar*zScore.ph3*log(technicalCoupling+1),data=data.01))
summary(r.04.ph4 <- lm(adopted.t4.frq ~ t3+perceivedCoupling+m.rareness.mar*zScore.ph4*log(technicalCoupling+1),data=data.01))

hist(data.01$technicalCoupling)
stargazer(cor(data.01[,c("t3","perceivedCoupling","technicalCoupling")]),type="text",title="Correlations") ## in paper.
vif(r.01.ph2)
vif(r.01.ph3)
vif(r.01.ph4)
vif(r.04.ph2)
vif(r.04.ph3)
vif(r.04.ph4)
plot(r.01.ph2)
lmtest::bptest(r.01.ph2)

summary(sandwich::vcovHC(r.01.ph2))
r.01.ph2.White <- coeftest(r.01.ph2, vcov.=vcovHC)
r.01.ph3.White <- coeftest(r.01.ph3, vcov.=vcovHC)
r.01.ph4.White <- coeftest(r.01.ph4, vcov.=vcovHC)
stargazer(r.01.ph2.White,r.01.ph3.White,r.01.ph4.White,type="html",out="reg011.html") ### in paper.

r.04.ph2.White <- coeftest(r.04.ph2, vcov.=vcovHC)
r.04.ph3.White <- coeftest(r.04.ph3, vcov.=vcovHC)
r.04.ph4.White <- coeftest(r.04.ph4, vcov.=vcovHC)
stargazer(r.04.ph2.White,r.04.ph3.White,r.04.ph4.White,type="html",out="reg012.html") ### in paper. (interactions).

