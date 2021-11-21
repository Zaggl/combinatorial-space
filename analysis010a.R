##### CREATE NOVELTY/ATYPICALTY MESASURE (Z-SCORE): ######
m.z.ph1 <- getZscore(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph1"]),repetitions=200)
m.z.ph2 <- getZscore(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph2"]),repetitions=800)
m.z.ph3 <- getZscore(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph3"]),repetitions=300)
m.z.ph4 <- getZscore(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph4"]),repetitions=800)
m.z.mar.ph1 <- getZscore.market2(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph1"]),repetitions=500,v.market=d.compos_agents$freq)
m.z.mar.ph2 <- getZscore.market2(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph2"]),repetitions=500,v.market=d.compos_agents$freq)
m.z.mar.ph3 <- getZscore.market2(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph3"]),repetitions=500,v.market=d.compos_agents$freq)
m.z.mar.ph4 <- getZscore.market2(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph4"]),repetitions=500,v.market=d.compos_agents$freq)
m.rareness.mar <- getRareness.market(d.compos_agents=d.compos_agents) 
#m.z.ph3.tminus1 <- getZscore.tminus1(m.compos_agents.obs=as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph3"]),m.compos_agents.obs.tminus1=as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph2"]),repetitions=800)
#m.z.ph4.tminus1 <- getZscore.tminus1(m.compos_agents.obs=as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph4"]),m.compos_agents.obs.tminus1=as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph3"]),repetitions=800)

##### Optional: Calcualte and zScores Distributions of apps:
d.agents <- addzScore(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph1"]),m.z.ph1,d.agents,"ph1")
d.agents <- addzScore(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph2"]),m.z.ph2,d.agents,"ph2")
d.agents <- addzScore(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph3"]),m.z.ph3,d.agents,"ph3")
d.agents <- addzScore(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph4"]),m.z.ph4,d.agents,"ph4")

##### CREATE COMPONENT ATTRIBUTES/IVs: ######
v.comps.perf.Tech.ph1 <- getPerceivedPerformance.compos(d.compos_agents=d.compos_agents,kind="Tech",phase=1,d.agents=d.agents)
v.comps.diff.Tech.ph1 <- getPerceivedUncertainty.compos(d.compos_agents=d.compos_agents,kind="Tech",phase=1,d.agents=d.agents)
v.comps.perf.Tech.ph2 <- getPerceivedPerformance.compos(d.compos_agents=d.compos_agents,kind="Tech",phase=2,d.agents=d.agents)
v.comps.diff.Tech.ph2 <- getPerceivedUncertainty.compos(d.compos_agents=d.compos_agents,kind="Tech",phase=2,d.agents=d.agents)
v.comps.perf.Tech.ph3 <- getPerceivedPerformance.compos(d.compos_agents=d.compos_agents,kind="Tech",phase=3,d.agents=d.agents)
v.comps.diff.Tech.ph3 <- getPerceivedUncertainty.compos(d.compos_agents=d.compos_agents,kind="Tech",phase=3,d.agents=d.agents)
v.comps.perf.Tech.ph4 <- getPerceivedPerformance.compos(d.compos_agents=d.compos_agents,kind="Tech",phase=4,d.agents=d.agents)
v.comps.diff.Tech.ph4 <- getPerceivedUncertainty.compos(d.compos_agents=d.compos_agents,kind="Tech",phase=4,d.agents=d.agents)
m.pairs.perf.Tech.ph1 <- getPerceivedPerformance.pairs(v.perf=v.comps.perf.Tech.ph1)
m.pairs.diff.Tech.ph1 <- getPerceivedUncertainty.pairs(v.perf=v.comps.perf.Tech.ph1)
m.pairs.perf.Tech.ph2 <- getPerceivedPerformance.pairs(v.perf=v.comps.perf.Tech.ph2)
m.pairs.diff.Tech.ph2 <- getPerceivedUncertainty.pairs(v.perf=v.comps.perf.Tech.ph2)
m.pairs.perf.Tech.ph3 <- getPerceivedPerformance.pairs(v.perf=v.comps.perf.Tech.ph3)
m.pairs.diff.Tech.ph3 <- getPerceivedUncertainty.pairs(v.perf=v.comps.perf.Tech.ph3)
m.pairs.perf.Tech.ph4 <- getPerceivedPerformance.pairs(v.perf=v.comps.perf.Tech.ph4)
m.pairs.diff.Tech.ph4 <- getPerceivedUncertainty.pairs(v.perf=v.comps.perf.Tech.ph4)

v.comps.perf.UX.ph1 <- getPerceivedPerformance.compos(d.compos_agents=d.compos_agents,kind="UX",phase=1,d.agents=d.agents)
v.comps.diff.UX.ph1 <- getPerceivedUncertainty.compos(d.compos_agents=d.compos_agents,kind="UX",phase=1,d.agents=d.agents)
v.comps.perf.UX.ph2 <- getPerceivedPerformance.compos(d.compos_agents=d.compos_agents,kind="UX",phase=2,d.agents=d.agents)
v.comps.diff.UX.ph2 <- getPerceivedUncertainty.compos(d.compos_agents=d.compos_agents,kind="UX",phase=2,d.agents=d.agents)
v.comps.perf.UX.ph3 <- getPerceivedPerformance.compos(d.compos_agents=d.compos_agents,kind="UX",phase=3,d.agents=d.agents)
v.comps.diff.UX.ph3 <- getPerceivedUncertainty.compos(d.compos_agents=d.compos_agents,kind="UX",phase=3,d.agents=d.agents)
v.comps.perf.UX.ph4 <- getPerceivedPerformance.compos(d.compos_agents=d.compos_agents,kind="UX",phase=4,d.agents=d.agents)
v.comps.diff.UX.ph4 <- getPerceivedUncertainty.compos(d.compos_agents=d.compos_agents,kind="UX",phase=4,d.agents=d.agents)
m.pairs.perf.UX.ph1 <- getPerceivedPerformance.pairs(v.perf=v.comps.perf.UX.ph1)
m.pairs.diff.UX.ph1 <- getPerceivedUncertainty.pairs(v.perf=v.comps.perf.UX.ph1)
m.pairs.perf.UX.ph2 <- getPerceivedPerformance.pairs(v.perf=v.comps.perf.UX.ph2)
m.pairs.diff.UX.ph2 <- getPerceivedUncertainty.pairs(v.perf=v.comps.perf.UX.ph2)
m.pairs.perf.UX.ph3 <- getPerceivedPerformance.pairs(v.perf=v.comps.perf.UX.ph3)
m.pairs.diff.UX.ph3 <- getPerceivedUncertainty.pairs(v.perf=v.comps.perf.UX.ph3)
m.pairs.perf.UX.ph4 <- getPerceivedPerformance.pairs(v.perf=v.comps.perf.UX.ph4)
m.pairs.diff.UX.ph4 <- getPerceivedUncertainty.pairs(v.perf=v.comps.perf.UX.ph4)

v.comps.perf.Tota.ph1 <- getPerceivedPerformance.compos(d.compos_agents=d.compos_agents,kind="Total",phase=1,d.agents=d.agents)
v.comps.diff.Tota.ph1 <- getPerceivedUncertainty.compos(d.compos_agents=d.compos_agents,kind="Total",phase=1,d.agents=d.agents)
v.comps.perf.Tota.ph2 <- getPerceivedPerformance.compos(d.compos_agents=d.compos_agents,kind="Total",phase=2,d.agents=d.agents)
v.comps.diff.Tota.ph2 <- getPerceivedUncertainty.compos(d.compos_agents=d.compos_agents,kind="Total",phase=2,d.agents=d.agents)
v.comps.perf.Tota.ph3 <- getPerceivedPerformance.compos(d.compos_agents=d.compos_agents,kind="Total",phase=3,d.agents=d.agents)
v.comps.diff.Tota.ph3 <- getPerceivedUncertainty.compos(d.compos_agents=d.compos_agents,kind="Total",phase=3,d.agents=d.agents)
v.comps.perf.Tota.ph4 <- getPerceivedPerformance.compos(d.compos_agents=d.compos_agents,kind="Total",phase=4,d.agents=d.agents)
v.comps.diff.Tota.ph4 <- getPerceivedUncertainty.compos(d.compos_agents=d.compos_agents,kind="Total",phase=4,d.agents=d.agents)
m.pairs.perf.Tota.ph1 <- getPerceivedPerformance.pairs(v.perf=v.comps.perf.Tota.ph1)
m.pairs.diff.Tota.ph1 <- getPerceivedUncertainty.pairs(v.perf=v.comps.perf.Tota.ph1)
m.pairs.perf.Tota.ph2 <- getPerceivedPerformance.pairs(v.perf=v.comps.perf.Tota.ph2)
m.pairs.diff.Tota.ph2 <- getPerceivedUncertainty.pairs(v.perf=v.comps.perf.Tota.ph2)
m.pairs.perf.Tota.ph3 <- getPerceivedPerformance.pairs(v.perf=v.comps.perf.Tota.ph3)
m.pairs.diff.Tota.ph3 <- getPerceivedUncertainty.pairs(v.perf=v.comps.perf.Tota.ph3)
m.pairs.perf.Tota.ph4 <- getPerceivedPerformance.pairs(v.perf=v.comps.perf.Tota.ph4)
m.pairs.diff.Tota.ph4 <- getPerceivedUncertainty.pairs(v.perf=v.comps.perf.Tota.ph4)

##### CREATE PRESENCE MATRICES: ######
m.compos.ph1.frq <- as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph1"])%*%t(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph1"]))
diag(m.compos.ph1.frq) <- 0
m.compos.ph1.bin <- m.compos.ph1.frq
m.compos.ph1.bin[m.compos.ph1.bin>0] <- 1
m.compos.ph2.frq <- as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph2"])%*%t(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph2"]))
diag(m.compos.ph2.frq) <- 0
m.compos.ph2.bin <- m.compos.ph2.frq
m.compos.ph2.bin[m.compos.ph2.bin>0] <- 1
m.compos.ph3.frq <- as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph3"])%*%t(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph3"]))
diag(m.compos.ph3.frq) <- 0
m.compos.ph3.bin <- m.compos.ph3.frq
m.compos.ph3.bin[m.compos.ph3.bin>0] <- 1
m.compos.ph4.frq <- as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph4"])%*%t(as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%".ph4"]))
diag(m.compos.ph4.frq) <- 0
m.compos.ph4.bin <- m.compos.ph4.frq
m.compos.ph4.bin[m.compos.ph4.bin>0] <- 1

m.compos.diff1to2.frq <- m.compos.ph2.frq - m.compos.ph1.frq
m.compos.diff2to3.frq <- m.compos.ph3.frq - m.compos.ph2.frq
m.compos.diff3to4.frq <- m.compos.ph4.frq - m.compos.ph3.frq
m.compos.diff1to2.bin <- m.compos.ph2.bin - m.compos.ph1.bin
m.compos.diff2to3.bin <- m.compos.ph3.bin - m.compos.ph2.bin
m.compos.diff3to4.bin <- m.compos.ph4.bin - m.compos.ph3.bin

#sum(m.compos.diff1to2.bin==1)
#sum(m.compos.diff1to2.bin==-1)

m.perceivedCoupling <- createPerceivedCouplingMatrix(d.compos_agents)
m.technicalCoupling <- t(createTechnicalCouplingMatrix(d.compos.dependencies,d.compos_agents))

##### CREATE JOINT DATAFRAME: ######
data.01 <- data.frame(t1=integer(),t2=integer(),t3=integer(),t4=integer()
                   ,adopted.t2.bin=integer()
                   ,adopted.t3.bin=integer()
                   ,adopted.t4.bin=integer()
                   ,adopted.t2.frq=integer()
                   ,adopted.t3.frq=integer()
                   ,adopted.t4.frq=integer()
                   ,adopted.t2.negativeZs=integer()
                   ,adopted.t2.positiveZs=integer()
                   ,adopted.t3.negativeZs=integer()
                   ,adopted.t3.positiveZs=integer()
                   ,adopted.t4.negativeZs=integer()
                   ,adopted.t4.positiveZs=integer()
                   ,zScore.ph2=double()
                   ,zScore.ph3=double()
                   ,zScore.ph4=double()
                   ,zScore.mar.ph1=double()
                   ,zScore.mar.ph2=double()
                   ,zScore.mar.ph3=double()
                   ,zScore.mar.ph4=double()
                   ,m.rareness.mar=double()
                   ,perceivedCoupling=integer()
                   ,technicalCoupling=integer()
                   ,perceivedPerformance.Tech.t2=double()
                   ,perceivedUncertainty.Tech.t2=double()
                   ,perceivedPerformance.UX.t2=double()
                   ,perceivedUncertainty.UX.t2=double()
                   ,perceivedPerformance.Tota.t2=double()
                   ,perceivedUncertainty.Tota.t2=double()
                   ,perceivedPerformance.Tech.t2.comp1=double()
                   ,perceivedPerformance.Tech.t2.comp2=double()
                   ,perceivedUncertainty.Tech.t2.comp1=double()
                   ,perceivedUncertainty.Tech.t2.comp2=double()
                   ,perceivedPerformance.UX.t2.comp1=double()
                   ,perceivedPerformance.UX.t2.comp2=double()
                   ,perceivedUncertainty.UX.t2.comp1=double()
                   ,perceivedUncertainty.UX.t2.comp2=double()
                   ,perceivedPerformance.Tota.t2.comp1=double()
                   ,perceivedPerformance.Tota.t2.comp2=double()
                   ,perceivedUncertainty.Tota.t2.comp1=double()
                   ,perceivedUncertainty.Tota.t2.comp2=double()
                   ,comp1=character(),comp2=character(),stringsAsFactors=FALSE)
for(i in 1:nrow(d.compos_agents)) {
  for(j in 1:nrow(d.compos_agents)) {
    if(i<j&rownames(m.compos.ph1.frq)[i]%in%rownames(m.z.ph2)&rownames(m.compos.ph2.frq)[j]%in%colnames(m.z.ph2)) {
      data.01[nrow(data.01)+1,] <- list(m.compos.ph1.frq[i,j],m.compos.ph2.frq[i,j],m.compos.ph3.frq[i,j],m.compos.ph4.frq[i,j]
                                       ,m.compos.diff1to2.bin[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.compos.diff2to3.bin[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.compos.diff3to4.bin[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.compos.diff1to2.frq[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.compos.diff2to3.frq[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.compos.diff3to4.frq[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,ifelse(m.compos.diff1to2.bin[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]*m.z.mar.ph2[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]<0,1,0)
                                       ,ifelse(m.compos.diff1to2.bin[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]*m.z.mar.ph2[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]>0,1,0)
                                       ,ifelse(m.compos.diff2to3.bin[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]*m.z.mar.ph3[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]<0,1,0)
                                       ,ifelse(m.compos.diff2to3.bin[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]*m.z.mar.ph3[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]>0,1,0)
                                       ,ifelse(m.compos.diff3to4.bin[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]*m.z.mar.ph4[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]<0,1,0)
                                       ,ifelse(m.compos.diff3to4.bin[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]*m.z.mar.ph4[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]>0,1,0)
                                       ,m.z.ph2[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.z.ph3[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.z.ph4[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.z.mar.ph1[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.z.mar.ph2[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.z.mar.ph3[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.z.mar.ph4[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.rareness.mar[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.perceivedCoupling[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.technicalCoupling[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.pairs.perf.Tech.ph2[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.pairs.diff.Tech.ph2[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.pairs.perf.UX.ph2[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.pairs.diff.UX.ph2[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.pairs.perf.Tota.ph2[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,m.pairs.diff.Tota.ph2[rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]]
                                       ,v.comps.perf.Tech.ph2[rownames(m.compos.ph2.frq)[i]]
                                       ,v.comps.perf.Tech.ph2[rownames(m.compos.ph2.frq)[j]]
                                       ,v.comps.diff.Tech.ph2[rownames(m.compos.ph2.frq)[i]]
                                       ,v.comps.diff.Tech.ph2[rownames(m.compos.ph2.frq)[j]]
                                       ,v.comps.perf.UX.ph2[rownames(m.compos.ph2.frq)[i]]
                                       ,v.comps.perf.UX.ph2[rownames(m.compos.ph2.frq)[j]]
                                       ,v.comps.diff.UX.ph2[rownames(m.compos.ph2.frq)[i]]
                                       ,v.comps.diff.UX.ph2[rownames(m.compos.ph2.frq)[j]]
                                       ,v.comps.perf.Tota.ph2[rownames(m.compos.ph2.frq)[i]]
                                       ,v.comps.perf.Tota.ph2[rownames(m.compos.ph2.frq)[j]]
                                       ,v.comps.diff.Tota.ph2[rownames(m.compos.ph2.frq)[i]]
                                       ,v.comps.diff.Tota.ph2[rownames(m.compos.ph2.frq)[j]]
                                       ,rownames(m.compos.ph2.frq)[i],rownames(m.compos.ph2.frq)[j]
                                       )

    }
  }
} ; rm(i,j)
str(data.01)

##### ANALYSES #####
summary(glm(t3~t2+perceivedPerformance.Tech.t2+perceivedUncertainty.Tech.t2,data=data.01,family=binomial))
summary(glm(adopted.t3~perceivedPerformance.Tech.t2+perceivedUncertainty.Tech.t2,data=data.01,family=binomial))

summary(glm(adopted.t3~perceivedPerformance.Tech.t2+perceivedUncertainty.Tech.t2+perceivedPerformance.UX.t2+perceivedUncertainty.UX.t2,data=data.01,family=binomial))
summary(glm(adopted.t3~perceivedPerformance.UX.t2+perceivedUncertainty.UX.t2,data=data.01,family=binomial))
summary(glm(t3~t2+zScore.t2+perceivedPerformance.UX.t2+perceivedUncertainty.UX.t2,data=data.01,family=binomial))

summary(glm(t3~t2+zScore.t3+perceivedPerformance.Tech.t2+perceivedUncertainty.Tech.t2+perceivedPerformance.UX.t2+perceivedUncertainty.UX.t2,data=data.01,family=binomial))

summary(glm(t3~t2+zScore.t2,data=data.01,family=binomial))

summary(glm(adopted.t3~t2+perceivedPerformance.UX.t2.comp1+perceivedPerformance.UX.t2.comp2+perceivedPerformance.Tech.t2.comp1+perceivedPerformance.Tech.t2.comp2,data=data.01,family=binomial))

summary(lm(adopted.t3~zScore.t4+perceivedPerformance.UX.t2.comp1+perceivedPerformance.UX.t2.comp2+perceivedPerformance.Tech.t2.comp1+perceivedPerformance.Tech.t2.comp2,data=data.01))

summary(glm(adopted.t3.negativeZs~perceivedPerformance.UX.t2.comp1+perceivedPerformance.UX.t2.comp2+perceivedPerformance.Tech.t2.comp1+perceivedPerformance.Tech.t2.comp2,data=data.01,family=binomial))
summary(glm(adopted.t3.positiveZs~perceivedPerformance.UX.t2.comp1+perceivedPerformance.UX.t2.comp2+perceivedPerformance.Tech.t2.comp1+perceivedPerformance.Tech.t2.comp2,data=data.01,family=binomial))

summary(glm(adopted.t3.negativeZs~perceivedPerformance.UX.t2.comp1+perceivedPerformance.UX.t2.comp2+perceivedPerformance.Tota.t2.comp1+perceivedPerformance.Tota.t2.comp2,data=data.01,family=binomial))
summary(glm(adopted.t3.positiveZs~perceivedPerformance.UX.t2.comp1+perceivedPerformance.UX.t2.comp2+perceivedPerformance.Tota.t2.comp1+perceivedPerformance.Tota.t2.comp2,data=data.01,family=binomial))

summary(glm(adopted.t3.negativeZs~perceivedPerformance.UX.t2.comp1+perceivedPerformance.UX.t2.comp2+perceivedPerformance.Tech.t2.comp1+perceivedPerformance.Tech.t2.comp2+perceivedPerformance.Tota.t2.comp1+perceivedPerformance.Tota.t2.comp2,data=data.01,family=binomial))
summary(glm(adopted.t3.positiveZs~perceivedPerformance.UX.t2.comp1+perceivedPerformance.UX.t2.comp2+perceivedPerformance.Tech.t2.comp1+perceivedPerformance.Tech.t2.comp2+perceivedPerformance.Tota.t2.comp1+perceivedPerformance.Tota.t2.comp2,data=data.01,family=binomial))
summary(glm(adopted.t3~perceivedPerformance.UX.t2.comp1+perceivedPerformance.UX.t2.comp2+perceivedPerformance.Tech.t2.comp1+perceivedPerformance.Tech.t2.comp2+perceivedPerformance.Tota.t2.comp1+perceivedPerformance.Tota.t2.comp2,data=data.01,family=binomial))


summary(glm(adopted.t3.positiveZs~perceivedUncertainty.UX.t2.comp1+perceivedUncertainty.UX.t2.comp2+perceivedUncertainty.Tech.t2.comp1+perceivedUncertainty.Tech.t2.comp2+perceivedUncertainty.Tota.t2.comp1+perceivedUncertainty.Tota.t2.comp2            
            ,data=data.01,family=binomial))

summary(glm(adopted.t3.positiveZs~perceivedPerformance.UX.t2.comp1+perceivedPerformance.UX.t2.comp2+perceivedPerformance.Tech.t2.comp1+perceivedPerformance.Tech.t2.comp2+perceivedPerformance.Tota.t2.comp1+perceivedPerformance.Tota.t2.comp2
          +perceivedUncertainty.UX.t2.comp1+perceivedUncertainty.UX.t2.comp2+perceivedUncertainty.Tech.t2.comp1+perceivedUncertainty.Tech.t2.comp2+perceivedUncertainty.Tota.t2.comp1+perceivedUncertainty.Tota.t2.comp2            
            ,data=data.01,family=binomial))


summary(lm(adopted.t2~zScore.mar+perceivedPerformance.UX.t2.comp1+perceivedPerformance.UX.t2.comp2+perceivedPerformance.Tech.t2.comp1+perceivedPerformance.Tech.t2.comp2,data=data.01))
summary(lm(adopted.t3~zScore.mar+perceivedPerformance.UX.t2.comp1+perceivedPerformance.UX.t2.comp2+perceivedPerformance.Tech.t2.comp1+perceivedPerformance.Tech.t2.comp2,data=data.01))
summary(lm(adopted.t4~zScore.mar+perceivedPerformance.UX.t2.comp1+perceivedPerformance.UX.t2.comp2+perceivedPerformance.Tech.t2.comp1+perceivedPerformance.Tech.t2.comp2,data=data.01))

## d3 classifaction:

  
### perceived "coupling" Felin Zhenger. Two are in the same box.  
### binary similarity and distance in lines
### breath and depth 
### REM  

summary(lm(t4~zScore.mar+perceivedCoupling,data=data.01))

summary(lm(t4~zScore.mar,data=data.01))
cor(zScore.mar,perceivedCoupling)

## include the local system variable (). 


