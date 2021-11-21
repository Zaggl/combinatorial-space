simplifyCompAgentMatrix <- function(m.components_agents)  {
  rownames(m.compos_agents) <- m.compos_agents[,"id"]
  m.compos_agents <- m.compos_agents.[,-which(colnames(m.compos_agents)=="id")]
  colnames(m.compos_agents) <- LETTERS
  return(m.components_agents)
}

getZscore <- function(m.compos_agents.obs,repetitions=100,seed=62345) {
  m.compos_agents.obs <- m.compos_agents.obs[rowSums(m.compos_agents.obs)>0,] ## possibly remove this filter if possible
  set.seed(seed)
  m.compos_agents.syn <- permatswap(m=m.compos_agents.obs,fixedmar="both",shuffle="samp",times=repetitions)
  
  l.compos.syn <- list()
  for(i in 1:repetitions) {
    l.compos.syn[[i]] <- m.compos_agents.syn[[3]][[i]] %*% t(m.compos_agents.syn[[3]][[i]])
    diag(l.compos.syn[[i]]) <- 0
  }
  
  m.syn.mean <- apply(simplify2array(l.compos.syn),1:2,mean)
  m.syn.sdev <- apply(simplify2array(l.compos.syn),1:2,sd)
  ##m.syn.sdev[m.syn.sdev==0] <- 0.00001 ## to avoid Inf and -Inf, needed if not used (isolated) components are included.
  
  m.agents.obs <- m.compos_agents.obs %*% t(m.compos_agents.obs)
  diag(m.agents.obs) <- 0
  
  m.z <- (m.agents.obs-m.syn.mean)/m.syn.sdev ## Mukherjee2015
  m.z[is.na(m.z)] <- 0
  return(m.z)
}

##getZscore.tminus1 <- function(m.compos_agents.obs,m.compos_agents.obs.tminus1,repetitions=100,seed=62345) {
##  ##m.compos_agents.obs.tminus1 <- m.compos_agents.obs.tminus1[rowSums(m.compos_agents.obs.tminus1)>0,] ## possibly remove this filter if possible
##  set.seed(seed)
##  m.compos_agents.syn <- permatswap(m=m.compos_agents.obs.tminus1,fixedmar="both",shuffle="samp",times=repetitions)
##  
##  l.compos.syn <- list()
##  for(i in 1:repetitions) {
##    l.compos.syn[[i]] <- m.compos_agents.syn[[3]][[i]] %*% t(m.compos_agents.syn[[3]][[i]])
##    diag(l.compos.syn[[i]]) <- 0
##  }
##  
##  m.syn.mean <- apply(simplify2array(l.compos.syn),1:2,mean)
##  m.syn.sdev <- apply(simplify2array(l.compos.syn),1:2,sd)
##  m.syn.sdev[m.syn.sdev==0] <- 0.0001 ## to avoid Inf and -Inf, needed if not used (isolated) components are included.
##  
##  m.agents.obs <- m.compos_agents.obs %*% t(m.compos_agents.obs)
##  diag(m.agents.obs) <- 0
##  
##  m.z <- (m.agents.obs-m.syn.mean)/m.syn.sdev ## Mukherjee2015
##  m.z[is.na(m.z)] <- 0
##  return(m.z)
##}

##getZscore.market <- function(m.compos_agents.mar) {
##  m.baseline <- matrix(data=0,nrow=nrow(m.compos_agents.mar),ncol=nrow(m.compos_agents.mar),dimnames=list(rownames(m.compos_agents.mar),rownames(m.compos_agents.mar)))
##  for(i in 1:nrow(m.baseline)) {
##    for(j in 1:ncol(m.baseline)) {
##      m.baseline[i,j] <- m.compos_agents.mar[i,"freq"]*m.compos_agents.mar[j,"freq"]
##    }
##  }
##  diag(m.baseline) <- 0
##  m.baseline <- (m.baseline-mean(m.baseline))/sd(m.baseline)
##  diag(m.baseline) <- 0
##  return(m.baseline)
##}

getZscore.market2 <- function(m.compos_agents.obs,repetitions=300,seed=62345,v.market) {
  ##m.compos_agents.obs <- m.compos_agents.obs[rowSums(m.compos_agents.obs)>0,] ## possibly remove this filter if possible
  set.seed(seed)
  
  m.baseline <- matrix(data=0,nrow=length(v.market),ncol=length(v.market),dimnames=list(rownames(v.market),rownames(v.market)))
  while(sum(m.baseline)<sum(v.market)) {
    m.baseline.tmp <- m.baseline
    coord.row <- sample(1:nrow(m.baseline),1)
    coord.col <- sample(1:ncol(m.baseline),1)
    m.baseline.tmp[coord.row,coord.col] <- m.baseline.tmp[coord.row,coord.col] + 1
    if(all(rowSums(m.baseline.tmp)<=v.market)&all(colSums(m.baseline.tmp)<=v.market)) {
      m.baseline <- m.baseline.tmp
    }
  }
  
  m.compos_agents.syn <- permatswap(m=m.baseline,fixedmar="both",shuffle="samp",times=repetitions)
  l.compos.syn <- list()
  for(i in 1:repetitions) {
    l.compos.syn[[i]] <- m.compos_agents.syn[[3]][[i]] %*% t(m.compos_agents.syn[[3]][[i]])
    diag(l.compos.syn[[i]]) <- 0
  }
  
  m.syn.mean <- apply(simplify2array(l.compos.syn),1:2,mean)
  m.syn.sdev <- apply(simplify2array(l.compos.syn),1:2,sd)
  m.syn.sdev[m.syn.sdev==0] <- 0.001 ## to avoid Inf and -Inf, needed if not used (isolated) components are included.
  
  m.agents.obs <- m.compos_agents.obs %*% t(m.compos_agents.obs)
  diag(m.agents.obs) <- 0
  m.z <- (m.agents.obs-m.syn.mean)/m.syn.sdev ## Mukherjee2015
  m.z[is.na(m.z)] <- 0
  return(m.z)
}

getRareness.market <- function(d.compos_agents) {
  v.market <- d.compos_agents$freq
  m.typical <- matrix(data=0,nrow=length(v.market),ncol=length(v.market),dimnames=list(rownames(d.compos_agents),rownames(d.compos_agents)))
  for(i in 1:nrow(m.typical)) {
    for(j in 1:ncol(m.typical)) {
      m.typical[i,j] <- ifelse(v.market[i]<=quantile(v.market,.20)|v.market[j]<=quantile(v.market,.20),0,1)
    }
  }
  return(m.typical)
}

addzScore <- function(m.compos_agents,m.z,d.agents,phase) {
  m.zScores <- matrix(data=0,nrow=nrow(d.agents),ncol=2,dimnames=list(rownames(d.agents),c(paste("perc.10th.",phase,sep=""),paste("perc.50th.",phase,sep=""))))
  for(i in 1:ncol(m.compos_agents)) {
    if(!rownames(d.agents)[i]%in%colnames(m.compos_agents)[i]) { "Mistake. Mismatch of agents." }
    agent.zScoreDistribution <- vector(mode="numeric",length=0)
    for(j in 1:nrow(m.compos_agents)) {
      for(k in 1:nrow(m.compos_agents)) {
        if(j>k&m.compos_agents[j,i]==1&m.compos_agents[k,i]==1) {
          z <- m.z[rownames(m.compos_agents)[j],rownames(m.compos_agents)[k]]
          agent.zScoreDistribution <- append(agent.zScoreDistribution,z)
        }
      }
    }
    m.zScores[i,] <- quantile(agent.zScoreDistribution,c(.10,.50)) 
  }
  m.zScores <- as.data.frame(m.zScores)
  m.zScores$agents <- rownames(m.zScores)
  agents.new <- cbind(d.agents,m.zScores)
  return(agents.new)
}

##### Calcualte IVs: #####
getPerceivedPerformance.compos <- function(d.compos_agents,kind,phase,d.agents) {
  m.compos_agents.org <- as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%paste(".ph",phase,sep="")])
  m.compos_agents.per <- m.compos_agents.org
  for(i in 1:ncol(m.compos_agents.per)) {
    m.compos_agents.per[,i] <- m.compos_agents.per[,i]*eval(parse(text=paste("d.agents$",kind,phase,"[i]",sep="")))
  }
  result <- rowSums(m.compos_agents.per)/rowSums(m.compos_agents.org)
  return(result)
}

getPerceivedUncertainty.compos <- function(d.compos_agents,kind,phase,d.agents) {
  m.compos_agents.org <- as.matrix(d.compos_agents[,colnames(d.compos_agents)%like%paste(".ph",phase,sep="")])
  m.compos_agents.per <- m.compos_agents.org
  for(i in 1:ncol(m.compos_agents.per)) {
    m.compos_agents.per[,i] <- m.compos_agents.per[,i]*eval(parse(text=paste("d.agents$",kind,phase,"[i]",sep="")))
  }
  m.compos_agents.per[m.compos_agents.org!=1] <- NA
  result <- apply(m.compos_agents.per,1,sd,na.rm=TRUE)
  return(result)
}

getPerceivedPerformance.pairs <- function(v.perf) {
  m.pair.perf <- matrix(data=0,nrow=length(v.perf),ncol=length(v.perf),dimnames=list(names(v.perf),names(v.perf)))
  for(i in 1:nrow(m.pair.perf)) {
    for(j in 1:ncol(m.pair.perf)) {
      m.pair.perf[i,j] <- max(v.perf[i],v.perf[j])
    }
  }
  diag(m.pair.perf) <- 0
  return(m.pair.perf)
}

getPerceivedUncertainty.pairs <- function(v.perf) {
  m.pair.diff <- matrix(data=0,nrow=length(v.perf),ncol=length(v.perf),dimnames=list(names(v.perf),names(v.perf)))
  for(i in 1:nrow(m.pair.diff)) {
    for(j in 1:ncol(m.pair.diff)) {
      m.pair.diff[i,j] <- abs(v.perf[i]-v.perf[j]) # /((v.perf[i]+v.perf[j])/2)
    }
  }
  diag(m.pair.diff) <- 0
  return(m.pair.diff)
}

createPerceivedCouplingMatrix <- function(d.compos_agents) {
  m <- matrix(data=0,nrow=nrow(d.compos_agents),ncol=nrow(d.compos_agents),dimnames=list(rownames(d.compos_agents),rownames(d.compos_agents)))
  for(i in 1:nrow(m)) {
    for(j in 1:ncol(m)) {
        if(strsplit(rownames(m)[i],split="[.]")[[1]][1] == strsplit(colnames(m)[j],split="[.]")[[1]][1]) {
          m[i,j] <- 1
        }
    }
  }
  return(m)
}

createTechnicalCouplingMatrix <- function(d.compos.dependencies,d.compos_agents) {
  m <- matrix(data=0,nrow=nrow(d.compos_agents),ncol=nrow(d.compos_agents),dimnames=list(rownames(d.compos_agents),rownames(d.compos_agents)))
  for(i in 1:nrow(d.compos.dependencies)) {
    if(d.compos.dependencies$origin[i]%in%rownames(m)&d.compos.dependencies$target[i]%in%rownames(m)) {
      m[d.compos.dependencies$origin[i],d.compos.dependencies$target[i]] <- 1
    }
  }
  return(m)
}


