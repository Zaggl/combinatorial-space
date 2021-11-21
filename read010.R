library(xlsx)
library(igraph)
library(network)
library(sna)
library(ndtv)
library(plyr)
library(numDeriv)
library(Hmisc)
library(ggplot2)
library(stringr)
library(rgl)
library(data.table)
library(rgexf)
library(vegan)
library(QuantPsyc)
library(stargazer)
library(spdep)
library(clusterSEs)

##### READ Data: ######
### Read: Score Data:
d.agents <- read.xlsx(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data02\\10-18-2016_All User Scores Data.xlsx",sheetName="Sheet1",stringsAsFactors=F)
rownames(d.agents) <- d.agents$GitHub.Name
d.agents <- d.agents[sort(rownames(d.agents)),]
d.agents$id <- LETTERS[1:nrow(d.agents)]

#### Read: Components Lists of used components: ####
d.agents_compos <- as.data.frame(rbindlist(list(
    cbind("ph1",read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data02\\components_spring2016-greenironhack_phase1.csv",stringsAsFactors=F))
   ,cbind("ph2",read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data02\\components_spring2016-greenironhack_phase2.csv",stringsAsFactors=F))
   ,cbind("ph3",read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data02\\components_spring2016-greenironhack_phase3.csv",stringsAsFactors=F))
   ,cbind("ph4",read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data02\\components_spring2016-greenironhack_phase4.csv",stringsAsFactors=F))
   )))
colnames(d.agents_compos) <- c("phase","agent","component.raw")
d.agents_compos$agent[which(d.agents_compos$agent=="jeffkeilmann")] <- "jeffkeilman"
d.agents_compos <- d.agents_compos[d.agents_compos$agent!="ccand93",]

ideas <- d.agents_compos$component.raw
ideas <- gsub(pattern="\\[]",replacement=",",x=ideas)
ideas <- gsub(pattern="]",replacement="",x=ideas)
ideas <- gsub(pattern="\\[",replacement="",x=ideas)
ideas <- gsub(pattern=", ",replacement=",",x=ideas)
ideas <- gsub(pattern="''",replacement=",",x=ideas)
ideas <- gsub(pattern="'",replacement=",",x=ideas)
ideas <- strsplit(ideas,split=",")
ideas <- rapply(ideas,c)
ideas <- ideas[ideas!=""]
ideas <- unique(ideas)
agents <- apply(expand.grid(unique(d.agents_compos$agent),unique(d.agents_compos$phase)),1,paste,collapse=".")

d.compos_agents <- as.data.frame(matrix(data=0,nrow=length(ideas),ncol=length(agents),dimnames=list(ideas,agents)))
for(i in 1:nrow(d.compos_agents)) {
  for(j in 1:ncol(d.compos_agents)) {
    d.compos_agents[i,j] <- grepl(rownames(d.compos_agents)[i]
            ,subset(d.agents_compos,agent==strsplit(colnames(d.compos_agents)[j],"\\.")[[1]][1]&phase==strsplit(colnames(d.compos_agents)[j],"\\.")[[1]][2])$component.raw)
  }
} ; rm(ideas,agents,i,j,d.agents_compos)

#### Read: Components List of market components: ####
d.compos <- read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data02\\library_market.csv",stringsAsFactors=F)
colnames(d.compos)[colnames(d.compos)=="frequency"] <- "freq"
colnames(d.compos)[colnames(d.compos)=="X"] <- "name"
d.compos <- d.compos[order(d.compos$name),]
rownames(d.compos) <- d.compos$name
d.compos$name <- NULL

d.compos_agents <- merge(x=d.compos,y=d.compos_agents,by="row.names",all=T)
d.compos_agents[is.na(d.compos_agents)] <- 0
rownames(d.compos_agents) <- d.compos_agents$Row.names
d.compos_agents$Row.names <- NULL
d.compos_agents <- d.compos_agents[rowSums(d.compos_agents[])>0,]
d.compos_agents$id <- seq(1,nrow(d.compos_agents),1)
rm(d.compos)
rownames(d.compos_agents) <- gsub("_[^_]+_", "",rownames(d.compos_agents))
rownames(d.compos_agents) <- gsub("_", "",rownames(d.compos_agents))

#### Read: Components technical dependencies: ####
d.compos.dependencies <- read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data02\\dependencies_edgelist.csv",stringsAsFactors=F)
d.compos.dependencies$origin_component <- gsub(pattern="src_",replacement="",x=d.compos.dependencies$origin_component)
d.compos.dependencies$origin_component <- gsub(pattern=".js",replacement="",x=d.compos.dependencies$origin_component)
d.compos.dependencies$target_package <- ifelse(startsWith(x=d.compos.dependencies$target_package,prefix=" ./")|startsWith(x=d.compos.dependencies$target_package,prefix=" ../"),d.compos.dependencies$package,d.compos.dependencies$target_package)
d.compos.dependencies$target_component <- trimws(d.compos.dependencies$target_component)
d.compos.dependencies$target_package <- trimws(d.compos.dependencies$target_package)
d.compos.dependencies$origin <- paste(d.compos.dependencies$package,".",d.compos.dependencies$origin_component,sep="")
d.compos.dependencies$target <- paste(d.compos.dependencies$target_package,".",d.compos.dependencies$target_component,sep="")

#### Read: Complexity Metrics: ####
d.agents.codeCompl <- read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data02\\green_complexity_metrics.csv",stringsAsFactors=F)
d.agents.codeCompl$GitHub.Name[which(d.agents.codeCompl$GitHub.Name=="jeffkeilmann")] <- "jeffkeilman"
d.agents.codeCompl.ph1 <- subset(d.agents.codeCompl,phase==1)
d.agents.codeCompl.ph2 <- subset(d.agents.codeCompl,phase==2)
d.agents.codeCompl.ph3 <- subset(d.agents.codeCompl,phase==3)
d.agents.codeCompl.ph4 <- subset(d.agents.codeCompl,phase==4)
colnames(d.agents.codeCompl.ph1) <- paste(colnames(d.agents.codeCompl.ph1),".ph1",sep="")
colnames(d.agents.codeCompl.ph2) <- paste(colnames(d.agents.codeCompl.ph2),".ph2",sep="")
colnames(d.agents.codeCompl.ph3) <- paste(colnames(d.agents.codeCompl.ph3),".ph3",sep="")
colnames(d.agents.codeCompl.ph4) <- paste(colnames(d.agents.codeCompl.ph4),".ph4",sep="")
d.agents.codeCompl <- cbind(d.agents.codeCompl.ph1,d.agents.codeCompl.ph2,d.agents.codeCompl.ph3,d.agents.codeCompl.ph4)
d.agents.codeCompl <- d.agents.codeCompl[order(d.agents.codeCompl$GitHub.Name.ph1),]
d.agents.codeCompl <- d.agents.codeCompl[d.agents.codeCompl$GitHub.Name.ph1!="ccand93",]
d.agents <- cbind(d.agents,d.agents.codeCompl)
rm(d.agents.codeCompl,d.agents.codeCompl.ph1,d.agents.codeCompl.ph2,d.agents.codeCompl.ph3,d.agents.codeCompl.ph4)


#############################################
#############################################
#### Read: Lines of Code: #### UNDER RECONSTRUCTION ####
d.agents.loc <- as.data.frame(rbindlist(list(
   cbind(1,read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data\\llocs_green_phase1.csv",stringsAsFactors=F))
  ,cbind(2,read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data\\llocs_green_phase2.csv",stringsAsFactors=F))
  ,cbind(3,read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data\\llocs_green_phase3.csv",stringsAsFactors=F))
  ,cbind(4,read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data\\llocs_green_phase4.csv",stringsAsFactors=F))

)))

data.gree.loc.ph1 <- read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data\\llocs_green_phase1.csv",stringsAsFactors=F)
rownames(data.gree.loc.ph1) <- substr(data.gree.loc.ph1$X,start=19,stop=99)
data.gree.loc.ph1$loc.logical.sum <- 0 
data.gree.loc.ph1$loc.physical.sum <- 0 
for(i in 1:nrow(data.gree.loc.ph1)) {
  data.gree.loc.ph1$loc.logical.sum[i]  <- sum(as.numeric(str_extract_all(data.gree.loc.ph1$Logical.LOC[i], "\\d{1,9999}",simplify=T)))
  data.gree.loc.ph1$loc.physical.sum[i] <- sum(as.numeric(str_extract_all(data.gree.loc.ph1$Physical.LOC[i], "\\d{1,9999}",simplify=T)))
}
data.gree.loc.ph1 <- data.gree.loc.ph1[,c("loc.logical.sum","loc.physical.sum")]
data.gree.loc.ph1 <- data.gree.loc.ph1[order(rownames(data.gree.loc.ph1)),]
rownames(data.gree.loc.ph1)[which(rownames(data.gree.loc.ph1)=="jeffkeilmann")] <- "jeffkeilman"
data.gree.loc.ph1 <- data.gree.loc.ph1[-c(which(rownames(data.gree.loc.ph1)=="ccand93")),]

data.gree.loc.ph2 <- read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data\\llocs_green_phase2.csv",stringsAsFactors=F)
rownames(data.gree.loc.ph2) <- substr(data.gree.loc.ph2$X,start=19,stop=99)
data.gree.loc.ph2$loc.logical.sum <- 0 
data.gree.loc.ph2$loc.physical.sum <- 0 
for(i in 1:nrow(data.gree.loc.ph2)) {
  data.gree.loc.ph2$loc.logical.sum[i]  <- sum(as.numeric(str_extract_all(data.gree.loc.ph2$Logical.LOC[i], "\\d{1,9999}",simplify=T)))
  data.gree.loc.ph2$loc.physical.sum[i] <- sum(as.numeric(str_extract_all(data.gree.loc.ph2$Physical.LOC[i], "\\d{1,9999}",simplify=T)))
}
data.gree.loc.ph2 <- data.gree.loc.ph2[,c("loc.logical.sum","loc.physical.sum")]
data.gree.loc.ph2 <- data.gree.loc.ph2[order(rownames(data.gree.loc.ph2)),]
rownames(data.gree.loc.ph2)[which(rownames(data.gree.loc.ph2)=="jeffkeilmann")] <- "jeffkeilman"
data.gree.loc.ph2 <- data.gree.loc.ph2[-c(which(rownames(data.gree.loc.ph2)=="ccand93")),]

data.gree.loc.ph3 <- read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data\\llocs_green_phase3.csv",stringsAsFactors=F)
rownames(data.gree.loc.ph3) <- substr(data.gree.loc.ph3$X,start=19,stop=99)
data.gree.loc.ph3$loc.logical.sum <- 0 
data.gree.loc.ph3$loc.physical.sum <- 0 
for(i in 1:nrow(data.gree.loc.ph3)) {
  data.gree.loc.ph3$loc.logical.sum[i]  <- sum(as.numeric(str_extract_all(data.gree.loc.ph3$Logical.LOC[i], "\\d{1,9999}",simplify=T)))
  data.gree.loc.ph3$loc.physical.sum[i] <- sum(as.numeric(str_extract_all(data.gree.loc.ph3$Physical.LOC[i], "\\d{1,9999}",simplify=T)))
}
data.gree.loc.ph3 <- data.gree.loc.ph3[,c("loc.logical.sum","loc.physical.sum")]
data.gree.loc.ph3 <- data.gree.loc.ph3[order(rownames(data.gree.loc.ph3)),]
rownames(data.gree.loc.ph3)[which(rownames(data.gree.loc.ph3)=="jeffkeilmann")] <- "jeffkeilman"
data.gree.loc.ph3 <- data.gree.loc.ph3[-c(which(rownames(data.gree.loc.ph3)=="ccand93")),]
data.gree.loc.ph3 <- data.gree.loc.ph3[-c(which(rownames(data.gree.loc.ph3)=="modules/handlebars/node_modules/source-map/build/test-prefix.js'")),]

data.gree.loc.ph4 <- read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data\\llocs_green_phase4.csv",stringsAsFactors=F)
rownames(data.gree.loc.ph4) <- substr(data.gree.loc.ph4$X,start=19,stop=99)
data.gree.loc.ph4$loc.logical.sum <- 0 
data.gree.loc.ph4$loc.physical.sum <- 0 
for(i in 1:nrow(data.gree.loc.ph4)) {
  data.gree.loc.ph4$loc.logical.sum[i]  <- sum(as.numeric(str_extract_all(data.gree.loc.ph4$Logical.LOC[i], "\\d{1,9999}",simplify=T)))
  data.gree.loc.ph4$loc.physical.sum[i] <- sum(as.numeric(str_extract_all(data.gree.loc.ph4$Physical.LOC[i], "\\d{1,9999}",simplify=T)))
}
data.gree.loc.ph4 <- data.gree.loc.ph4[,c("loc.logical.sum","loc.physical.sum")]
data.gree.loc.ph4 <- data.gree.loc.ph4[order(rownames(data.gree.loc.ph4)),]
rownames(data.gree.loc.ph4)[which(rownames(data.gree.loc.ph4)=="jeffkeilmann")] <- "jeffkeilman"
data.gree.loc.ph4 <- data.gree.loc.ph4[-c(which(rownames(data.gree.loc.ph4)=="ccand93")),]
data.gree.loc.ph4 <- data.gree.loc.ph4[-c(which(rownames(data.gree.loc.ph4)==".js'")),]

#### Read: Evaluation of Technology: #### not used
d.agents.evalTech <- read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data\\cleaning_process_tech.csv",stringsAsFactors=F)

#### Read: Code Similarity: #### UNDER RECONSTRUCTION ####
m.agent_agent.codeSim.ph1 <- read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data01\\similarity-metric-phase-1.csv",stringsAsFactors=F)
m.agent_agent.codeSim.ph2 <- read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data01\\similarity-metric-phase-2.csv",stringsAsFactors=F)
m.agent_agent.codeSim.ph3 <- read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data01\\similarity-metric-phase-3.csv",stringsAsFactors=F)
m.agent_agent.codeSim.ph4 <- read.csv(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data01\\similarity-metric-phase-4.csv",stringsAsFactors=F)
m.agent_agent.codeSim.ph1$X[which(m.agent_agent.codeSim.ph1$X=="jeffkeilmann")] <- "jeffkeilman"
m.agent_agent.codeSim.ph2$X[which(m.agent_agent.codeSim.ph2$X=="jeffkeilmann")] <- "jeffkeilman"
m.agent_agent.codeSim.ph3$X[which(m.agent_agent.codeSim.ph3$X=="jeffkeilmann")] <- "jeffkeilman"
m.agent_agent.codeSim.ph4$X[which(m.agent_agent.codeSim.ph4$X=="jeffkeilmann")] <- "jeffkeilman"
colnames(m.agent_agent.codeSim.ph1)[which(colnames(m.agent_agent.codeSim.ph1)=="jeffkeilmann")] <- "jeffkeilman"
colnames(m.agent_agent.codeSim.ph2)[which(colnames(m.agent_agent.codeSim.ph2)=="jeffkeilmann")] <- "jeffkeilman"
colnames(m.agent_agent.codeSim.ph3)[which(colnames(m.agent_agent.codeSim.ph3)=="jeffkeilmann")] <- "jeffkeilman"
colnames(m.agent_agent.codeSim.ph4)[which(colnames(m.agent_agent.codeSim.ph4)=="jeffkeilmann")] <- "jeffkeilman"
rownames(m.agent_agent.codeSim.ph1) <- m.agent_agent.codeSim.ph1$X
rownames(m.agent_agent.codeSim.ph2) <- m.agent_agent.codeSim.ph2$X
rownames(m.agent_agent.codeSim.ph3) <- m.agent_agent.codeSim.ph3$X
rownames(m.agent_agent.codeSim.ph4) <- m.agent_agent.codeSim.ph4$X
m.agent_agent.codeSim.ph1$X <- NULL
m.agent_agent.codeSim.ph2$X <- NULL
m.agent_agent.codeSim.ph3$X <- NULL
m.agent_agent.codeSim.ph4$X <- NULL
m.agent_agent.codeSim.ph1 <- m.agent_agent.codeSim.ph1[-c(which(rownames(m.agent_agent.codeSim.ph1)=="ccand93")),-c(which(colnames(m.agent_agent.codeSim.ph1)=="ccand93"))] ##ccand93 did not submit and therefore he is not in the score data. maybe leave him in and assign a score of 0.
m.agent_agent.codeSim.ph2 <- m.agent_agent.codeSim.ph2[-c(which(rownames(m.agent_agent.codeSim.ph2)=="ccand93")),-c(which(colnames(m.agent_agent.codeSim.ph2)=="ccand93"))]
m.agent_agent.codeSim.ph3 <- m.agent_agent.codeSim.ph3[-c(which(rownames(m.agent_agent.codeSim.ph3)=="ccand93")),-c(which(colnames(m.agent_agent.codeSim.ph3)=="ccand93"))]
m.agent_agent.codeSim.ph4 <- m.agent_agent.codeSim.ph4[-c(which(rownames(m.agent_agent.codeSim.ph4)=="ccand93")),-c(which(colnames(m.agent_agent.codeSim.ph4)=="ccand93"))]
m.agent_agent.codeSim.ph1 <- m.agent_agent.codeSim.ph1[sort(rownames(m.agent_agent.codeSim.ph1)),sort(colnames(m.agent_agent.codeSim.ph1))]
m.agent_agent.codeSim.ph2 <- m.agent_agent.codeSim.ph2[sort(rownames(m.agent_agent.codeSim.ph2)),sort(colnames(m.agent_agent.codeSim.ph2))]
m.agent_agent.codeSim.ph3 <- m.agent_agent.codeSim.ph3[sort(rownames(m.agent_agent.codeSim.ph3)),sort(colnames(m.agent_agent.codeSim.ph3))]
m.agent_agent.codeSim.ph4 <- m.agent_agent.codeSim.ph4[sort(rownames(m.agent_agent.codeSim.ph4)),sort(colnames(m.agent_agent.codeSim.ph4))]

##### NOT IN USE: #####
#### Read: Survey Data: ####
data.gree.survy <- read.csv("C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data01\\NSF-PRE SURVEY - GREEN IronHack _March 31, 2018_22.20 EA.csv",stringsAsFactors=F)
data.gree.survy <- data.gree.survy[3:28,]

#### Read: Participants List: (not needed) ####
data.gree.parti <- read.xlsx(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data01\\Participants_greenIronhack.xls",sheetName="class registration")

#### Read: Click Data: ####
data.gree.click <- read.xlsx(file="C:\\Users\\micha\\Dropbox\\2018Sabine-Michael-HacksModeling\\data01\\10-18-2016_Cleaned Click Data.xlsx",sheetName="Sheet1")
data.gree.click$Phase <- data.gree.click$Phase+1
data.gree.click <- merge(x=data.gree.click,y=data.gree.score[,c("Number","GitHub.Name")],by.x="Clicked.",by.y="Number")
colnames(data.gree.click)[colnames(data.gree.click)=="GitHub.Name"] <- "Clicked.name"
data.gree.click <- merge(x=data.gree.click,y=data.gree.score[,c("Number","GitHub.Name")],by.x="Clicker.",by.y="Number")
colnames(data.gree.click)[colnames(data.gree.click)=="GitHub.Name"] <- "Clicker.name"
data.gree.click.matrix.ph2.det <- createAdjMatrix(clickListe=data.gree.click,phase=2,category="detail")
data.gree.click.matrix.ph2.sol <- createAdjMatrix(clickListe=data.gree.click,phase=2,category="solution")
data.gree.click.matrix.ph2.git <- createAdjMatrix(clickListe=data.gree.click,phase=2,category="github")
data.gree.click.matrix.ph3.det <- createAdjMatrix(clickListe=data.gree.click,phase=3,category="detail")
data.gree.click.matrix.ph3.sol <- createAdjMatrix(clickListe=data.gree.click,phase=3,category="solution")
data.gree.click.matrix.ph3.git <- createAdjMatrix(clickListe=data.gree.click,phase=3,category="github")
data.gree.click.matrix.ph4.det <- createAdjMatrix(clickListe=data.gree.click,phase=4,category="detail")
data.gree.click.matrix.ph4.sol <- createAdjMatrix(clickListe=data.gree.click,phase=4,category="solution")
data.gree.click.matrix.ph4.git <- createAdjMatrix(clickListe=data.gree.click,phase=4,category="github")






