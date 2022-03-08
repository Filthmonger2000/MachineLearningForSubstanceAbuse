#Import
setwd("C:/Users/benow/Desktop/Dissertation/Robot Doctor")
library(tidyverse)
library(boot)
library(neuralnet)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(dplyr)
library(purrr)
library(plyr)
library(NeuralNetTools)
library(rapport)
library(rapportools)
library(grDevices)
library(VennDiagram)
library(reshape2)
library(data.table)
library(DMwR2)
library(plot3D)
library(raster)
library(rgl)
library(plotly)
library(partykit)
library(stringr)
library(DescTools)

memory.limit(size = 25000)
options(scipen = 999999)
samp <- 80/100
reps <- 3


if(exists("PUF2019_100920") == FALSE){
  load("SA.RData")
}

data <- PUF2019_100920
data <- data[, colSums(is.na(data)) == 0] 
data <- data[, sapply(data, function(x) !any(is.na(x)))]
demos <- c("inhospyr", "catage", "WRKSTATWK2", "irwrkstat", "ifather", "IRINSUR4", "irfstamp", "IRFAMIN3", "govtprog", "PDEN10", "COUTYP4", "snrlgsvc", "yerlgsvc", "imother")
demodat <- data[,demos]
var1 <- data[,-which(names(data) %in% demos)] %>%
  select(contains("rec")) %>%
  select(-contains("ii")) %>%
  select(-contains("ir")) %>%
  select(-contains("recvd")) %>%
  select(-contains("txrcvdrec")) %>%
  select(-contains("rect"))%>%
  select(-contains("trdrec"))%>%
  select(-contains(c("1","2", "3", "4","5","6","7","8","9")))

demoopt <- c("", "inhospyr", "catage", "WRKSTATWK2", "irwrkstat", "ifather", "IRINSUR4", "irfstamp", "IRFAMIN3", "govtprog", "PDEN10", "COUTYP4", "snrlgsvc", "yerlgsvc", "imother")

nnDat <- expand_grid(
  var = names(var1),
  algor = "rprop+",
  Hid1 = (5:9),
  Hid2 = (4:8),
  Thr = c(0.1),
  lrnrt = c(0.00001))

data <- cbind(demodat, var1)

sums <- data.frame("", "", "", "", "", "", "", "", "")
names(sums) <- c("Trial", "var", "Hid1", "Hid2", "Thr", "Chance", "RawChance", "Success", "Result")


 if(exists("sumsMax")){
   nnDat <- sumsMax
   nnDat$Thr <- 2
 }

#Neural Network
if(!exists("k")){
k <- 1
}
startTime <- Sys.time()
repeat{
  data <- cbind(demodat, var1)
  if(grepl(nnDat$var[k], pattern = "Bin")){
    t <- substr(nnDat$var[k],1,nchar(nnDat$var[k])-3)
  }else{
  t <- nnDat$var[k]
  }
  t <- as.character(t)
  names(data)[names(data) == t] <- 't'
  p <- 0
  repeat{
    p <- p + 1
    if(p %in% count(data$t)[,1]){
    }else{
      data$t[data$t > p] <- p
      break
    }
  }
  data <- data %>%
    na.omit()
   maxs <- apply(data, 2, max) 
   mins <- apply(data, 2, min)
  torg <- data[,"t"]
  scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
  index <- sample(1:nrow(data), round(samp*nrow(data)))
  train <- data[index,]
  test <- data[-index,]
  trainer <- scaled[index,]
  tester <- scaled[-index,]
  algor <- "rprop+"
  print(k)
  NN <- neuralnet(data = trainer, formula = t ~ inhospyr+catage+WRKSTATWK2+irwrkstat+ifather+IRINSUR4+irfstamp+IRFAMIN3+govtprog+PDEN10+COUTYP4+snrlgsvc+yerlgsvc+imother, reps, hidden = c(as.numeric(nnDat$Hid1[k]), as.numeric(nnDat$Hid2[k])), threshold = nnDat$Thr[k], err.fct = "sse", act.fct = "logistic", lifesign = "full", stepmax = 50000, learningrate = nnDat$lrnrt[k])
  
  pred <- neuralnet::compute(NN, tester[,1:14])
  pred <- as.numeric(pred$net.result)
  lvls <- count(tester$t)[,1]
  cuts <- c(-Inf, lvls[-1]-diff(lvls)/2, Inf)
  pred <- as.data.frame(cut(pred, breaks=cuts, labels=lvls))
  pred <- as.numeric(pred$`cut(pred, breaks = cuts, labels = lvls)`)
  
  
  tabl <- tibble("Actual" = as.numeric(test$t), "Pred" = pred, "T" = ifelse(Actual == Pred, 1, 0), "TClose" = ifelse(Actual == Pred | Actual+1 == Pred | Actual-1 == Pred, 1, 0))
  
  max(count(tabl$TB))/nrow(tabl)
  
    summer1 <- (max(count(tabl$Actual))/(nrow(tabl)))
    rsummer <- 1-summer1
    summer2 <- rsummer/summer1
    summer <- sum(tabl$T == 1)/((sum(tabl$T == 0)+sum(tabl$T == 1)))
    absol <- (summer*100)
    summer <- (summer-summer1)/summer1
    summer <- (summer/summer2)*100
    ifchance <- (summer1*100)
    rawchance <- (1/length(lvls))*100
  
  new_row <- c(k, t, (nnDat[k,3]), (nnDat[k,4]),(nnDat[k,5]), ifchance, rawchance, absol, summer) 
  names(new_row) <- c("Trial","var", "Hid1", "Hid2", "Thr", "Chance", "RawChance", "Success", "Result")
  sums <- rbind(sums, new_row)

  summer1 <- (max(count(tabl$Actual))/(nrow(tabl)))
  rsummer <- 1-summer1
  summer2 <- rsummer/summer1
  summer <- sum(tabl$TClose == 1)/((sum(tabl$TClose == 0)+sum(tabl$TClose == 1)))
  absol <- (summer*100)
  summer <- (summer-summer1)/summer1
  summer <- (summer/summer2)*100
  ifchance <- (summer1*100)
  rawchance <- (1/length(lvls))*100
  
  new_row <- c(k+nrow(nnDat), paste(t, "Bin", sep = ""), (nnDat[k,3]), (nnDat[k,4]),(nnDat[k,5]), ifchance, rawchance, absol, summer) 
  names(new_row) <- c("Trial","var", "Hid1", "Hid2", "Thr", "Chance", "RawChance", "Success", "Result")
  sums <- rbind(sums, new_row)
  
  names(data)[names(data) == "t"] <- nnDat$var[k]
  #data <- data %>%
  #  select(-t)
  if(k >= nrow(nnDat)) { break }
  
  print(Sys.time() - startTime)
  k <- k + 1

}
if(k >= nrow(nnDat)){
sums <- sums %>%
  na.omit()
sums$Hid1 <- as.numeric(as.character(sums$Hid1))
sums$Hid2 <- as.numeric(as.character(sums$Hid2))
sums$Thr <- as.numeric(as.character(sums$Thr))
p <- t.test(c(as.numeric(sums$Chance)), c(as.numeric(sums$Success)))
sums$p <- p$p.value
sums$multChance <- (as.numeric(sums$Success)/as.numeric(sums$Chance))-1
#write.csv(sums, "NNSumsFinal.csv")
sumsMax <- sums %>%
  group_by(var) %>%
  slice(which.max(Success))

}
