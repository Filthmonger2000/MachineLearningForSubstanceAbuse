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

memory.limit(size = 50000)
options(scipen = 999999)

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

CTDat <- expand_grid(
  var = names(var1),
  alph = c(0.0001,0.001, 0.005, 0.1))

democols <- as.character(paste0(colnames(data)[1:14], collapse = "+"))

successTab <- data.frame("", "", "", "", "")
colnames(successTab) <- c("Substance", "Alpha", "Chance", "Success", "multChance")
samp <- 80/100

for(k in 1:nrow(CTDat)){
  print((k/nrow(CTDat))*100)
  data <- cbind(demodat, var1)
  t <- CTDat$var[k]
  t <- as.character(t)
  names(data)[names(data) == t] <- 't'
  data$t <- as.factor(data$t)
  index <- sample(1:nrow(data), round(samp*nrow(data)))
  train <- data[index,]
  test <- data[-index,]
  
  ctreer <- ctree(formula = t ~ inhospyr+catage+WRKSTATWK2+irwrkstat+ifather+IRINSUR4+irfstamp+IRFAMIN3+govtprog+PDEN10+COUTYP4+snrlgsvc+yerlgsvc+imother,
                  data = train,
                  control = ctree_control(alpha = CTDat$alph[k]))
  
  pdf(paste("CTrees/",CTDat$var[k], CTDat$alph[k], "CTree.pdf", sep = ""))
  plot(ctreer)
  dev.off()
  ctreePred <- predict(ctreer, test, type = "response")
  
  perSuc <- data.frame("test" = test$t, "train" = ctreePred)
  perSuc$train <- factor(perSuc$train, levels=levels(test$t))
  if(nrow(count(ctreePred)) > 1){
    perSuc$SAME <- ifelse(perSuc$test == perSuc$train, TRUE, FALSE)
    success <- round(count(perSuc$SAME)[2,2]/nrow(perSuc)*100, 5)
  }else{
    success <- max(count(as.numeric(train$t)))/nrow(train)*100
  }  
  names(data)[names(data) == "t"] <- CTDat$var[k]
  
  Chance <- max(count(as.numeric(train$t)))/nrow(train)*100
  
  newRow <- c(CTDat$var[k], CTDat$alph[k], Chance, success, (success/Chance)-1)
  names(newRow) <- colnames(successTab)
  successTab <- rbind(successTab, newRow)
}
successTab <- successTab[-1,]
successTab <- successTab %>%
  group_by(Substance) %>%
  slice(which.max(Success))
#write.csv(successTab, "CTreeSumsFinal.csv")
