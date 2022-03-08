#Import
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
library(tidyverse)
library(tidyquant)
library(neuralnet)
library(ggplot2)
library(zoo)
library(extrafont)
library(ggthemes)
library(NeuralNetTools)
library(e1071)
library(gbm)
library(Hmisc)
library(dplyr)
library(purrr)
library(plyr)
library(data.table)
library(lime)
library(DescTools)
library(caret)
library(doBy)
library(zoo)
library(TTR)
library(gtrendsR)
library(tidyverse)
library(lubridate)

#Load Data
if(!exists("PUF2019_100920")){
load(file = 'SA.RData')
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

# GBMDat <- expand_grid(
#   var = names(var1),
#   shrinkage = c(0.001),
#   intDepth = c(1,3,5))

sums <- data.frame("", "", "", "", "", "", "", "", "")
names(sums) <- c("Trial", "var", "Shrinkage", "intDepth", "Chance", "RawChance", "Success", "Result", "MultSuc")

k <- 1
repeat{
data <- cbind(demodat, var1)

t <- GBMDat$var[k]
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
index <- sample(1:(nrow(data)),size = round(nrow(data)*0.8))
trainer <- as.data.frame(data[index,])
tester <- as.data.frame(data[-index,])

gbm.train <- gbm(
  formula = t ~ inhospyr+catage+WRKSTATWK2+irwrkstat+ifather+IRINSUR4+irfstamp+IRFAMIN3+govtprog+PDEN10+COUTYP4+snrlgsvc+yerlgsvc+imother,
  data = trainer,
  distribution = "poisson",
  n.trees = 10000,
  interaction.depth = as.numeric(GBMDat$intDepth[k]),
  bag.fraction = .8,
  train.fraction = .8,
  n.cores = NULL,
  verbose = TRUE,
  cv.folds = 1,
  n.minobsinnode = 10,
  shrinkage = GBMDat$shrinkage[k]
)

OptimalTrees <- which.min(gbm.train$valid.error)

summariser <- summary(gbm.train)
summariser %>%
 filter(rel.inf >= 1L & rel.inf <= 70L) %>%
 ggplot() +
 aes(x = var, weight = rel.inf) +
 geom_bar(fill = "#1A55BD") +
 labs(x = "Variable", 
 y = "%", title = "Relatively Importance of Variables in Predicting Cocaine History in employed GBM") +
 coord_flip() +
 theme_minimal()

pred <- predict.gbm(gbm.train, n.trees = OptimalTrees, tester, type = "response")
predround <- as.vector(round(pred))
preddf <- data.frame(Pred = predround, Actual = tester$t)
preddf <- preddf %>%
  na.omit() %>%
  as.data.frame()
preddf$Correct[preddf$Pred == preddf$Actual] <- 1
preddf$Correct[preddf$Pred != preddf$Actual] <- 0
preddf <- preddf %>%
  na.omit()
origpreddf <- preddf
Success <- (count(preddf$Correct == 1)[2,2]/nrow(preddf))*100
Chance <- max(count(trainer$t))/nrow(trainer)*100

summer <- 100-Chance
summer1 <- Success-Chance
summer <- (summer1/summer)*100

multSuccess <- (Success/Chance)-1

newRow <- data.frame(k, GBMDat$var[k], GBMDat$shrinkage[k], GBMDat$intDepth[k], Chance, 1/nrow(count(trainer$t)), Success, summer, multSuccess)
names(newRow) <- c("Trial", "var", "Shrinkage", "intDepth", "Chance", "RawChance", "Success", "Result", "MultSuc")

sums <- rbind(sums,newRow)

predround <- as.vector(round(pred))
preddf <- data.frame(Pred = predround, Actual = tester$t)
preddf <- preddf %>%
  na.omit() %>%
  as.data.frame()
preddf$Correct<- 0
preddf$Correct[(preddf$Pred-1 == preddf$Actual) | (preddf$Pred == preddf$Actual) | (preddf$Pred+1 == preddf$Actual)] <- 1
preddf <- preddf %>%
  na.omit()
origpreddf <- preddf
SuccessBin <- (count(preddf$Correct == 1)[2,2]/nrow(preddf))*100
Chance <- max(count(trainer$t))/nrow(trainer)*100

summer <- 100-Chance
summer1 <- SuccessBin-Chance
summer <- (summer1/summer)*100

multSuccessBin <- (SuccessBin/Chance)-1

newRow <- data.frame(k+nrow(GBMDat), paste(GBMDat$var[k], "Bin", sep = ""), GBMDat$shrinkage[k], GBMDat$intDepth[k], Chance, 1/nrow(count(trainer$t)), SuccessBin, summer, multSuccess)
names(newRow) <- c("Trial", "var", "Shrinkage", "intDepth", "Chance", "RawChance", "Success", "Result", "MultSuc")

sums <- rbind(sums,newRow)


printer <- paste(k/nrow(GBMDat)*100,newRow$var, round(newRow$Result, 2))

print(printer)

if(k >= nrow(GBMDat)){
  break
}
k <- k + 1

}
sums <- sums %>%
  na.omit() %>%
  group_by(var) %>%
  slice(which.max(MultSuc))
sums$MultSuc <- sums$Success/sums$Chance


write.csv(sums, "GBMSumsFinal.csv")
