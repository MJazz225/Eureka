#########################################################
#  
#  
#
#########################################################
#這是EurekaHedge和EurekaAi的報酬作比較
rm(list = ls())
library(xts)
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(zoo)

#讀檔案
EurekaAI = read.csv("EurekaAI.csv")
EurekaHedge = read.csv("EurekaHedge.csv")
FTSE = c("^FTSE")
getSymbols(FTSE, src = "yahoo", from = "2010-10-1", to = Sys.Date())
FTSE = to.monthly(FTSE)
#附上變數的名字
names(EurekaAI) = c("Date", "Return", "Index")
names(EurekaHedge) = c("Date", "EurekaHedge", "Index")

FTSEClose = (FTSE$FTSE.Close)
#換成時間序列和建檔
EurekaAIClose = merge(xts(EurekaAI$Index, order.by = as.yearmon(2010+seq(10,103)/12), EurekaAI$Index))
EurekaHedgeClose = merge(xts(EurekaHedge$Index, order.by = as.yearmon(2010+seq(10,103)/12), EurekaHedge$Index))

names(EurekaAIClose) = c("EurekaAI")
names(EurekaHedgeClose) = c("EurekaHedge")

################################################## 完 美 的 分 割 綫 ~ ~ ####################################################################ji
#計算
Mean = rbind(mean(EurekaHedgeClose), mean(EurekaAIClose), mean(FTSEClose))
Min = rbind(min(EurekaHedgeClose), min(EurekaAIClose), min(FTSEClose))
Max = rbind(max(EurekaHedgeClose), max(EurekaAIClose), max(FTSEClose))
Median = rbind(median(EurekaHedgeClose), median(EurekaAIClose), median(FTSEClose))
Standard_Devation = rbind(sd(EurekaHedgeClose), sd(EurekaAIClose), sd(FTSEClose))

Stat1 = data.frame(Mean, Min, Max, Median, Standard_Devation, row.names = c("EurekaHedge", "EurekaAI", "FTSE"))
#計算每日報酬率
lagEurekaAI = lag(EurekaAIClose,1)
simpleretAI = (EurekaAIClose-lagEurekaAI)/lagEurekaAI
lagEurekaHedge = lag(EurekaHedgeClose,1)
simpleretHedge = (EurekaHedgeClose-lagEurekaHedge)/lagEurekaHedge
lagFTSE = lag(FTSEClose,1)
simpleretFTSE = (FTSEClose-lagFTSE)/lagFTSE


#轉換成向量才能跑T檢定 
View(Stat1)

t.test(as.vector(EurekaAIClose), as.vector(EurekaHedgeClose), paired = FALSE, mu = 0, conf.level =  0.95,
       var.equal = TRUE)
t.test(as.vector(EurekaAIClose), as.vector(FTSEClose), paired = FALSE, mu = 0, conf.level =  0.95,
       var.equal = TRUE)

chart.CumReturns(merge(simpleretAI,simpleretHedge,simpleretFTSE), legend.loc = "bottomright", 
                 main = "EurekaAI vs HedgeFund vs FTSE", date.format = "%m-%Y")
