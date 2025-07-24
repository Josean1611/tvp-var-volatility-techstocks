library(PortfolioAnalytics)
library(quantmod)
library(fBasics)
library(ggplot2)
library(PerformanceAnalytics)
library(rugarch)
library(rmgarch)
library(writexl)
library(WriteXLS)
library(openxlsx)
library(xts)
library(zoo)
library(readr)
library(dplyr)
library(ConnectednessApproach)
library(devtools)
library(data.table)
install.packages("data.table")
#COPIO ESTO PARA TENER LA REFERENCAI DE COMO HE DE TENER LOS DATOS 
load("C:/Users/Usuario/Desktop/DInamic conect/El bueno/acg2020.rda")
basicStats(acg2020)
acg2020
#SEGUIMOS CON LOS DATOS. USAMOS DATOS DE LAS EMPRESAS.EMPEZAMOS CON LAS EMPRESAS. LUEGO ÍNDICES

CARTERA<-c("TSM","NVDA","AMD","META","AAPL")
getSymbols(CARTERA,from="2010-01-01",to="2024-05-20")

rend.TSM<-100*dailyReturn(TSM,type="log")
rend.NVDA<-100*dailyReturn(NVDA,type="log")
rend.AMD<-100*dailyReturn(AMD,type="log")
rend.META<-100*dailyReturn(META,type="log")
rend.AAPL<-100*dailyReturn(AAPL,type="log")

#UNIFICAMOS LAS SERIES

rend<-cbind.xts(rend.TSM,rend.NVDA,rend.AMD,rend.META,rend.AAPL)
any(is.na(rend))
rend<-na.omit(rend)
colnames(rend)<-CARTERA
rend

#PASAMOS LOS DATOS A TIPO zoo
REND.CARTERA<-as.zoo(rend)
REND.CARTERA
basicStats(REND.CARTERA)
BS<-basicStats(REND.CARTERA)
write.xlsx(BS,file="C:/Users/Usuario/Desktop")
#YA TENEMOS LOS DATOS PREPARADOS PARA TRBAJAR CON ELLOS

dca = ConnectednessApproach(REND.CARTERA, 
                            nlag=1, 
                            nfore=12,
                            window.size=200,
                            model="TVP-VAR",
                            connectedness="Time",
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.96, prior="BayesPrior")))
dca

DCA = list()

DCA = suppressMessages(ConnectednessApproach(REND.CARTERA, 
                                                    nlag=1, 
                                                    nfore=12))


PlotTCI(dca, ca=DCA, ylim=c(0,80))

PlotNET(dca, ca=DCA, ylim=c(-20,20))

PlotNPDC(dca, ca=DCA, ylim=c(-20,20))

DCA

#esto es para la tabla

fevd = FEVD(Phi=fit$B, Sigma=fit$Q, nfore=10, type="time", generalized=TRUE)$FEVD
fit = VAR(REND.CARTERA, configuration=list(nlag=1))
TABLA.1<-ConnectednessTable(fevd)
class(TABLA.1)
names(TABLA.1)<-CARTERA
TABLA.1

DCT<-as.data.frame(TABLA.1[["TABLE"]])
DCT
colnames(DCT)[1] <-"TSM"
colnames(DCT)[2] <-"NVDA"
colnames(DCT)[3] <-"AMD"
colnames(DCT)[4] <-"META"
colnames(DCT)[5] <-"AAPL"
DCT

rownames(DCT)[1] <-"TSM"
rownames(DCT)[2] <-"NVDA"
rownames(DCT)[3] <-"AMD"
rownames(DCT)[4] <-"META"
rownames(DCT)[5] <-"AAPL"
DCT
write.xlsx(DCT,file="C:/Users/Usuario/Desktop")
