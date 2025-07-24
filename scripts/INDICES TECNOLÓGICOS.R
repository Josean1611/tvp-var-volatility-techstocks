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

#IMPORTAMOS LOS DATOS DE EXCEL DESDE FILE/IMPORT DATABASE

CHINA<-as.xts(CHINA)
EEUU<-as.xts(EEUU)
EUROPA<-as.xts(EUROPA)
TAIWAN<-as.xts(TAIWAN)


rend.CHINA<-100*dailyReturn(CHINA,type="log")
rend.EEUU<-100*dailyReturn(EEUU,type="log")
rend.EUROPA<-100*dailyReturn(EUROPA,type="log")
rend.TAIWAN<-100*dailyReturn(TAIWAN,type="log")

NOMBRES<-c("China","EEUU","Europa","Taiwan")

rend<-cbind.xts(rend.CHINA,rend.EEUU,rend.EUROPA,rend.TAIWAN)
any(is.na(rend))
rend<-na.omit(rend)
colnames(rend)<-NOMBRES
rend

REND.INDICES<-as.zoo(rend)
REND.INDICES
basicStats(REND.INDICES)
BS<-basicStats(REND.INDICES)
write.xlsx(BS,file="C:/Users/Usuario/Desktop")

dca = ConnectednessApproach(REND.INDICES, 
                            nlag=1, 
                            nfore=12,
                            model="TVP-VAR",
                            connectedness="Time",
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.96, prior="BayesPrior")))

dca

DCA = list()
DCA = suppressMessages(ConnectednessApproach(REND.INDICES, 
                                                  nlag=1, 
                                                  nfore=12))
PlotTCI(dca, ca=DCA, ylim=c(0,80))

# Define las fechas de inicio y fin
start_date <- as.Date("2018-09-01")
end_date <- as.Date("2019-02-28")

print("Dimensiones de CT:")
print(dimnames(dca$CT)[[3]])

print("Dimensiones de TCI:")
print(dimnames(dca$TCI)[[1]])


# Función para filtrar matrices bidimensionales
filter_dates_2d <- function(data, start_date, end_date) {
  date_strings <- dimnames(data)[[1]]
  date_names <- as.Date(date_strings, format="%Y-%m-%d")
  
  # Filtrar solo las fechas válidas
  valid_dates <- !is.na(date_names)
  date_names <- date_names[valid_dates]
  date_strings <- date_strings[valid_dates]
  data <- data[valid_dates, , drop = FALSE]
  
  date_filter <- date_names >= start_date & date_names <= end_date
  filtered_data <- data[date_filter, , drop = FALSE]
  dimnames(filtered_data)[[1]] <- date_strings[date_filter]
  
  return(filtered_data)
}

# Función para filtrar matrices tridimensionales
filter_dates_3d <- function(data, start_date, end_date) {
  date_strings <- dimnames(data)[[3]]
  date_names <- as.Date(date_strings, format="%Y-%m-%d")
  
  # Filtrar solo las fechas válidas
  valid_dates <- !is.na(date_names)
  date_names <- date_names[valid_dates]
  date_strings <- date_strings[valid_dates]
  data <- data[, , valid_dates, drop = FALSE]
  
  date_filter <- date_names >= start_date & date_names <= end_date
  filtered_data <- data[, , date_filter, drop = FALSE]
  dimnames(filtered_data)[[3]] <- date_strings[date_filter]
  
  return(filtered_data)
}

# Filtrar las matrices y arreglos dentro de dca si no hay fechas no válidas
dca_filtered <- list(
  TABLE = dca$TABLE,
  CT = filter_dates_3d(dca$CT, start_date, end_date),
  TCI = filter_dates_2d(dca$TCI, start_date, end_date),
  TO = filter_dates_2d(dca$TO, start_date, end_date),
  FROM = filter_dates_2d(dca$FROM, start_date, end_date),
  NET = filter_dates_2d(dca$NET, start_date, end_date),
  NPT = filter_dates_2d(dca$NPT, start_date, end_date),
  NPDC = filter_dates_3d(dca$NPDC, start_date, end_date),
  PCI = filter_dates_3d(dca$PCI, start_date, end_date),
  INFLUENCE = filter_dates_3d(dca$INFLUENCE, start_date, end_date),
  config = dca$config,
  date = dca$date
)

PlotNET(dca_filtered, ca=DCA, ylim=c(-60,60))

PlotNPDC(dca, ca=DCA, ylim=c(-30,40))

DCA

fevd = FEVD(Phi=fit$B, Sigma=fit$Q, nfore=10, type="time", generalized=TRUE)$FEVD
fit = VAR(REND.INDICES, configuration=list(nlag=1))
TABLA.1<-ConnectednessTable(fevd)
class(TABLA.1)
TABLA.1

DCT<-as.data.frame(TABLA.1[["TABLE"]])
DCT
colnames(DCT)[1] <-"CHINA"
colnames(DCT)[2] <-"EEUU"
colnames(DCT)[3] <-"EUROPA"
colnames(DCT)[4] <-"TAIWAN"
DCT

rownames(DCT)[1] <-"CHINA"
rownames(DCT)[2] <-"EEUU"
rownames(DCT)[3] <-"EUROPA"
rownames(DCT)[4] <-"TAIWAN"

DCT
write.xlsx(DCT,file="C:/Users/Usuario/Desktop")
