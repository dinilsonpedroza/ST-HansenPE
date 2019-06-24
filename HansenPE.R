#################Criando a ST de propor??o de casos######################

getwd()
PCH<-read.csv2("Propor??odecasos.csv", header = T)
PCH
PCHST<-ts(PCH, start = c(2001,01), frequency = 12)
PCHST
class(PCHST)
plot(PCHST, ylab= "casos por 100 mil", xlab= "anos", main= "Hansen?ase em PE", col="blue")

#################Criando a ST de propor??o de multibacilar###############

PCM<-read.csv2("Propor??omultibacilar.csv", header = T)
PCM
PCMST<-ts(PCM, start = c(2001,01), frequency = 12)
PCMST
plot(PCMST, ylab= "multibacilar por 100 mil", xlab= "anos", main= "Hansen?ase em PE", col="orange")


################Criando a ST de propor??o de incapacitados ###############

PCI<-read.csv2("Propor??oincapacidade.csv",header = T)
PCI
PCIST<-ts(PCI, start = c(2001,01), frequency = 12)
plot(PCIST, ylab= "incapacitados por 100 mil", xlab= "anos", main= "Hansen?ase em PE", col="yellow")


##########################Decompondo as s?ries############################

plot(decompose(PCHST))
plot(decompose(PCMST))
plot(decompose(PCIST))

###############Estudando a sazonalidade das s?ries ######################

################# estudando a sazonalidade da ST ######

monthplot(PCHST, ylab= "casos por 100 mil", 
          xlib="meses", col="blue")

monthplot(PCMST, ylab= "casos por 100 mil", 
          xlib="meses", col="blue")

monthplot(PCIST, ylab= "casos por 100 mil", 
          xlib="meses", col="blue")

###############Estudando a estacionariedade das s?ries###################
###############Aplicando o teste Dickey-Fuller em casos(PCHST) ##########
library(urca)
summary(ur.df(PCHST, type='none', lags=0))
#######A s?rie PCHST ? n?o estacion?ria##################################

###############Aplicando o teste Dickey-Fuller em multibacilar(PCMST) ###
summary(ur.df(PCMST, type='none', lags=0))
#######A s?rie PCMST ? n?o estacion?ria##################################

###############Aplicando o teste Dickey-Fuller em incapacitados (PCIST) #
summary(ur.df(PCIST, type='none', lags=0))
#######A s?rie PCIST ? estacion?ria######################################

###############Diferenciando as s?ries###################################

dfPCHST <- diff(PCHST)
plot(dfPCHST, main = "st com 1? diferen?a", ylab="1? diferen?a", 
     xlab="tempo", col="blue")
dfPCMST<-diff(PCMST)
plot(dfPCMST, main = "st com 1? diferen?a", ylab="1? diferen?a", 
     xlab="tempo", col="blue")
dfPCIST<-diff(PCIST)
plot(dfPCIST, main = "st com 1? diferen?a", ylab="1? diferen?a", 
     xlab="tempo", col="blue")

###############Aplicando o teste Dickey-Fuller nas s?ries diferenciadas##
summary(ur.df(dfPCHST, type='none', lags=0))
summary(ur.df(dfPCMST, type='none', lags=0))
summary(ur.df(dfPCIST, type='none', lags=0))
###############Todas s?o estacion?rias###################################

###############Modelos SARIMAS###########################################

SARPCHST<-Arima(PCHST, order = c(1,1,1), seasonal = c(1,1,1), method="ML")
SARPCMST<-Arima(PCMST, order = c(1,1,1), seasonal = c(1,1,1), method="ML")
SARPCIST<-Arima(PCIST, order = c(1,1,1), seasonal = c(1,1,1), method="ML")

########Verificando o ajuste dos modelos ################################
########Gr?fico #########################################################

install.packages("forecast")
require(forecast)

plot(PCHST, ylab= "propor??o de casos", main= "Grafico 13: s?rie de casos x SARIMA", 
     xlab= "tempo", col= 'red')
lines(fitted(SARPCHST), col='blue')

plot(PCMST, ylab="casos multibacilares", xlab= "tempo", main= "Gr?fico 14: s?rie de multibacilares 
     x SARIMA", col= 'red')
lines(fitted(SARPCMST), col='blue')

plot(PCIST, ylab="incapacitados", xlab="tempo", main= "Gr?fico 15: s?rie 
     de incapacitados x SARIMA", col= 'red')
lines(fitted(SARPCIST), col='blue')

accuracy(SARPCHST)
accuracy(SARPCMST)
accuracy(SARPCIST)

###########################Estudando os res?duos##########################

checkresiduals(SARPCHST)
checkresiduals(SARPCMST)
checkresiduals(SARPCIST)

###########Resultados: o comportamento dos erros pr?ximo a nomalidade######

###########################################################################
######################Auto-Arima###########################################
###########################################################################

AUTPCHST<- auto.arima(PCHST, max.p=5, max.q=5, max.P=2, max.Q=2,
                      seasonal = T)
AUTPCMST<- auto.arima(PCMST, max.p=5, max.q=5, max.P=2, max.Q=2,
                      seasonal = T)
AUTPCIST<- auto.arima(PCIST, max.p=5, max.q=5, max.P=2, max.Q=2,
                      seasonal = T)

######################Gr?ficos de ajuste dos modelos auto-Arima############

plot(PCHST, col= 'red')
lines(fitted(AUTPCHST), col='blue')

plot(PCMST, col= 'red')
lines(fitted(AUTPCMST), col='blue')

plot(PCIST, col= 'red')
lines(fitted(AUTPCIST), col='blue')

######################Testes de ajustes dos modelos auto.arima###############


accuracy(AUTPCHST)
accuracy(AUTPCMST)
accuracy(AUTPCIST)

###########################Estudando os res?duos#############################

checkresiduals(AUTPCHST)
checkresiduals(AUTPCMST)
checkresiduals(AUTPCIST)

#############################################################################
######################Previs?es##############################################
#############################################################################

forecast.PCH <- forecast(SARPCHST, h=60)
forecast.PCH
forecast.PCM <- forecast(SARPCMST, h=60)
forecast.PCM
forecast.PCI <- forecast(SARPCIST, h=60)
forecast.PCI
######################Gr?ficos da Previs?o###################################

plot(forecast(SARPCHST, h=60, level=0.95), ylab="propor??o de casos por 100 mil hab.", xlab = "tempo", 
     main = "Gr?fico 16: previs?o da propor??o de casos")

plot(forecast(SARPCMST, h=60,level=0.95), ylab="casos multibacilares por cem mil hab.", xlab = "tempo",
     main = "Gr?fico 17: previs?o de casos multibacilares" )

plot(forecast(SARPCIST, h=60, level=0.95), ylab = "incapacitados", xlab = "tempo",
     main = "Gr?fico 18: previs?o de incapacitados por 100 mil hab.")

library(knitr)
devtools::install_github("rstudio/rmarkdown")
install.packages("yaml")

################Estudando a sazonalidade das s?ries com QS do Arima X-13#######

download.file("https://www.census.gov/ts/x13as/pc/x13as_V1.1_B19.zip",
              destfile = "./x13.zip")
unzip("x13.zip")
install.packages("seasonal")
library("seasonal")
checkX13()

#########Testando a ocorr?ncia da sazonalidade usando o qs do X-13#############

######testando a sazonalidade na propor??o de casos############################
PCHSTajustada <- seas(PCHST)
qs(PCHSTajustada)
######Conclu?-se que h? componente sazonal nesta s?rie original################

######testando a sazonalidade na s?rie de casos multibacilares#################
PCMSTajustada<-seas(PCMST)
qs(PCMSTajustada)
######Conclu?-se que h? componente sazonal nesta s?rie original################

######testando a sazonalidade na s?rie de incapacitados########################
PCISTajustada<-seas(PCIST)
qs(PCISTajustada)
######Conclu?-se que h? componente sazonal nesta s?rie original################

