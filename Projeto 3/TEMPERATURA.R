require(forecast)
require(lmtest)
require(openxlsx)
require(nortest)
require(moments)
require(ggplot2)
require(rstudioapi)
library(zoo)
require(tseries)
require(gridExtra)

rm(list=ls(all=TRUE))

###Série de Temperatura Máxima da Região Sul, no período de janeiro de 1980 a 
###junho de 2014.

#Questão 1

dados <- read.xlsx(rstudioapi::selectFile(), sheet = 2)
dados$Mês_Ano <- as.Date(as.numeric(dados$Mês_Ano), origin = "1899-12-30")
dados$Mês_Ano <- format(dados$Mês_Ano, "%b-%y")
print(dados)

p=12 #frequencia
inicial=c(1980,1)


data=dados[,1]
y=dados[,2] ##temperatura max

y=ts(y,frequency=p,start=inicial)
autoplot(y,main="Temperatura Máxima da Região Sul")

### Série aparentemente estacionária. 

#a)

###Seleconando as 4 primeiras observações 

y1=window(y, end=c(1980,4)) #### Periodo  Amostral ####
autoplot(y1,main="Temperatura Máxima da Região Sul")


#b)

ggAcf(y)
##Picos nos lags 6, 12, 18, indicando sazonalidade, decaimento exponencial
##de forma senoidal, componente regressora AR. 
ggPacf(y)
##Decaimento exponencial com ondas senoides amortecidas, indicando MA
adf.test(y)

##Diferenciacao: 

dif_y=diff(y,lag=1,dif=1)
autoplot(dif_y,main="Grafico de Linha")
ggAcf(dif_y,lag=60)
##Manteve decaimento exponencial senoidal. 
ggPacf(dif_y,lag=60)
##picos em 6, 12, 18....

dif_y2=diff(dif_y,lag=12,dif=1)
autoplot(dif_y2,main="Grafico de Linha")

ggAcf(dif_y2,lag=60)
##Picos no lag 12, com queda apos o pico (AR = 0)
ggPacf(dif_y2,lag=60)
###Não ocorre decaimento exponencial, porém ocorre o decaimento nos picos (MA = 1)

###MODELOS###

M1="SARIMA(2,0,2)(1,0,0)[6]"
modelo1 <- Arima(y,order=c(2,0,2),seasonal=list(order=c(1,0,0),period=6))
summary(modelo1)
coeftest(modelo1)

M2="SARIMA(2,0,2)(1,0,0)[12]"
modelo2 <- Arima(y,order=c(2,0,2),seasonal=list(order=c(1,0,0),period=12))
summary(modelo2)
coeftest(modelo2)

M3="SARIMA(2,1,0)(1,0,0)[12]"
modelo3 <- Arima(y,order=c(2,1,0),seasonal=list(order=c(1,0,0),period=12))
summary(modelo3)
coeftest(modelo3)

M4="SARIMA(1,1,0)(1,0,0)[12]"
modelo4 <- Arima(y,order=c(1,1,0),seasonal=list(order=c(1,0,0),period=12))
summary(modelo4)
coeftest(modelo4)

M5="Auto.arima (4,0,1)(0,1,2)[12]"
modelo5<- auto.arima(y, seasonal=TRUE)
summary(modelo5)
coeftest(modelo5)

M6="SARIMA(2,0,2)(0,1,1)[12]"
modelo6 <- Arima(y,order=c(2,0,2),seasonal=list(order=c(1,1,1),period=12))
summary(modelo6)
coeftest(modelo6)

### Analise de Residuos #####

# Lista com todos os resíduos
residuos <- list(
  residuos1 = residuals(modelo1),
  residuos2 = residuals(modelo2),
  residuos3 = residuals(modelo3),
  residuos4 = residuals(modelo4),
  residuos5 = residuals(modelo5),
  residuos6 = residuals(modelo6)
)

# Loop para gerar gráficos para cada modelo
for (i in 1:length(residuos)) {
  resid <- residuos[[i]]
  nome_modelo <- names(residuos)[i]
    print(
    autoplot(resid) +
      ggtitle(paste("Série Temporal dos Resíduos -", nome_modelo)) +
      theme_minimal()
  )
    print(
    ggAcf(resid, lag.max = 60, ci = 0.99) +
      ggtitle(paste("ACF dos Resíduos -", nome_modelo)) +
      theme_minimal()
  )
  
  print(
    ggPacf(resid, lag.max = 60, ci = 0.99) +
      ggtitle(paste("PACF dos Resíduos -", nome_modelo)) +
      theme_minimal()
  )
}


## Lista de modelos para automatização de visualização de gráficos. 

modelos <- list(modelo1, modelo2, modelo3, modelo4, modelo5, modelo6)

plot_list <- lapply(1:length(modelos), function(i) {
  if (!is.null(modelos[[i]])) {
    autoplot(y) + 
      autolayer(fitted(modelos[[i]]), series = paste("Modelo", i)) +
      ggtitle(paste("Série vs Modelo", i)) +
      xlab("Ano") +
      ylab("Temperatura Max") +
      theme_classic() +
      theme(plot.title = element_text(size=10))
  }
})
x11()
grid.arrange(grobs = plot_list, nrow = 2, ncol = 3)

#Q-Q plots
par(mfrow = c(2, 3))

for (nome in names(residuos)) {
  qqnorm(residuos[[nome]], main = nome)
  qqline(residuos[[nome]], col = "red")
}

### Teste de Hipóteses: 
### H0: Os resíduos apresentam distribuição Normal vs
### H1: Os resíduos não apresentam distribuição Normal.

lapply(residuos, shapiro.test)
lapply(residuos, ad.test)


lapply(residuos, function (x) Box.test(x,lag=12,type="Ljung"))
## H0: Os resíduos não estão autocorrelacionados vs
## H1: Os resíduos estão correlacionados. 

# Estatísticas dos MODELOS
Est_Modelo1 = rbind(AICc=modelo1$aicc, AIC=modelo1$aic, BIC=modelo1$bic)
Est_Modelo2 = rbind(AICc=modelo2$aicc, AIC=modelo2$aic, BIC=modelo2$bic)
Est_Modelo3 = rbind(AICc=modelo3$aicc, AIC=modelo3$aic, BIC=modelo3$bic)
Est_Modelo4 = rbind(AICc=modelo4$aicc, AIC=modelo4$aic, BIC=modelo4$bic)
Est_Modelo5 = rbind(AICc=modelo5$aicc, AIC=modelo5$aic, BIC=modelo5$bic)
Est_Modelo6 = rbind(AICc=modelo6$aicc, AIC=modelo6$aic, BIC=modelo6$bic)

# Estatísticas dos RESÍDUOS
Est_residuo_M1 = rbind(Média=mean(residuals(modelo1)), 
                       Sd=sd(residuals(modelo1)),
                       skewness=skewness(residuals(modelo1)),
                       kurtose=kurtosis(residuals(modelo1)),
                       p_valor=shapiro.test(residuals(modelo1))$p.value)

Est_residuo_M2 = rbind(Média=mean(residuals(modelo2)), 
                       Sd=sd(residuals(modelo2)),
                       skewness=skewness(residuals(modelo2)),
                       kurtose=kurtosis(residuals(modelo2)),
                       p_valor=shapiro.test(residuals(modelo2))$p.value)

Est_residuo_M3 = rbind(Média=mean(residuals(modelo3)), 
                       Sd=sd(residuals(modelo3)),
                       skewness=skewness(residuals(modelo3)),
                       kurtose=kurtosis(residuals(modelo3)),
                       p_valor=shapiro.test(residuals(modelo3))$p.value)

Est_residuo_M4 = rbind(Média=mean(residuals(modelo4)), 
                       Sd=sd(residuals(modelo4)),
                       skewness=skewness(residuals(modelo4)),
                       kurtose=kurtosis(residuals(modelo4)),
                       p_valor=shapiro.test(residuals(modelo4))$p.value)

Est_residuo_M5 = rbind(Média=mean(residuals(modelo5)), 
                       Sd=sd(residuals(modelo5)),
                       skewness=skewness(residuals(modelo5)),
                       kurtose=kurtosis(residuals(modelo5)),
                       p_valor=shapiro.test(residuals(modelo5))$p.value)

Est_residuo_M6 = rbind(Média=mean(residuals(modelo6)), 
                       Sd=sd(residuals(modelo6)),
                       skewness=skewness(residuals(modelo6)),
                       kurtose=kurtosis(residuals(modelo6)),
                       p_valor=shapiro.test(residuals(modelo6))$p.value)

#c)

tabela1=cbind(Est_Modelo1,Est_Modelo2,Est_Modelo3,Est_Modelo4, Est_Modelo5, Est_Modelo6)
colnames(tabela1)=c(M1,M2,M3,M4,M5,M6)
tabela1

tabela2=cbind(Est_residuo_M1,Est_residuo_M2,Est_residuo_M3,Est_residuo_M4,Est_residuo_M5, Est_residuo_M6)
colnames(tabela2)=c(M1,M2,M3,M4,M5,M6)
tabela2

tabela3=rbind(tabela1,tabela2)
tabela3

write.xlsx(tabela3, 
           file = file.path(selectDirectory(), "Tabela_Est.xlsx"), 
           rowNames = TRUE)

#e)

nomes <- paste0("Modelo", 1:6)
for (i in seq_along(modelos)) {
  previsao <- forecast(modelos[[i]], h = 4, level = 95)
    print(
    autoplot(previsao) +
      autolayer(fitted(modelos[[i]]), linetype = "dashed", series = paste("Modelo", nomes[i])) +
      autolayer(y, series = "Dados Reais") +
      ggtitle(paste("Previsões com", nomes[i])) +
      theme_minimal()
  )
}
