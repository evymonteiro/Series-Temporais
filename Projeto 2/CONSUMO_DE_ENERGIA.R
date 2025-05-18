require(ggplot2)
require(forecast)
require(lmtest)
require(openxlsx)
require(nortest)
require(moments)
require(rstudioapi)

# Dados Simulados:

rm(list=ls(all=TRUE))
dados <- read.xlsx(rstudioapi::selectFile())
T=nrow(dados)
y=dados[,1]
p=1 #frequencia
y=ts(y,frequency=p)

autoplot(y)
ggAcf(y) #### Autocorrelação até lag 6. 
ggPacf(y) ###autocorrelação só a partir do lag 25.

modelo <- Arima(y,order=c(1,0,0),include.constant=F) #Arima (p,d,q) 
### p = autoregressivos, d = diferenciações, q = média móvel)
summary(modelo)
coeftest(modelo)
### Coeficientes dentro do intervalo [-1, 1]
residuos=residuals(modelo)
sresiduos=scale(residuos)

autoplot(residuos)
ggAcf(residuos,ci=0.99)
ggPacf(residuos,ci=0.99)

##Os resíduos não apresentaram autocorrelação a um nível de confiança de 99%. 

fits=fitted(modelo) ##Valores previstos.
autoplot(y) +autolayer(fits)

### O modelo se adequou bem a série. 

### Análise de Resíduos: 

hist(sresiduos, freq = FALSE)
qqnorm(sresiduos)
qqline(sresiduos, col = 2)

### Graficamente os resíduos apresentam normalidade. 
### Teste de Hipóteses: 
### H0: Os resíduos apresentam distribuição Normal vs
### H1: Os resíduos não apresentam distribuição Normal.

shapiro.test(sresiduos)
ad.test(sresiduos)

### Com 99% de confiança, não há evidências para rejeição de H0, portanto, 
### pode-se considerar que os resíduos seguem distribuição normal. 

### Testes de autocorrelação: 

tsdiag(modelo,gof.lag=16)

### O gráfico ACF e o de Ljung-Box não indicaram presença de autocorrelação nos 
### resíduos. 

Box.test(residuos,lag=5,type="Ljung")

## H0: Os resíduos não estão autocorrelacionados vs
## H1: Os resíduos estão correlacionados. 

###Não há evidências para rejeição de H0. 


# Estudo de Caso: Consumo de energia elétrica no período de 1963 a 2018

dados <- read.xlsx(rstudioapi::selectFile(),"energia")
T=nrow(dados)
print(dados)
y=dados[,2] #seleção de segunda coluna de energia

p=1 #frequencia uma observação por ano.
inicial=c(1963,1)
y=ts(y,frequency=p,start=inicial)

autoplot(y,main="Grafico de Linha") ###Aparente crescimento ao longo dos anos
ggAcf(y) ###autocorrelação até o lag 12, decaimento exponencial
ggPacf(y) ###sem autocorrelação parcial, primeiro lag significativo.
##Indica série AR(1)

dif_y=diff(y,lag=1,dif=1) ##primeira diferenciação para tornar a série estacionária
##Aqui os parâmetros são estabelecidos com lag = número de posições que os elementos
##são subtraídos, dif = quantidade de vezes que a série será diferenciada

autoplot(dif_y,main="Grafico de Linha")
##O gráfico de linha que antes aparentava um crescimento quase que exponencial
##agora indica crescimento mas tambem ciclos.

ggAcf(dif_y)
##Autocorrelação só até o lag 2.

ggPacf(dif_y)
###Autocorrelação a partir do lag 13, decaimento alternado.

#modelo <- Arima(y,order=c(1,0,0),include.constant=TRUE)
###Como a série não é estacionária, o modelo violou phi < 1. 

modelo <- Arima(y,order=c(0,1,1),include.constant=F)
###Sem constante = Sem tendência, 1 diferenciação (vista por dif_y) e 1 componente MA. 

coeftest(modelo)
###Componente MA(1) significativa.

residuos=residuals(modelo)
ggAcf(residuos,ci=0.99)
ggPacf(residuos,ci=0.99)

##Visualmente, o gráfico ACFP não indica correlação entre os resíduos, enquanto 
##o gráfico ACF indica no lag 2.

autoplot(y, series = "Observado") + 
  autolayer(fitted(modelo), series = "ARIMA", linetype = "dashed") +
  ggtitle("Série Observada vs. ARIMA") +
  xlab("Ano") + ylab("Consumo de Energia") +
  scale_color_manual(values = c("Observado" = "black", "ARIMA" = "red")) +
  theme_classic()

### O gráfico indica que o modelo ARIMA(0,1,1) conseguiu se adequar
### bem a série proposta. 


### Análise de Resíduos: 

tsdisplay(residuos)
###O gráfico ACF e ACFP indicam que os resíduos estão correlacionados. 

hist(residuos, freq = FALSE)
## Não apresenta normalidade de residuos.

tsdiag(modelo,gof.lag=20)

#Shapiro-Wilk normality test
shapiro.test(residuos)

###H0: Os resíduos possuem distribuição normal vs.
###H1: Os resíduos não possuem distribuição normal.
### Com p-value 1.353e-5, H0 é rejeitada. 

#Anderson-Darling normality test
ad.test(residuos)
###H0: Os resíduos possuem distribuição normal vs.
###H1: Os resíduos não possuem distribuição normal.
###Com p-value = 5.409e-7, H0 é rejeitada. 

sresiduos=scale(residuos)
qqnorm(sresiduos)
qqline(sresiduos, col = 2)

###Os resíduos não apresentam normalidade no gráfico Q-QPlot. 

round(rbind(MSE=accuracy(modelo)[2],AIC=modelo$aic,BIC=modelo$aicc),3)

round(rbind(Média=mean(residuos),Sd=sd(residuos),skewness=skewness(residuos),kurtose=kurtosis(residuos),p_valor=shapiro.test(residuos)$p.value),3)

##Proposta de novo modelo e utilizando a função auto.arima

modelo2 <- auto.arima(y)
summary(modelo2)
coeftest(modelo2)
###Componente MA e AR significativos. 

autoplot(y, series = "Observado") + 
  autolayer(fitted(modelo2), series = "ARIMA", linetype = "dashed") +
  ggtitle("Série Observada vs. ARIMA") +
  xlab("Ano") + ylab("Consumo de Energia") +
  scale_color_manual(values = c("Observado" = "black", "ARIMA" = "red")) +
  theme_classic()

#Análise de resíduos:

autoplot(residuals(modelo2),main="Grafico de Linha de Resíduos")

### Autocorrelação:
ggAcf(residuals(modelo2),ci=0.99)
ggPacf(residuals(modelo2),ci=0.99)
Box.test(residuals(modelo2), lag = 20, type = "Ljung-Box")

##H0: Não existe correlação significativa entre os resíduos vs.
##H1: Existe correlação significativa entre os resíduos.

##Os resíduos não estão autocorrelacionados, pois não existem evidências
##para rejeição de H0 (p-value = 0,12) e graficamente tambem não apresentou
##nenhum indicativo de correlação. 

### Normalidade

#Shapiro-Wilk normality test
shapiro.test(residuals(modelo2))

hist(residuals(modelo2))

###H0: Os resíduos possuem distribuição normal vs.
###H1: Os resíduos não possuem distribuição normal.
### Com p-value 1,13e-9, H0 é rejeitada, concluindo que os resíduos não são
### normais, porém como não possuem autocorrelação, podem ser considerados
### ruídos brancos. 

#ad.test(residuals(modelo2))

##A função auto.arima indicou um modelo com 1 componente regressora, 2 diferenciações
##e 1 componentede média móvel ARIMA(1,2,1)

### Comparação entre os modelos criados pela função arima e autoarima. 

###Pelo critério AIC, o modelo 1 = 984,919, enquanto o modelo2 = 945,52
### logo, o modelo 2 explica melhor o fenômeno. 

## Visualização do modelo 2: 

dif2_y=diff(y,lag=1,dif=2)

autoplot(dif2_y,main="Grafico de Linha")

### Possível identificar uma série sem "crescimento"
ggAcf(dif2_y)
##Primeiro lag significativo.

ggPacf(dif2_y)

round(rbind(MSE=accuracy(modelo2)[2],AIC=modelo2$aic,BIC=modelo2$aicc),3)

round(rbind(Média=mean(residuals(modelo2)),Sd=sd(residuals(modelo2)),skewness=skewness(residuals(modelo2)),kurtose=kurtosis(residuals(modelo2)),p_valor=shapiro.test(residuals(modelo2))$p.value),3)

