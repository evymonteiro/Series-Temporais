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

autoplot(y,main="Grafico de Linha")
ggAcf(y) 
ggPacf(y)

dif_y=diff(y,lag=1,dif=1)
autoplot(dif_y,main="Grafico de Linha")
ggAcf(dif_y)
ggPacf(dif_y)

modelo <- Arima(y,order=c(1,0,0),include.constant=T)
#modelo <- Arima(y,order=c(0,1,1),include.constant=F)

coeftest(modelo)
residuos=residuals(modelo)
ggAcf(residuos,ci=0.99)
ggPacf(residuos,ci=0.99)

fits=fitted(modelo)
plot.ts(y,main="Grafico de Linha")
lines(fits,col=2,lty=2)

tsdisplay(residuos)
hist(residuos)
tsdiag(modelo,gof.lag=20)
#Shapiro-Wilk normality test
shapiro.test(residuos)

#Anderson-Darling normality test
ad.test(residuos)


sresiduos=scale(residuos)
qqnorm(sresiduos)
qqline(sresiduos, col = 2)

round(rbind(MSE=accuracy(modelo)[2],AIC=modelo$aic,BIC=modelo$aicc),3)

round(rbind(Média=mean(residuos),Sd=sd(residuos),skewness=skewness(residuos),kurtose=kurtosis(residuos),p_valor=shapiro.test(residuos)$p.value),3)


auto.arima(y)